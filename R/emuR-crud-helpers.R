##' Insert one item into the database
##'
##' @description One item, identified as \code{session:bundle:level:sequenceIndex},
##' is inserted into the database. One label has to be provided for every attribute
##' of the given level.
##'
##' @param emuDBhandle emuDB handle as returned by \code{\link{load_emuDB}}
##' @param itemToInsert Data frame containing the labels for the item to be inserted.
##'                     Must contain the columns \code{session}, \code{bundle}, \code{level},
##'                     \code{start_item_seq_idx}, \code{attribute}, \code{labelIndex}, and
##'                     \code{label}. The first four of these identify the item and must
##'                     contain the same value in all rows. \code{attribute} and \code{labelIndex}
##'                     must match up - the label index marks the position of the
##'                     attribute within its level (see \code{\link{get_labelIndex}}.
insertItemIntoDatabase = function(emuDBhandle,
                                  itemToInsert) {
  
  session = itemToInsert$session[1]
  bundle = itemToInsert$bundle[1]
  level = itemToInsert$level[1]
  sequenceIndex = itemToInsert$start_item_seq_idx[1]
  
  ##
  ## Make sure that the provided attributes exactly match those required for the provided level
  ##
  requiredAttributes = list_attributeDefinitions(emuDBhandle, level)$name
  
  if (!identical (sort(requiredAttributes), sort(itemToInsert$attribute))) {
    stop (call. = FALSE,
          paste0("Error in item ",
                 paste(session, bundle, level, sequenceIndex, sep = ":"),
                 ". ",
                 "The provided attributes (",
                 paste0(itemToInsert$attribute, collapse = ", "),
                 ") do not match the attributes required (",
                 paste0(requiredAttributes, collapse = ", "),
                 ") for the level (",
                 level,
                 ")."))
  }
  
  
  ##
  ## Insert item into the database (first the item itself, then the corresponding labels)
  ##
  itemId = 1 + bas_get_max_id(emuDBhandle,
                              session,
                              bundle)
  
  sampleRate = bas_get_samplerate(emuDBhandle,
                                  session,
                                  bundle)
  
  statement = DBI::dbSendStatement(
    emuDBhandle$connection,
    "INSERT INTO items_annot_crud_tmp (
    db_uuid, session, bundle, item_id, level, type, seq_idx, sample_rate,
    sample_point, sample_start, sample_dur
  )
    VALUES (
    ?, ?, ?, ?, ?, ?, ?, ?, NULL, NULL, NULL
    )"
  )
  
  DBI::dbBind(
    statement,
    list(
      emuDBhandle$UUID,
      session,
      bundle,
      itemId,
      level,
      "ITEM",
      sequenceIndex,
      sampleRate
    )
  )
  
  DBI::dbClearResult(statement)
  
  # now labels
  statement = DBI::dbSendStatement(
    emuDBhandle$connection,
    "INSERT INTO labels_annot_crud_tmp (
    db_uuid, session, bundle, item_id, label_idx, name, label
  )
    VALUES (
    ?, ?, ?, ?, ?, ?, ?
    )"
  )
  
  itemToInsert$dbUuid = emuDBhandle$UUID
  itemToInsert$itemId = itemId
  
  DBI::dbBind(
    statement,
    list(
      itemToInsert$dbUuid,
      itemToInsert$session,
      itemToInsert$bundle,
      itemToInsert$itemId,
      itemToInsert$labelIndex,
      itemToInsert$attribute,
      itemToInsert$label
    )
  )
  
  DBI::dbClearResult(statement)
  
  invisible(itemToInsert)
}

##' Vectorized function to translate level/attribute name pairs into label indexes.
##' 
##' @param emuDBhandle emuDB handle as returned by \code{\link{load_emuDB}}
##' @param levelName The level of the level/attribute pairs. This vector must
##' match the \code{attributeName} vector.
##' @param attributeName The attribute of the level/attribute pairs. This vector must
##' match the \code{levelName} vector.
get_labelIndex = function(emuDBhandle,
                          levelName,
                          attributeName) {
  ## @todo Should we export this function? I do not think so.
  # check_emuDBhandle(emuDBhandle, checkCache = T)
  
  allAttributes = data.frame (
    levelName = character(0),
    attributeName = character(0),
    index = numeric(0),
    
    stringsAsFactors = FALSE
  )
  
  requestedAttributes = data.frame(
    levelName = levelName,
    attributeName = attributeName,
    
    stringsAsFactors = FALSE
  )
  
  
  DBconfig = load_DBconfig(emuDBhandle)
  
  for (levelDefinition in DBconfig$levelDefinitions) {
    currentAttributeIndex = 0
    for (attributeDefinition in levelDefinition$attributeDefinitions) {
      currentAttributeIndex = currentAttributeIndex + 1
      allAttributes = rbind(
        allAttributes,
        data.frame (
          levelName = levelDefinition$name,
          attributeName = attributeDefinition$name,
          index = currentAttributeIndex,
          
          stringsAsFactors = FALSE
        )
      )
    }
  }
  
  result = dplyr::left_join(x = requestedAttributes,
                            y = allAttributes,
                            by = c("levelName", "attributeName"))
  
  return (result$index)
  
  ## Unit test
  ## get_labelIndex(db,
  ##                c("Word", "bundle", "Word", "foo", "bundle"),
  ##                c("Canonical", "transcription", "Word", "foo", "transcription")) == c(2,2,1, NA, 2)
}

##' Rewrite all sequence indexes across all sessions and bundles.
##' 
##' Reads the existing sequences of all items, assuming they are a mixture of
##' natural values, real values and NULL, sorts them in ascending order, and
##' replaces them with the sequence 1..n, where n is the number of items on the
##' respective level in the respective bundle. NULL values are placed at the end
##' of the sequence.
##' 
##' @param emuDBhandle emuDB handle as returned by \code{\link{load_emuDB}}
##' 
##' @importFrom rlang .data
rewrite_allSequenceIndexes = function (emuDBhandle) {
  allItems = DBI::dbReadTable(emuDBhandle$connection, "items_annot_crud_tmp")
  
  allItems %>%
    dplyr::group_by(.data$db_uuid, .data$session, .data$bundle, .data$level) %>%
    dplyr::do(rewrite_sequenceIndexesOneLevel(emuDBhandle, .data))
}

##' See \code{\link{rewrite_allSequenceIndexes}}
##' 
##' @param emuDBhandle emuDB handle as returned by \code{\link{load_emuDB}}
##' @param itemsOnLevel Data frame describing all items on a particular level.
##' 
##' @importFrom rlang .data
rewrite_sequenceIndexesOneLevel = function (emuDBhandle,
                                            itemsOnLevel) {
  # Sort items by their current sequence_index
  itemsOnLevel = dplyr::arrange(itemsOnLevel, .data$seq_idx)
  
  # @todo
  # dplyr::arrange handles NA values the way I want it to - they go at the end
  # of the list, no matter if sorting in ascending or descending order (we only
  # use ascending anyway). However, this does not seem to be documented. Should
  # We rely on it?
  
  # Re-calculate the sequence index
  itemsOnLevel$newSequenceIndex = 1:nrow(itemsOnLevel)
  
  statement = DBI::dbSendStatement(
    emuDBhandle$connection,
    "UPDATE items_annot_crud_tmp SET seq_idx = ? WHERE db_uuid = ? AND session = ? AND bundle = ? AND item_id = ?"
  )

  DBI::dbBind(
    statement,
    list(
      itemsOnLevel$newSequenceIndex,
      itemsOnLevel$db_uuid,
      itemsOnLevel$session,
      itemsOnLevel$bundle,
      itemsOnLevel$item_id
    )
  )
  
  DBI::dbClearResult(statement)
  
  invisible(itemsOnLevel)
}


ensureSequenceIndexesAreUnique = function (itemsOnAttribute) {
  
  uniqueSequenceIndexes = unique (itemsOnAttribute$start_item_seq_idx)
  
  if (length(uniqueSequenceIndexes) != length(itemsOnAttribute$start_item_seq_idx)) {
    stop(call. = FALSE,
         paste("Sequence indexes must be uniqe within one level.",
               "Found duplicate sequence indexes in",
               paste(itemsOnAttribute[1, "session"],
                     itemsOnAttribute[1, "bundle"],
                     itemsOnAttribute[1, "level"],
                     itemsOnAttribute[1, "attribute"],
                     sep = ":")))
  }
  
  invisible(itemsOnAttribute)
}

database.DDL.emuDB_items_annot_crud_tmp = 'CREATE TEMP TABLE items_annot_crud_tmp (
  db_uuid VARCHAR(36),
  session TEXT,
  bundle TEXT,
  item_id INTEGER,
  level TEXT,
  type TEXT,
  seq_idx FLOAT,
  sample_rate FLOAT,
  sample_point INTEGER,
  sample_start INTEGER,
  sample_dur INTEGER,
  PRIMARY KEY (db_uuid, session, bundle, item_id)
  -- FOREIGN KEY (db_uuid, session, bundle) REFERENCES bundle(db_uuid, session, name) ON DELETE CASCADE
);'

database.DDL.emuDB_labels_annot_crud_tmp = 'CREATE TEMP TABLE labels_annot_crud_tmp (
  db_uuid VARCHAR(36),
  session TEXT,
  bundle TEXT,
  item_id INTEGER,
  label_idx INTEGER,
  name TEXT,
  label TEXT,
  PRIMARY KEY (db_uuid, session, bundle, item_id, label_idx)
-- FOREIGN KEY (db_uuid, session, bundle) REFERENCES bundle(db_uuid, session, name) ON DELETE CASCADE
-- FOREIGN KEY (db_uuid, session, bundle, item_id) REFERENCES items(db_uuid, session, bundle, item_id) ON DELETE CASCADE
);'

create_annotCrudTmpTables = function(emuDBhandle) {
  DBI::dbExecute(emuDBhandle$connection, database.DDL.emuDB_items_annot_crud_tmp)
  DBI::dbExecute(emuDBhandle$connection, "INSERT INTO items_annot_crud_tmp SELECT * FROM items")
  DBI::dbExecute(emuDBhandle$connection, database.DDL.emuDB_labels_annot_crud_tmp)
  DBI::dbExecute(emuDBhandle$connection, "INSERT INTO labels_annot_crud_tmp SELECT * FROM labels")
}

remove_annotCrudTmpTables = function(emuDBhandle) {
  DBI::dbExecute(emuDBhandle$connection, "DROP TABLE IF EXISTS items_annot_crud_tmp")
  DBI::dbExecute(emuDBhandle$connection, "DROP TABLE IF EXISTS labels_annot_crud_tmp")
}


moveback_annotCrudTmpTables = function(emuDBhandle) {
  DBI::dbExecute(emuDBhandle$connection, "INSERT OR REPLACE INTO items SELECT * FROM items_annot_crud_tmp")
  DBI::dbExecute(emuDBhandle$connection, "INSERT OR REPLACE INTO labels SELECT * FROM labels_annot_crud_tmp")
  remove_annotCrudTmpTables(emuDBhandle)
}
