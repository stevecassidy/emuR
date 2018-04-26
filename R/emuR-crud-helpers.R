##' Transform a segment list of type emuRsegs into a data frame for CRUD operations
##' @export
##' 
##' @todo Is the name good?
##' 
##' @description A segment list (object of type emuRsegs) is often the basis for
##' CRUD operations. It must therefore be possible to pass them directly into
##' the various CRUD functions (\link{\code{create_itemsInLevel}},
##' \link{\code{update_itemsInLevel}}, \link{\code{delete_itemsInLevel}}).
##' 
##' By definition, segment lists are in fact segment sequence lists. Therefore,
##' they have the columns \code{start_item_seq_idx} and \code{end_item_seq_idx},
##' but nothing like \code{seq_idx}.
##' 
##' CRUD operations, however, operate on individual segments and not on segment
##' sequences.
##' 
##' Also, segment lists contain a column 'level' that actually contains an
##' attribute name rather than a level name.
##' 
##' @todo Maybe resolve sequences to all their members instead of reject them? Is this a problem with the labels?
as.crud.data.frame = function (emuDBhandle,
                               segmentList) {
  check_emuDBhandle(emuDBhandle, checkCache = F) # Do not check cache, we only need the DBconfig
  
  ##
  ## Check that the segment list contains no sequences
  ##
  if (!identical(segmentList$start_item_seq_idx, segmentList$end_item_seq_idx)) {
    stop("The segment list contains sequences. This is not yet supported.")
  }
  
  ##
  ## Resolve level names
  ##
  segmentList$attribute = segmentList$level
  
  for (rowname in rownames(segmentList)) {
    currentRow = segmentList[rowname,]
    segmentList$level = get_levelNameForAttributeName(emuDBhandle = emuDBhandle,
                                                             attributeName = currentRow$level)
  }
  
  crudDataFrame = data.frame(
    session = segmentList$session,
    bundle = segmentList$bundle,
    level = segmentList$level,
    sequenceIndex = segmentList$start_item_seq_idx,
    attribute = segmentList$attribute,
    label = segmentList$label,
    
    stringsAsFactors = F
  )
  
  return(crudDataFrame)
}


##' Insert one item into the database
##'
##' @description One item, identified as \code{session:bundle:level:sequenceIndex},
##' is inserted into the database. One label has to be provided for every attribute
##' of the given level.
##'
##' @param emuDBhandle emuDB handle as returned by \code{\link{load_emuDB}}
##' @param item Data frame containing the labels for the item to be inserted.
##'             Must contain the columns \code{session}, \code{bundle}, \code{level},
##'             \code{sequenceIndex}, \code{attribute}, \code{labelIndex}, and
##'             \code{label}. The first four of these identify the item and must
##'             contain the same value in all rows. \code{attribute} and \code{labelIndex}
##'             must match up - the label index marks the position of the
##'             attribute within its level (see \code{\link{get_labelIndex}}.
insertItemIntoDatabase = function(emuDBhandle,
                                  itemToInsert) {
  session = itemToInsert$session[1]
  bundle = itemToInsert$bundle[1]
  level = itemToInsert$level[1]
  sequenceIndex = itemToInsert$sequenceIndex[1]
  
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
  print (paste0("Inserting item ",
                paste(session, bundle, level, sequenceIndex, sep = ":")))
  
  itemId = 1 + bas_get_max_id(emuDBhandle,
                                     session,
                                     bundle)
  
  sampleRate = bas_get_samplerate(emuDBhandle,
                                         session,
                                         bundle)
  
  statement = DBI::dbSendStatement(
    emuDBhandle$connection,
    "INSERT INTO items (
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
  
  rowsAffected = DBI::dbGetRowsAffected(statement)
  DBI::dbClearResult(statement)
  print(paste("Inserted", rowsAffected, "items."))
  
  
  statement = DBI::dbSendStatement(
    emuDBhandle$connection,
    "INSERT INTO labels (
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
}

##' Vectorized function to translate level/attribute name pairs into label indexes.
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
rewrite_allSequenceIndexes = function (emuDBhandle) {
  allItems = DBI::dbReadTable(emuDBhandle$connection, "items")
  
  allItems %>%
    dplyr::group_by(~(db_uuid * session * bundle * level)) %>%
    dplyr::do(rewrite_sequenceIndexesOneLevel(~(.)))
}

##' See \code{\link{rewrite_allSequenceIndexes}}
rewrite_sequenceIndexesOneLevel = function (emuDBhandle,
                                            itemsOnLevel) {
  # Sort items by their current sequence_index
  itemsOnLevel = dplyr::arrange(itemsOnLevel, ~(seq_idx))
  
  # @todo handle NULL to be at the end of the sorting
  
  # Re-calculate the sequence index
  itemsOnLevel$newSequenceIndex = 1:nrow(itemsOnLevel)
  
  statement = DBI::dbSendStatement(
    emuDBhandle$connection,
    "UPDATE items SET seq_idx = ? WHERE db_uuid = ? AND session = ? AND bundle = ? AND item_id = ?"
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
  
  rowsAffected = DBI::dbGetRowsAffected(statement)
  DBI::dbClearResult(statement)
  
  print(paste("Updated", rowsAffected, "items."))
}
