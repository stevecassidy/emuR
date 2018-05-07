################################################################################
#                                                                              #
# This file contains public API functions for CRUD operations on Emu databases #
#                                                                              #
################################################################################


##' Create new items programmatically
##' @export
##' 
##' @description Allows to create annotation items programmatically. You have to
##' pass in a data frame describing the new items. Each new item is identified by
##' its \code{session}, \code{bundle}, \code{level}, and \code{sequence index}.
##' The \code{level} with its associated \code{attributes} determines how many
##' labels must be provided. You must provide a label for every existing attribute.
##' 
##' Sessions, bundles, levels and attributes must exist beforehand. The sequence
##' index is explained below.
##' 
##' Within each bundle, there can be multiple annotation items on every level.
##' Their order within the level is given by their sequence index. All *existing*
##' items have a natural-valued sequence index and there are no gaps in the
##' sequences (i.e. if a level contains n annotation items, they are indexed 1..n).
##' 
##' Any newly created item must be given a sequence index. The sequence index may
##' be real-valued (it will automatically be replaced with a natural value). To
##' prepend the new item to the existing one, pass a value lower than one. To
##' append it to the existing items, you can either pass \code{NA} or any value
##' that you know is greater than n (the number of existing items in that level).
##' It does not need to be exactly n+1. To place the new item between two
##' existing ones, use any real value between the sequence indexes of the existing
##' neighbors.
##' 
##' If you are appending multiple items at the same time, every sequence index
##' (including NA) can only be used once per session/bundle/level combination
##' (because session/bundle/level/sequence index make the item's unique identifier).
##' 
##' After creating the items, all sequence indexes (which may now be real-valued,
##' natural-valued or NA) are sorted in ascending order and then replaced with
##' the values 1..n, where n is the number of items on that level. While sorting,
##' NA values are placed at the end.
##' 
##' At this time, it is not possible to insert items into time-based levels (SEGMENT or EVENT).
##'
##' @param emuDBhandle emuDB handle as returned by \code{\link{load_emuDB}}
##' @param itemsToCreate A data frame with the columns
##' \itemize{
##' \item\code{session},
##' \item\code{bundle},
##' \item\code{level},
##' \item\code{sequenceIndex},
##' \item\code{attribute}, and
##' \item \code{label}.
##' }
##' *None* of the columns should be factors.
##' \code{sequenceIndex} must be numeric (can be real-valued or natural-valued),
##' all other columns must be of type character.
##' @param rewriteAllAnnots should changes be written to file system (_annot.json
##'                         files) (intended for expert use only)
##' @param verbose if set to \code{TRUE}, more status messages are printed
##' 
##' @importFrom rlang .data
create_itemsInLevel = function(emuDBhandle,
                               itemsToCreate,
                               rewriteAllAnnots = TRUE,
                               verbose = TRUE) {
  ##
  ## First, check that every session/bundle combination exists
  ##
  bundleList = list_bundles(emuDBhandle)
  
  invalidItems = dplyr::anti_join (x = itemsToCreate,
                                   y = bundleList,
                                   by = c("session",
                                          "bundle" = "name"))
  
  if (nrow(invalidItems) != 0) {
    stop("Some of the session/bundle combinations provided are invalid: ", invalidItems)
  }
  
  ##
  ## Second, check that all levels are of type ITEM
  ##
  allLevelsAreOfTypeItem = TRUE
  
  for (level in unique(itemsToCreate$level)) {
    levelDefinition = get_levelDefinition(emuDBhandle, level)
    if (levelDefinition$type != "ITEM") {
      allLevelsAreOfTypeItem = FALSE
      warning ("One of the levels provided is not of type ITEM: ", level)
    }
  }
  
  if (allLevelsAreOfTypeItem == FALSE) {
    stop("Some of the levels provided are not of type ITEM.", call. = FALSE)
  }
  
  ##
  ## Make sure all sequence indexes are unique within their respective level/attribute pair.
  ##
  itemsToCreate %>%
    dplyr::group_by(.data$session, .data$bundle, .data$level, .data$attribute) %>%
    dplyr::do(ensureSequenceIndexesAreUnique(.data))
  
  ##
  ## Get the label index for each attribute (the label index marks the order of attributes within their level)
  ##
  itemsToCreate$labelIndex = get_labelIndex(emuDBhandle = emuDBhandle,
                                            levelName = itemsToCreate$level,
                                            attributeName = itemsToCreate$attribute)
  
  
  ##
  ## Split the item list into individual items (identified by the session/bundle/level/sequenceIndex tuple),
  ## and proceed separately for each of them
  ##
  itemsToCreate %>%
    dplyr::group_by(.data$session, .data$bundle, .data$level, .data$sequenceIndex) %>%
    dplyr::do(insertItemIntoDatabase(emuDBhandle, .data))
  
  
  ##
  ## Rewrite sequence indexes
  ##
  rewrite_allSequenceIndexes(emuDBhandle)
  ## @todo fail if the user is trying to add sequence indexes that are already
  ## there - or accept it silently, producing undefined order?
  
  if (rewriteAllAnnots) {
    rewrite_allAnnots(emuDBhandle, verbose)
  }
  
  invisible(NULL)
}

## A function called read_itemsInLevel will not exist - query() is the function that does the job.
# read_itemsInLevel = function (...)


##' Change item labels programmatically
##' @export
##' 
##' @param emuDBhandle emuDB handle as returned by \code{\link{load_emuDB}}
##' @param itemsToUpdate A data frame with the columns
##' \itemize{
##' \item\code{session},
##' \item\code{bundle},
##' \item\code{level},
##' \item\code{sequenceIndex},
##' \item\code{attribute}, and
##' \item \code{label}.
##' }
##' *None* of the columns should be factors.
##' \code{sequenceIndex} must be numeric (natural-valued), all other columns
##' must be of type character.
##' @param rewriteAllAnnots should changes be written to file system (_annot.json
##'                         files) (intended for expert use only)
##' @param verbose if set to \code{TRUE}, more status messages are printed
##' 
##' @importFrom rlang .data
update_itemsInLevel = function (emuDBhandle,
                                itemsToUpdate,
                                rewriteAllAnnots = TRUE,
                                verbose = TRUE) {
  ##
  ## Find the index of each attribute definition on its respective level
  ##
  itemsToUpdate$labelIndex = get_labelIndex(emuDBhandle = emuDBhandle,
                                            levelName = itemsToUpdate$level,
                                            attributeName = itemsToUpdate$attribute)
  

  ##
  ## First thing, make sure all the items whose labels are gonna be changed do exist
  ##
  statement = DBI::dbSendStatement(
    emuDBhandle$connection,
    "SELECT count(*) FROM items
    WHERE
    db_uuid = ? AND
    session = ? AND
    bundle = ? AND
    level = ? AND
    seq_idx = ?"
  )
  DBI::dbBind(
    statement,
    list(
      rep(emuDBhandle$UUID, nrow(itemsToUpdate)),
      itemsToUpdate$session,
      itemsToUpdate$bundle,
      itemsToUpdate$level,
      itemsToUpdate$sequenceIndex
    )
  )
  
  existenceMatrix = DBI::dbFetch(statement)
  DBI::dbClearResult(statement)
  
  if (!all(existenceMatrix)) {
    total = nrow(itemsToUpdate)
    notFound = length(existenceMatrix[existenceMatrix == 0, ])
    
    warning (paste(
      "Error:",
      notFound,
      "of the",
      total,
      "specified items do/does not exist"
    ))
    
    if (verbose) {
      warning(itemsToUpdate[existenceMatrix == 0, ])
    } else {
      warning ("Set verbose to TRUE to see them listed.")
    }
    
    return (invisible(NULL))
  }
  
  statement = DBI::dbSendStatement(
    emuDBhandle$connection,
    "SELECT item_id FROM items
    WHERE
    db_uuid = ? AND
    session = ? AND
    bundle = ? AND
    level = ? AND
    seq_idx = ?"
  )
  
  DBI::dbBind(
    statement,
    list(
      rep(emuDBhandle$UUID, nrow(itemsToUpdate)),
      itemsToUpdate$session,
      itemsToUpdate$bundle,
      itemsToUpate$level,
      itemsToUpdate$sequenceIndex
    )
  )
  
  item_id_list = DBI::dbFetch(statement)
  DBI::dbClearResult(statement)
  
  statement = DBI::dbSendStatement(
    emuDBhandle$connection,
    "INSERT OR REPLACE INTO labels
    (db_uuid, session, bundle, item_id, label_idx, name, label)
    VALUES (?, ?, ?, ?, ?, ?, ?)"
  )
  
  DBI::dbBind(
    statement,
    list(
      rep(emuDBhandle$UUID, nrow(itemsToUpdate)),
      itemsToUpdate$session,
      itemsToUpdate$bundle,
      item_id_list$item_id,
      itemsToUpdate$labelIndex,
      itemsToUpdate$attribute,
      itemsToUpdate$label
    )
  )
  
  rowsAffected = DBI::dbGetRowsAffected(statement)
  DBI::dbClearResult(statement)
  
  print(paste("Updated", rowsAffected, "labels."))
  
  if (rewriteAllAnnots) {
    rewrite_allAnnots(emuDBhandle, verbose)
  }
  
  invisible(NULL)
}



##' Delete new items programmatically
##' @export
##' 
##' @description Allows to delete annotation items programmatically.
##' 
##' @param emuDBhandle emuDB handle as returned by \code{\link{load_emuDB}}
##' @param itemsToDelete A data frame with the columns
##' \itemize{
##' \item\code{session},
##' \item\code{bundle},
##' \item\code{level}, and
##' \item\code{sequenceIndex}.
##' }
##' *None* of the columns should be factors.
##' \code{sequenceIndex} must be numeric (natural-valued), all other columns must
##' be of type character.
##' @param rewriteAllAnnots should changes be written to file system (_annot.json
##'                         files) (intended for expert use only)
##' @param verbose if set to \code{TRUE}, more status messages are printed
delete_itemsInLevel = function (emuDBhandle,
                                itemsToDelete,
                                rewriteAllAnnots = TRUE,
                                verbose = TRUE) {
  print ("[Dummy] Deleting items")
  print(itemsToDelete)
  
  ##
  ## ...
  ##
  
  if (rewriteAllAnnots) {
    rewrite_allAnnots(emuDBhandle, verbose)
  }
  
  invisible(NULL)
}
