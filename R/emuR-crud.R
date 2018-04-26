################################################################################
#                                                                              #
# This file contains public API functions for CRUD operations on Emu databases #
#                                                                              #
################################################################################


##' Create new items programmatically
##' @export
##' 
##' @description Allows to create annotation items programmatically. You have to
##' pass in either an \code{emuRsegs} object or a data frame describing the new
##' items. Each new item is identified by its \code{session}, \code{bundle},
##' \code{level}, and \code{sequence index}. The \code{level} with its associated
##' \code{attributes} determines how many labels must be provided. You must
##' provide a label for every existing attribute.
##' 
##' Sessions, bundles, levels and attributes must exist beforehand. The sequence
##' index is explained below.bla
##' 
##' Within each bundle, there can be multiple annotation items on every level.
##' Their order within the level is given by their sequence index. All *existing*
##' items have a natural-valued sequence index and there are no gaps in the
##' sequences (i.e. if a level contains n annotation items, they are indexed 1..n).
##' 
##' Any newly created item must be given a sequence index. The sequence index may
##' be real-valued (it will automatically be replaced with a natural value). To
##' prepend the new item to the existing one, pass a value lower than one. To
##' append it to the existing items, you can either pass \code{NULL} or any value
##' that you know is greater than n (the number of existing items in that level).
##' It does not need to be exactly n+1. If you are appending multiple items at the
##' same time, you should not choose \code{NULL}, because then the order of the
##' new items will not be guaranteed. To place the new item between two existing
##' ones, use any real value between the sequence indexes of the existing neighbors.
##' 
##' After creating the items, all sequence indexes (which may now be real-valued,
##' natural-valued or NULL) are sorted in ascending order and then replaced with
##' the values 1..n, where n is the number of items on that level. While sorting,
##' NULL values are placed at the end.
##' 
##' If you pass in an \code{emuRsegs} object, make sure \code{start_item_seq_idx}
##' and \code{end_item_seq_idx} are the same. This is not the case when you have
##' queried for sequences, e.g. as in
##' \code{query(emuDBhandle, "[Phonetic == i -> Phonetic == t]")}. Sequences of
##' this kind are currently unsupported in \code{create_itemsInLevel}.
##' 
##' At this time, it is not possible to insert items into time-based levels (SEGMENT or EVENT).
##'
##' @param emuDBhandle emuDB handle as returned by \code{\link{load_emuDB}}
##' @param itemsToCreate Either an emuRsegs object or a data frame with the
##' columns \code{session}, \code{bundle}, \code{level}, \code{sequenceIndex},
##' \code{attribute}, and \code{label}. *None* of the columns should be factors.
##' \code{sequenceIndex} must be numeric (can be real-valued or natural-valued),
##' all other columns must be of type character.
create_itemsInLevel = function(emuDBhandle,
                               itemsToCreate,
                               rewriteAllAnnots = TRUE,
                               verbose = TRUE) {
  UseMethod("create_itemsInLevel", itemsToCreate)
}

## A function called read_itemsInLevel will not exist - query() is the function that does the job.
# read_itemsInLevel = function (...)


## The currently unexported function change_labels will be renamed to update_itemsInLevel (and then be exported)
# update_itemsInLevel = function (...)


##' Delete new items programmatically
##' @export
##' 
##' @description Allows to delete annotation items programmatically.
delete_itemsInLevel = function (emuDBhandle,
                                itemsToDelete,
                                rewriteAllAnnots = TRUE,
                                verbose = TRUE) {
  UseMethod("delete_itemsInLevel", itemsToDelete)
}





############################################
### Wrappers that take emuRsegs as input ###
############################################

##' Transforms an emuRsegs object and calls the actual create function.
##' See \code{\link{create_itemsInLevel}}.
##' @export
create_itemsInLevel.emuRsegs = function (emuDBhandle,
                                         segmentList,
                                         rewriteAllAnnots = TRUE,
                                         verbose = TRUE) {
  itemsToCreate = as.crud.data.frame(emuDBhandle,
                                     segmentList)
  create_itemsInLevel(emuDBhandle,
                      itemsToCreate,
                      rewriteAllAnnots,
                      verbose)
}

##' Transforms an emuRsegs object and calls the actual delete function.
##' See \code{\link{delete_itemsInLevel}}.
##' @export
delete_itemsInLevel.emuRsegs = function (emuDBhandle,
                                         segmentList,
                                         rewriteAllAnnots = TRUE,
                                         verbose = TRUE) {
  itemsToDelete = as.crud.data.frame(emuDBhandle,
                                     segmentList)
  delete_itemsInLevel(emuDBhandle,
                      itemsToDelete,
                      rewriteAllAnnots,
                      verbose)
}





##############################
### Actual implementations ###
##############################


##' Actual implementation of the create function.
##' See \code{\link{create_itemsInLevel}}.
##' @export
create_itemsInLevel.data.frame = function (emuDBhandle,
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
  allLevelsAreItems = TRUE
  
  for (level in unique(itemsToCreate$level)) {
    levelDefinition = get_levelDefinition(emuDBhandle, level)
    if (levelDefinition$type != "ITEM") {
      allLevelsAreItems = FALSE
      warning ("One of the levels provided is not of type ITEM: ", level)
    }
  }
  
  if (allLevelsAreItems == FALSE) {
    stop("Some of the levels provided are not of type ITEM.")
  }
  
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
    dplyr::group_by_(~(session * bundle * level * sequenceIndex)) %>%
    dplyr::do_(insertItemIntoDatabase(emuDBhandle, ~(.)))

  ##
  ## Rewrite sequence indexes - fail if there are duplicate sequence indexes
  ##
  
  # ...
  
  if (rewriteAllAnnots) {
    rewrite_allAnnots(emuDBhandle, verbose)
  }
  
  invisible(NULL)
}


##' Actual implementation of the delete function.
##' See \code{\link{delete_itemsInLevel}}.
delete_itemsInLevel.data.frame = function (emuDBhandle,
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
