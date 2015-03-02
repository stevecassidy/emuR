##' Initialize empty database tables 
##' 
##' Initialize empty database tables in the sql database 
##' specified by the conn argument. The tables are items, labels, links
##' 
##' @param conn sql DB connection
##' @param itemsTableName SQL table name of items table
##' @param labelTableName SQL table name of label table
##' @param linksTableName SQL table name of links table
##' @author Raphael Winkelmann
##'
initialize_database_tables <- function(conn, itemsTableName, labelTableName, linksTableName){
  
  # initialize empty tables (items, labels, links)
  items = data.frame(id=character(), session=character(), bundle=character(), level=character(),
                     itemID=integer(), type=character(), seqIdx=integer(), sampleRate=numeric(), 
                     samplePoint=integer(), sampleStart=integer(), sampleDur=integer(), stringsAsFactors=FALSE)
  
  dbWriteTable(conn, "emuR_emuDB_items_tmp", items)
  
  labels = data.frame(itemID=character(), session=character(), bundle=character(),
                      labelIdx=integer(), name=character(), label=character(), stringsAsFactors=FALSE)
  
  dbWriteTable(conn, "emuR_emuDB_labels_tmp", labels)
  
  links = data.frame(session=character(), bundle=character(), fromID=integer(),
                     toID=integer(), label=character(), stringsAsFactors=FALSE)
  
  dbWriteTable(conn, "emuR_emuDB_links_tmp", links)
  
}

##' Delete unwanted levels from database tables
##' 
##' Deletes all item, label and link rows that do not
##' belong to the levels specified in namesOfLevelsToKeep
##' 
##' @param conn sql DB connection
##' @param itemsTableName SQL table name of items table
##' @param labelTableName SQL table name of label table
##' @param linksTableName SQL table name of links table
##' @param namesOfLevelsToKeep character vector containing 
##' the level names that should remain in the tables
##' @author Raphael Winkelmann
##'
delete_unwanted_levels_from_database_tables <- function(conn, itemsTableName, labelsTableName, linksTableName, namesOfLevelsToKeep){
  
  condStr = paste0("level!='", paste0(namesOfLevelsToKeep, collapse = paste0("' AND ", " level!='")), "'")
  
  # delete items
  dbSendQuery(conn, paste0("DELETE FROM ", itemsTableName, " WHERE ", condStr))
  
  # delete labels
  dbSendQuery(conn, paste0("DELETE FROM ", labelsTableName, 
                           " WHERE itemID NOT IN (SELECT id FROM ", itemsTableName, ")"))
  
  # TODO (currently only used by convert.TextGridCollection.R -> DB has no links): 
  # delete links 
#   dbSendQuery(conn, paste0("DELETE FROM ", labelsTableName, 
#                            " WHERE itemID NOT IN (SELECT id FROM ", itemsTableName, ")"))
#   print(dbReadTable(conn, labelsTableName))
  
}

# FOR DEVELOPMENT
# library('testthat')
# test_file('tests/testthat/test_sqlDBactions.R')
