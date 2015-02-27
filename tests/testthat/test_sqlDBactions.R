##' testthat tests for delete_unwanted_levels_from_database_tables
##'
##' @author Raphael Winkelmann

require(RSQLite)

context("testing delete_unwanted_levels_from_database_tables function")


path2ae = system.file("extdata/emu/DBs/ae/", package = "emuR")

ae = load.emuDB(path2ae, verbose = F)

##############################
test_that("test delete_unwanted_levels_from_database_tables", {
  
  # Create an ephemeral in-memory RSQLite database
  itemsTableName = "emuR_emuDB_items_tmp"
  labelsTableName ="emuR_emuDB_labels_tmp"
  linksTableName = "emuR_emuDB_links_tmp"
  
  con <- dbConnect(RSQLite::SQLite(), ":memory:")
  
  initialize_database_tables(con, itemsTableName, labelsTableName, linksTableName)
  
  
  dbWriteTable(con, itemsTableName, ae$items, overwrite=T)
  dbWriteTable(con, labelsTableName, ae$labels, overwrite=T)
  dbWriteTable(con, linksTableName, ae$links, overwrite=T)
  
  
  delete_unwanted_levels_from_database_tables(con, itemsTableName, labelTableName, linksTableName, c("Phonetic", "Tone"))
  
  # check only Phonetic + Tone items left
  res = dbSendQuery(con, paste0("SELECT DISTINCT level FROM ", itemsTableName))
  levelNames = dbFetch(res)
  dbClearResult(res)
  
  expect_equal(levelNames[1,], "Phonetic")
  expect_equal(levelNames[2,], "Tone")

  #TODO more for correct labels/links
    
  # clean up
  dbRemoveTable(con, itemsTableName)
  dbRemoveTable(con, labelsTableName)
  dbRemoveTable(con, linksTableName)
  dbDisconnect(con)
  
})

