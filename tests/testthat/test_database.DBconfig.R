##' testthat tests for database.DBconfig
##'
##' @author Raphael Winkelmann
context("testing database.DBconfig functions")

if(!is.emuDB.loaded("ae")){
  path2extdata = system.file("extdata", package = "emuR")
  load_emuDB(paste(path2extdata, '/emu/DBs/ae/', sep = ''), verbose = F)
}

##############################
test_that("get.levelDefinition returns correct levelDef", {
  
  #########################
  # get dbObj
  dbUUID = get_emuDB_UUID(dbName = "ae", dbUUID = NULL)
  dbObj = .load.emuDB.DBI(uuid = dbUUID)
  
  
  ld = get.levelDefinition(dbObj$DBconfig, 'Phonetic')
  expect_equal(ld$name, 'Phonetic')
  expect_equal(ld$type, 'SEGMENT')
  expect_equal(ld$attributeDefinitions[[1]]$name, 'Phonetic')
  expect_equal(ld$attributeDefinitions[[1]]$type, 'STRING')
})
