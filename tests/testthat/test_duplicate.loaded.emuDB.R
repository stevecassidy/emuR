##' testthat tests for duplicate.loaded.emuDB
##'
##' @author Raphael Winkelmann
context("testing duplicate.loaded.emuDB function")

# load database 
if(!is.emuDB.loaded("ae")){
  path2extdata = system.file("extdata", package = "emuR")
  load.emuDB(paste(path2extdata, '/emu/DBs/ae/', sep = ''), verbose = F)
}

#############################
test_that("duplicate.loaded.emuDB handles bad calls", {
  if(is.emuDB.loaded("ae_copy")){
    UUID = get.emuDB.UUID(dbName = "ae_copy")
    .purge.emuDB(UUID)
  }
  fp = file.path(tempdir(), 'ae_copy')
  
  expect_error(duplicate.loaded.emuDB("ae", "ae", fp))
  expect_error(duplicate.loaded.emuDB("a", "ae", fp))
  
})


#############################
test_that("duplicate.loaded.emuDB works correctly", {
  if(is.emuDB.loaded("ae_copy")){
    UUID = get.emuDB.UUID(dbName = "ae_copy")
    .purge.emuDB(UUID)
  }
  fp = file.path(tempdir(), 'ae_copy')
  duplicate.loaded.emuDB("ae", "ae_copy", fp)
  
  emuDBtbl = dbReadTable(emuDBs.con, 'emuDB')
  expect_equal(length(emuDBtbl$uuid), 2)
  
  # todo check for equality of elements of duplicate
  
})
