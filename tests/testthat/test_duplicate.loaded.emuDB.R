##' testthat tests for duplicate.loaded.emuDB
##'
##' @author Raphael Winkelmann
context("testing duplicate.loaded.emuDB function")

# load database 
if(!is.emuDB.loaded("ae")){
  path2extdata = system.file("extdata", package = "emuR")
  load_emuDB(paste(path2extdata, '/emu/DBs/ae/', sep = ''), verbose = F)
}

#############################
test_that("duplicate.loaded.emuDB handles bad calls", {
  fp = file.path(tempdir(), 'ae_copy')
  
  expect_error(duplicate.loaded.emuDB("ae", "ae", fp))
  expect_error(duplicate.loaded.emuDB("a", "ae", fp))
  
  # clean up
  if(is.emuDB.loaded("ae_copy")){
    UUID = get_emuDB_UUID(dbName = "ae_copy")
    .purge.emuDB(UUID)
  }
})


#############################
test_that("duplicate.loaded.emuDB works correctly", {
  fp = file.path(tempdir(), 'ae_copy')
  duplicate.loaded.emuDB("ae", "ae_copy", fp)
  
  emuDBtbl = dbReadTable(getEmuDBcon(), 'emuDB')
  expect_equal(length(emuDBtbl$uuid), 2)
  
  # todo check for equality of elements of duplicate
  
  # clean up
  if(is.emuDB.loaded("ae_copy")){
    UUID = get_emuDB_UUID(dbName = "ae_copy")
    .purge.emuDB(UUID)
  }
  
})
