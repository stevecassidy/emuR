##' testthat tests for convert.TextGridCollection.to.emuDB
##'
##' @author Raphael Winkelmann

require(RSQLite)

context("testing convert.TextGridCollection.to.emuDB function")


path2root = system.file("extdata/legacy_emu/DBs/ae/", package = "emuR")

path2tmpDir = tempdir()

path2newDb = file.path(path2tmpDir, 'convert_TextGridCollection_testDB')

emuDBname = 'convert_TextGridCollection_testDB'

unlink(path2newDb, recursive = T)

##############################
test_that("bad calls cause errors", {
  
  # create dir
  dir.create(path2newDb)
  
  # existing targetDir causes errors
  expect_error(convert.TextGridCollection.to.emuDB(path2rootDir = path2root, 
                                                   dbName = emuDBname,
                                                   path2tmpDir, showProgress=T))
  # clean up
  unlink(path2newDb, recursive = T)
  
})

##############################
test_that("correct emuDB is created", {
  
  convert.TextGridCollection.to.emuDB(path2rootDir = path2root, 
                                      dbName = emuDBname,
                                      path2tmpDir, showProgress=F)
  
  test_that("emuDB has correct file format on disc", {
    tmp = list.files(path2newDb)
    expect_equal(length(tmp), 2)
    
    tmp = list.files(file.path(path2newDb,'0000_ses'), recursive = T)
    expect_equal(length(tmp), 14)
  })

})
