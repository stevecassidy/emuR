##' testthat tests for convert.TextGridCollection.to.emuDB
##'
##' @author Raphael Winkelmann
context("testing convert.TextGridCollection.to.emuDB function")

path2root = system.file("extdata/legacy_emu/DBs/ae/", package = "emuR")

path2tmpDir = tempdir()

path2newDb = paste0(path2tmpDir, 'convert_TextGridCollection_testDB') 

unlink(path2newDb, recursive = T)

##############################
# test_that("bad calls cause errors", {
#   expect_error(convert.TextGridCollection.to.emuDB('alskfjaslödkfj'))
# })

##############################
test_that("bad calls cause errors", {
  convert.TextGridCollection.to.emuDB(path2rootDir = path2root, 
                                                   dbName = 'convert_TextGridCollection_testDB',
                                                   path2newDb, showProgress=F)
})

