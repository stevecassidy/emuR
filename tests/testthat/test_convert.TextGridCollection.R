##' testthat tests for convert.TextGridCollection.to.emuDB
##'
##' @author Raphael Winkelmann
context("testing convert.TextGridCollection.to.emuDB function")

path2root = system.file("extdata/legacy_emu/DBs/", package = "emuR")

##############################
test_that("bad calls cause errors", {
  expect_error(convert.TextGridCollection.to.emuDB('alskfjaslödkfj'))
})

