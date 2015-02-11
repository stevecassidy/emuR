##' testthat tests for database.DBconfig
##'
##' @author Raphael Winkelmann
context("testing database.DBconfig functions")

path2ae = system.file("extdata/emu/DBs/ae/", package = "emuR")

ae = load.emuDB(path2ae, verbose = F)

##############################
test_that("get.levelDefinition returns correct levelDef", {
  
  ld = get.levelDefinition(ae$DBconfig, 'Phonetic')
  expect_equal(ld$name, 'Phonetic')
  expect_equal(ld$type, 'SEGMENT')
  expect_equal(ld$attributeDefinitions[[1]]$name, 'Phonetic')
  expect_equal(ld$attributeDefinitions[[1]]$type, 'STRING')
})
