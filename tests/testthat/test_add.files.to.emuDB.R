##' testthat tests for convert.TextGridCollection.to.emuDB
##'
##' @author Raphael Winkelmann
context("testing add.files.to.emuDB functions")

path2ae = system.file("extdata/emu/DBs/ae/", package = "emuR")

path2tgs = system.file("extdata/legacy_emu/DBs/ae/labels", package = "emuR")

# copy db to tempdir
file.copy(path2ae, tempdir(), recursive = T)

path2newDB = file.path(tempdir(),'ae')


##############################
test_that("files are copied and added correctly", {
  add.files.to.bundles(path2rootDir = path2tgs, path2sessionDir = file.path(path2newDB, '0000_ses'), fileExt = 'TextGrid')
  tgFilePaths = list.files(path2newDB, pattern = '*TextGrid$', recursive = T)
  
  expect_equal(length(tgFilePaths), 7)
  expect_equal(tgFilePaths[1], file.path('0000_ses', 'msajc003_bndl', 'msajc003.TextGrid'))
  
})


# clean up
unlink(path2newDB, recursive = T)
