##' testthat tests for convert.TextGridCollection.to.emuDB
##'
##' @author Raphael Winkelmann
context("testing add.files.to.emuDB functions")

path2ae = system.file("extdata/emu/DBs/ae/", package = "emuR")

path2legacy_ae = system.file("extdata/legacy_emu/DBs/ae/", package = "emuR")

# copy db to tempdir
file.copy(path2ae, tempdir(), recursive = T)

path2newDB = file.path(tempdir(),'ae')


##############################
test_that("files are copied and added correctly", {
  ae = load.emuDB(path2newDB, verbose = F)
  add.files.to.emuDB(emuDB = ae, path2rootDir = path2legacy_ae, fileExt = 'fms')
  fmsFilePaths = list.files(path2newDB, pattern = '*.fms', recursive = T)
  
  expect_equal(length(fmsFilePaths), 7)
  expect_equal(fmsFilePaths[1], file.path('0000_ses', 'msajc003_bndl', 'msajc003.fms'))
  
})


# clean up
unlink(path2newDB, recursive = T)
