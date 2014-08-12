##' testthat tests for getFiles.emusegs
##'
##' @author Raphael Winkelmann
context("testing getFiles.emusegs function")

path2db = system.file("extdata", package = "emuR")

# get segmentlist of type segment
path2segl <- list.files(system.file("extdata", package = "emuR"), pattern = glob2rx("ae-n.seg"), full.names = TRUE)
n = read.emusegs(path2segl)

##############################
test_that("bad calls", {
  expect_error(getFiles(n, path2db, fileExt = 'fms', verbose = F))
  expect_error(getFiles(n, 'aaaaaaaaaaaaaaa', fileExt = 'fms', verbose = F))
})
