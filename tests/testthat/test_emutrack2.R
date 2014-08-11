##' testthat test for alternative output dir
##'
##' @author Raphael Winkelmann
context("test emu.track2 function")

path2segl <- list.files(system.file("extdata", package = "emuR"), pattern = glob2rx("ae-n.seg"), full.names = TRUE)
path2db = system.file("extdata", package = "emuR")
  
n = read.emusegs(path2segl)

test_that("correct classes are returned", {
  
  td = emuR::emu.track2(n, 'fms:fm', path2db)
  expect_that(class(td), equals('trackdata'))
  
  td = emuR::emu.track2(n, 'fms:fm', path2db, cut=.5)
  expect_that(class(td), equals('data.frame'))

  td = emuR::emu.track2(n, 'fms:fm', path2db, cut=.5, npoints=3)
  expect_that(class(td), equals('trackdata'))
  
})

