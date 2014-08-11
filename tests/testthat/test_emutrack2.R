##' testthat test for alternative output dir
##'
##' @author Raphael Winkelmann
context("test emu.track2 function")

path2db = system.file("extdata", package = "emuR")

# get segmentlist of type segment
path2segl <- list.files(system.file("extdata", package = "emuR"), pattern = glob2rx("ae-n.seg"), full.names = TRUE)
n = read.emusegs(path2segl)

# get segmentlist of type event
path2segl <- list.files(system.file("extdata", package = "emuR"), pattern = glob2rx("ae-hStar.seg"), full.names = TRUE)
hStar = read.emusegs(path2segl)

test_that("correct classes are returned", {
  
  td = emu.track2(n, 'fms:fm', path2db)
  expect_that(class(td), equals('trackdata'))
  
  td = emu.track2(n, 'fms:fm', path2db, cut=.5)
  expect_that(class(td), equals('data.frame'))

  td = emu.track2(n, 'fms:fm', path2db, cut=.5, npoints=3)
  expect_that(class(td), equals('trackdata'))
  
  td = emu.track2(n, 'fms:fm', path2db, cut=.5, npoints=1)
  expect_that(class(td), equals('data.frame'))

  td = emu.track2(hStar, 'fms:fm', path2db)
  expect_that(class(td), equals('data.frame'))

  td = emu.track2(hStar, 'fms:fm', path2db, npoints=3)
  expect_that(class(td), equals('trackdata'))
  
})

test_that("bad calls", {
  expect_error(emu.track2(n, 'fms:fm', path2db, npoints=3))
})


test_that("returned trackdata$data field has correct length", {
  td = emu.track2(n, 'fms:fm', path2db, cut=.5, npoints=3)
  expect_that(dim(td$data)[1], equals(length(n$utts)*3))

  td = emu.track2(n, 'fms:fm', path2db, cut=.5, npoints=5)
  expect_that(dim(td$data)[1], equals(length(n$utts)*5))

})