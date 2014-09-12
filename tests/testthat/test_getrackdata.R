##' testthat tests for get.trackdata
##'
##' @author Raphael Winkelmann
context("testing get.trackdata function")
require('emuSX') # SIC will not be necessary after merge

# load database 
path2extdata = system.file("extdata", package = "emuR")
aeDB = load.database(paste(path2extdata, '/emuDBs/ae/', sep = '')) # SIC! This function will be renamed shortly...

class(aeDB) <- 'emuDB' # SIC! override until new object class available after merge

# get segmentlist of type segment
path2segl <- list.files(system.file("extdata", package = "emuR"), pattern = glob2rx("ae-n.seg"), full.names = TRUE)
n = read.emusegs(path2segl)

# get segmentlist of type event
path2segl <- list.files(system.file("extdata", package = "emuR"), pattern = glob2rx("ae-hStar.seg"), full.names = TRUE)
hStar = read.emusegs(path2segl)

##############################
test_that("correct classes are returned", {
  
  td = get.trackdata(aeDB, n, 'fm', verbose=F)
  expect_that(class(td), equals('trackdata'))
  
  td = get.trackdata(aeDB, n, 'fm', cut=.5, verbose=F)
  expect_that(class(td), equals('data.frame'))
  
  td = get.trackdata(aeDB, n, 'fm', cut=.5, npoints=3, verbose=F)
  expect_that(class(td), equals('trackdata'))
  
  td = get.trackdata(aeDB, n, 'fm', cut=.5, npoints=1, verbose=F)
  expect_that(class(td), equals('data.frame'))
  
  td = get.trackdata(aeDB, hStar, 'fm', verbose=F)
  expect_that(class(td), equals('data.frame'))
  
  td = get.trackdata(aeDB, hStar, 'fm', npoints=3, verbose=F)
  expect_that(class(td), equals('trackdata'))
  
})
# 
# ##############################
# test_that("bad calls", {
#   expect_error(emu.track2(n, 'fms:fm', path2db, npoints=3, verbose=F))
#   expect_error(emu.track2(hStar, 'fms:fm', path2db, cut=.5, verbose=F))
# })
# 
# ##############################
# test_that("returned trackdata$data field has correct length", {
#   td = emu.track2(n, 'fms:fm', path2db, cut=.5, npoints=3, verbose=F)
#   expect_that(dim(td$data)[1], equals(length(n$utts)*3))
#   
#   td = emu.track2(n, 'fms:fm', path2db, cut=.5, npoints=5, verbose=F)
#   expect_that(dim(td$data)[1], equals(length(n$utts)*5))
#   
# })
# 
# ##############################
# test_that("all sorts of cut values work", {
#   cutVals = seq(0, 1, 0.04)
#   for(cutV in cutVals){
#     td = emu.track2(n, 'fms:fm', path2db, cut=cutV, verbose=F)
#     expect_that(class(td), equals('data.frame'))
#   }
# })
# 
# ##############################
# test_that("preexpanded seglist", {
#   nExp = getFiles(n, path2db, fileExt = '.fms', verbose = F)
#   td = emu.track2(nExp, 'fms:fm', path2db, verbose=F)
#   expect_that(class(td), equals('trackdata'))
# })
# 
# ##############################
# test_that("on the fly calculation", {
#   td = emu.track2(n, 'dft:dft', path2db, OnTheFlyFunctionName = 'dftSpectrum', verbose=F)
#   expect_that(class(td), equals('trackdata'))
#   td = emu.track2(n, 'pit:pitch', path2db, OnTheFlyFunctionName = 'mhsF0', verbose=F)
#   expect_that(class(td), equals('trackdata'))
# })