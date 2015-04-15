##' testthat tests for get.trackdata
##'
##' @author Raphael Winkelmann
context("testing get.trackdata function")

# load database 
if(!is.emuDB.loaded("ae")){
  path2extdata = system.file("extdata", package = "emuR")
  load_emuDB(paste(path2extdata, '/emu/DBs/ae/', sep = ''), verbose = F)
}

# get segmentlist of type segment
path2segl <- list.files(system.file("extdata", package = "emuR"), pattern = glob2rx("ae-n.seg"), full.names = TRUE)
n = read.emusegs(path2segl)

# get segmentlist of type event
path2segl <- list.files(system.file("extdata", package = "emuR"), pattern = glob2rx("ae-hStar.seg"), full.names = TRUE)
hStar = read.emusegs(path2segl)

##############################
test_that("correct classes are returned", {
  
  td = get.trackdata("ae", n, 'fm', verbose=F)
  expect_that(class(td), equals('trackdata'))
  
  td = get.trackdata("ae", n, 'fm', cut=.5, verbose=F)
  expect_that(class(td), equals('data.frame'))
  
  td = get.trackdata("ae", n, 'fm', cut=.5, npoints=3, verbose=F)
  expect_that(class(td), equals('trackdata'))
  
  td = get.trackdata("ae", n, 'fm', cut=.5, npoints=1, verbose=F)
  expect_that(class(td), equals('data.frame'))
  
  td = get.trackdata("ae", hStar, 'fm', verbose=F)
  expect_that(class(td), equals('data.frame'))
  
  td = get.trackdata("ae", hStar, 'fm', npoints=3, verbose=F)
  expect_that(class(td), equals('trackdata'))
  
})

#############################
test_that("bad calls", {
  expect_error(get.trackdata("ae", verbose=F)) # seglist missing error
  expect_error(get.trackdata(seglist=n, verbose=F)) # S3 error
  expect_error(get.trackdata("ae", n, verbose=F)) # ssffTrackName missing error
  expect_error(get.trackdata("ae", n, cut=2, verbose=F)) # cut > 1 error
  expect_error(get.trackdata("ae", n, cut=-1, verbose=F)) # cut < 0 error
  expect_error(get.trackdata("ae", n, 'fm', npoints=3, verbose=F)) # npoint with no cut argument error
  expect_error(get.trackdata("ae", n, 'fm', onTheFlyParams = formals(open), verbose=F)) # no onTheFlyFunctionName error
  expect_error(get.trackdata(aeDB, n, 'fm', onTheFlyOptLogFilePath = '/path/to/bla/', verbose=F)) # onTheFlyOptLogFilePath error
})

##############################
test_that("returned trackdata$data field has correct length", {
  td = get.trackdata("ae", n, 'fm',cut=.5, npoints=3, verbose=F)
  expect_that(dim(td$data)[1], equals(length(n$utts)*3))
  
  td = get.trackdata("ae", n, 'fm',cut=.5, npoints=5, verbose=F)
  expect_that(dim(td$data)[1], equals(length(n$utts)*5))
  
})

##############################
test_that("all sorts of cut values work", {
  cutVals = seq(0, 1, 0.04)
  for(cutV in cutVals){
    td = get.trackdata("ae", n, 'fm',cut=cutV, verbose=F)
    expect_that(class(td), equals('data.frame'))
  }
})

##############################
test_that("on-the-fly calculations work", {
  for(wrasspFun in names(wrasspOutputInfos)){
    if(length(wrasspOutputInfos[[wrasspFun]]$tracks) > 0 && wrasspOutputInfos[[wrasspFun]]$outputType == "SSFF"){
      td = get.trackdata("ae", n, wrasspOutputInfos[[wrasspFun]]$tracks[1], onTheFlyFunctionName = wrasspFun, verbose=F)
    }
    expect_that(class(td), equals('trackdata'))
  }
})

##############################
test_that("data fields are the same as hardcoded values (taken from original emu.track(n, 'fm') command)", {
  td = get.trackdata("ae", n, 'fm', verbose=F)
  expect_that(td$data[10,1], equals(258))
  expect_that(td$data[10,2], equals(1504))
  expect_that(td$data[10,3], equals(2382))
  expect_that(td$data[10,4], equals(3569))
})


