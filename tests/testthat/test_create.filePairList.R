##' testthat tests for convert.TextGridCollection.to.emuDB
##'
##' @author Raphael Winkelmann
context("testing create.filePairList function")

path2root = system.file("extdata/legacy_emu/DBs/ae/", package = "emuR")

ext1 = 'wav'
ext2 = 'TextGrid'

wavPaths = list.files(path2root, pattern=paste(ext1, "$", sep = ""), recursive=T, full.names=T)
tgPaths = list.files(path2root, pattern=paste(ext2, "$", sep = ""), recursive=T, full.names=T)

testDirName = 'test_createFilePairList'

path2testDir = file.path(tempdir(), testDirName)

##############################
test_that("bad calls cause errors", {
  
  expect_error(create.filePairList('asdf', '', '', ''), 'ext1Path2rootDir does not exist!')
  expect_error(create.filePairList(path2root, 'asdf', '', ''), 'ext2Path2rootDir does not exist!')
  
})

##############################
test_that("error is generated when nr of ext1 files > ext2 files", {
  # create testdir
  dir.create(path2testDir)
  
  # copy files 
  file.copy(wavPaths, path2testDir)
  file.copy(tgPaths[-length(tgPaths)], path2testDir)
  
  expect_error(create.filePairList(path2testDir, path2testDir, 'wav', 'TextGrid'))
  
  # clean up
  unlink(path2testDir, recursive = T)
  
})


##############################
test_that("correct filePairList is generated when nr of ext1 files < ext2 files", {
  # create testdir
  dir.create(path2testDir)
  
  # copy files 
  file.copy(wavPaths[-length(wavPaths)], path2testDir)
  file.copy(tgPaths, path2testDir)
  
  fpl = create.filePairList(path2testDir, path2testDir, 'wav', 'TextGrid')
  
  expect_equal(dim(fpl)[1], 6)
  expect_equal(dim(fpl)[2], 2)
  
  # clean up
  unlink(path2testDir, recursive = T)
  
})

##############################
test_that("error is thrown if dirs are empty", {
  
  # create testdir
  dir.create(path2testDir)
  
  expect_error(create.filePairList(path2testDir, path2testDir, 'wav', 'TextGrid'))
    
  # clean up
  unlink(path2testDir, recursive = T)
  
})

##############################
test_that("error is thrown if one ext2 does not have same base name", {
  
  # create testdir
  dir.create(path2testDir)
  
  # copy files 
  file.copy(wavPaths, path2testDir)
  file.copy(tgPaths, path2testDir)
  
  #rename file
  file.rename(file.path(path2testDir, basename(tgPaths[3])), file.path(path2testDir, 'asdf.TextGrid'))
  
  expect_error(create.filePairList(path2testDir, path2testDir, 'wav', 'TextGrid'))
  
  # clean up
  unlink(path2testDir, recursive = T)
  
})

