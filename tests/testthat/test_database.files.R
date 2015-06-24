##' testthat tests for convert.TextGridCollection.to.emuDB
##'
##' @author Raphael Winkelmann
context("testing database.files functions")



#######################################
test_that("file operations work", {
  
  dbName = 'ae'
  
  path2orig = file.path(tempdir(), "emuR_demoData", dbName)
  path2testData = file.path(tempdir(), "emuR_testthat")
  path2db = file.path(path2testData, dbName)
  
  # purge, delete, copy and load
  if(is.emuDB.loaded(dbName)){
    purge_emuDB(dbName, interactive = F)
  }
  unlink(path2db, recursive = T)
  file.copy(path2orig, path2testData, recursive = T)
  
  load_emuDB(path2db, inMemoryCache = internalVars$testingVars$inMemoryCache, verbose = F)
  
  
  test_that("import_mediaFiles works", {
    wavPath = system.file('extdata', package='wrassp')
    import_mediaFiles(dbName, dir = wavPath, targetSessionName = 'newSes', verbose = F)
    expect_true(file.exists(file.path(path2db, 'newSes_ses')))
    paths = list.files(file.path(path2db, 'newSes_ses'), recursive = T, full.names = T, pattern = 'wav$')
    expect_equal(length(paths), 9)
    paths = list.files(file.path(path2db, 'newSes_ses'), recursive = T, full.names = T, pattern = '_annot.json$')
    expect_equal(length(paths), 9)
  })
  
  test_that("CRUD operations for files work", {
    
    test_that("add = (C)RUD", {
      wrasspExtdataPath = system.file('extdata', package='wrassp')
      wavFilePaths = list.files(wrasspExtdataPath, pattern = "wav$", full.names = T, recursive = T)
      
      outDirPath = file.path(path2testData, 'zcranaVals')
      dir.create(outDirPath)
      zcrana(wavFilePaths, outputDirectory = outDirPath)
      
      add_files(dbName, dir = outDirPath, fileExtension = 'zcr', targetSessionName = 'newSes')
      zcrPaths = list.files(path2db, pattern = 'zcr$', recursive = T)
      expect_equal(length(zcrPaths), 9)
      
    })
    
    test_that("list = C(R)UD", {
      df = list_files(dbName)
      expect_equal(dim(df),c(55, 3))
    })
    
    
  })
  
  
  # clean up
  if(is.emuDB.loaded(dbName)){
    UUID = get_emuDB_UUID(dbName = dbName)
    purge_emuDB(dbName = dbName, dbUUID = UUID, interactive = F)
  }
  
})
