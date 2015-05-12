##' testthat tests for convert.TextGridCollection.to.emuDB
##'
##' @author Raphael Winkelmann
context("testing add.files.to.emuDB functions")

path2extdata = system.file("extdata", package = "emuR")

if(!is.emuDB.loaded("ae")){
  load_emuDB(paste(path2extdata, '/emu/DBs/ae/', sep = ''), verbose = F) # SIC / hardcoded!!!!!!!!!!
}

tmpDbName = 'ae_copy'


#######################################
test_that("file operations work", {
  # pre clean (just in case)
  unlink(file.path(tempdir(),tmpDbName), recursive = TRUE)
  
  # copy ae and rename
  file.copy(file.path(path2extdata, '/emu/DBs/ae/'), tempdir(), recursive = T)
  file.rename(file.path(tempdir(), 'ae'), file.path(tempdir(), 'ae_copy'))
  
  # make copy of ae to mess with (caution correct DBconfig not stored)
  fp = file.path(tempdir(), tmpDbName)
  duplicate.loaded.emuDB("ae", tmpDbName, fp)
  
  test_that("import_mediaFiles works", {
    wavPath = system.file('extdata', package='wrassp')
    import_mediaFiles(tmpDbName, dir = wavPath, targetSessionName = 'newSes', verbose = F)
    expect_true(file.exists(file.path(fp, 'newSes_ses')))
    paths = list.files(file.path(fp, 'newSes_ses'), recursive = T, full.names = T, pattern = 'wav$')
    expect_equal(length(paths), 9)
    paths = list.files(file.path(fp, 'newSes_ses'), recursive = T, full.names = T, pattern = '_annot.json$')
    expect_equal(length(paths), 9)
  })
  
  test_that("CRUD operations for files work", {
    
    test_that("add = (C)RUD", {
      wrasspExtdataPath = system.file('extdata', package='wrassp')
      wavFilePaths = list.files(wrasspExtdataPath, pattern = "wav$", full.names = T, recursive = T)
      
      outDirPath = file.path(tempdir(), 'zcranaVals')
      dir.create(outDirPath)
      zcrana(wavFilePaths, outputDirectory = outDirPath)
      
      add_files(tmpDbName, dir = outDirPath, fileExtension = 'zcr', targetSessionName = 'newSes')
      zcrPaths = list.files(fp, pattern = 'zcr$', recursive = T)
      expect_equal(length(zcrPaths), 9)
      
    })

    test_that("list = C(R)UD", {
      df = list_files(tmpDbName)
      expect_equal(dim(df),c(55, 3))
    })
    
    
  })
  
  
  # clean up
  if(is.emuDB.loaded(tmpDbName)){
    UUID = get_emuDB_UUID(dbName = tmpDbName)
    purge_emuDB(dbName = tmpDbName, dbUUID = UUID, interactive = F)
  }

})
