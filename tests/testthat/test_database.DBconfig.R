##' testthat tests for database.DBconfig
##'
##' @author Raphael Winkelmann
context("testing database.DBconfig functions")

path2extdata = system.file("extdata", package = "emuR")

if(!is.emuDB.loaded("ae")){
  load_emuDB(paste(path2extdata, '/emu/DBs/ae/', sep = ''), verbose = F) # SIC / hardcoded!!!!!!!!!!
}

tmpDbName = 'ae_copy'

##############################
test_that("get.levelDefinition returns correct levelDef", {
  
  #########################
  # get dbObj
  dbUUID = get_emuDB_UUID(dbName = "ae", dbUUID = NULL)
  dbObj = .load.emuDB.DBI(uuid = dbUUID)
  
  
  ld = get.levelDefinition(dbObj$DBconfig, 'Phonetic')
  expect_equal(ld$name, 'Phonetic')
  expect_equal(ld$type, 'SEGMENT')
  expect_equal(ld$attributeDefinitions[[1]]$name, 'Phonetic')
  expect_equal(ld$attributeDefinitions[[1]]$type, 'STRING')
})

##############################
test_that("CRUD operations work for ssffTrackDefinitions", {
  # pre clean (just in case)
  unlink(file.path(tempdir(),tmpDbName), recursive = TRUE)
  
  # copy ae and rename
  file.copy(file.path(path2extdata, '/emu/DBs/ae/'), tempdir(), recursive = T)
  file.rename(file.path(tempdir(), 'ae'), file.path(tempdir(), 'ae_copy'))
  
  # make copy of ae to mess with (caution correct DBconfig not stored)
  fp = file.path(tempdir(), tmpDbName)
  duplicate.loaded.emuDB("ae", tmpDbName, fp)
  
  test_that("add = (C)RUD", {
    expect_error(add_ssffTrackDefinitions(dbName=tmpDbName, 'fm'))
    expect_error(add_ssffTrackDefinitions(dbName=tmpDbName, 'fm', 'bla'))
    expect_error(add_ssffTrackDefinitions(dbName=tmpDbName, 'newTrackName', 'badColName', 'pit', 
                             onTheFlyFunctionName = 'mhsF0', interactive = T))
    
    add_ssffTrackDefinitions(dbName=tmpDbName, 'newTrackName', 'pitch', 'pit', 
                             onTheFlyFunctionName = 'mhsF0', interactive = F)
    
    pitFilePaths = list.files(fp, pattern = 'pit$', recursive = T)
    expect_equal(length(pitFilePaths), 7)
    
  })
  
  test_that("list = C(R)UD", {
    df = list_ssffTrackDefinitions(dbName=tmpDbName)
    expect_equal(df$name, c('dft','fm', 'newTrackName'))
    expect_equal(df$columnName, c('dft','fm', 'pitch'))
    expect_equal(df$fileExtension, c('dft','fms', 'pit'))
  })

  test_that("modify = CR(U)D", {
    # bad name causes errors
    expect_error(modify_ssffTrackDefinitions(dbName=tmpDbName, name="asdf"))
    
    # check if renaming works
    modify_ssffTrackDefinitions(dbName=tmpDbName, name="fm", newName="fmNewName")
    uuid=get_emuDB_UUID(tmpDbName, NULL)
    dbObj = .load.emuDB.DBI(uuid = uuid)
    expect_equal(dbObj$DBconfig$ssffTrackDefinitions[[2]]$name, "fmNewName")
    expect_equal(dbObj$DBconfig$ssffTrackDefinitions[[2]]$columnName, "fm")
    expect_equal(dbObj$DBconfig$ssffTrackDefinitions[[2]]$fileExtension, "fms")
    # check if modifying everything works
    modify_ssffTrackDefinitions(dbName=tmpDbName, name="fmNewName", newName="fmNewName2", 
                                newColumnName = "test12", newFileExtension = "test12")
    uuid=get_emuDB_UUID(tmpDbName, NULL)
    dbObj = .load.emuDB.DBI(uuid = uuid)
    expect_equal(dbObj$DBconfig$ssffTrackDefinitions[[2]]$name, "fmNewName2")
    expect_equal(dbObj$DBconfig$ssffTrackDefinitions[[2]]$columnName, "test12")
    expect_equal(dbObj$DBconfig$ssffTrackDefinitions[[2]]$fileExtension, "test12")
    # revert changes
    modify_ssffTrackDefinitions(dbName=tmpDbName, name="fmNewName2", newName="fm", 
                                newColumnName = "fm", newFileExtension = "fms")    
    
  })
  
  test_that("remove = CRU(D)", {
    # bad name
    expect_error(remove_ssffTrackDefinitions(dbName=tmpDbName, name="asdf"))
    remove_ssffTrackDefinitions(dbName=tmpDbName, name="newTrackName", deleteFiles = T)
    # check that _DBconfig entry is deleted
    uuid=get_emuDB_UUID(tmpDbName, NULL)
    dbObj = .load.emuDB.DBI(uuid = uuid)
    expect_equal(dbObj$DBconfig$ssffTrackDefinitions[[1]]$name, "dft")
    expect_equal(dbObj$DBconfig$ssffTrackDefinitions[[2]]$name, "fm")
    
    # check that files have been deleted
    filePaths = list_bundleFilePaths(dbName=tmpDbName, "pit", dbUUID = NULL)
    expect_equal(length(filePaths), 0)
    
  })
  
  # clean up
  if(is.emuDB.loaded(tmpDbName)){
    UUID = get_emuDB_UUID(dbName = tmpDbName)
    purge_emuDB(dbName = tmpDbName, dbUUID = UUID, interactive = F)
  }
})
  