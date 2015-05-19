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
    expect_error(add_ssffTrackDefinition(dbName=tmpDbName, 'fm'))
    expect_error(add_ssffTrackDefinition(dbName=tmpDbName, 'fm', 'bla'))
    expect_error(add_ssffTrackDefinition(dbName=tmpDbName, 'newTrackName', 'badColName', 'pit', 
                                         onTheFlyFunctionName = 'mhsF0', interactive = T))
    
    add_ssffTrackDefinition(dbName=tmpDbName, 'newTrackName', 'pitch', 'pit', 
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
    expect_error(modify_ssffTrackDefinition(dbName=tmpDbName, name="asdf"))
    
    # check if renaming works
    modify_ssffTrackDefinition(dbName=tmpDbName, name="fm", newName="fmNewName")
    uuid=get_emuDB_UUID(tmpDbName, NULL)
    dbObj = .load.emuDB.DBI(uuid = uuid)
    expect_equal(dbObj$DBconfig$ssffTrackDefinitions[[2]]$name, "fmNewName")
    expect_equal(dbObj$DBconfig$ssffTrackDefinitions[[2]]$columnName, "fm")
    expect_equal(dbObj$DBconfig$ssffTrackDefinitions[[2]]$fileExtension, "fms")
    # check if modifying everything works
    modify_ssffTrackDefinition(dbName=tmpDbName, name="fmNewName", newName="fmNewName2", 
                               newColumnName = "test12", newFileExtension = "test12")
    uuid=get_emuDB_UUID(tmpDbName, NULL)
    dbObj = .load.emuDB.DBI(uuid = uuid)
    expect_equal(dbObj$DBconfig$ssffTrackDefinitions[[2]]$name, "fmNewName2")
    expect_equal(dbObj$DBconfig$ssffTrackDefinitions[[2]]$columnName, "test12")
    expect_equal(dbObj$DBconfig$ssffTrackDefinitions[[2]]$fileExtension, "test12")
    # revert changes
    modify_ssffTrackDefinition(dbName=tmpDbName, name="fmNewName2", newName="fm", 
                               newColumnName = "fm", newFileExtension = "fms")    
    
  })
  
  test_that("remove = CRU(D)", {
    # bad name
    expect_error(remove_ssffTrackDefinition(dbName=tmpDbName, name="asdf"))
    remove_ssffTrackDefinition(dbName=tmpDbName, name="newTrackName", deleteFiles = T)
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

##############################
test_that("CRUD operations work for levelDefinitions", {
  # pre clean (just in case)
  unlink(file.path(tempdir(),tmpDbName), recursive = TRUE)
  
  # copy ae and rename
  file.copy(file.path(path2extdata, '/emu/DBs/ae/'), tempdir(), recursive = T)
  file.rename(file.path(tempdir(), 'ae'), file.path(tempdir(), 'ae_copy'))
  
  # make copy of ae to mess with (caution correct DBconfig not stored)
  fp = file.path(tempdir(), tmpDbName)
  duplicate.loaded.emuDB("ae", tmpDbName, fp)
  
  test_that("add = (C)RUD", {
    expect_error(add_levelDefinition(dbName=tmpDbName, 'Phonetic', 'SEGM')) # bad type
    expect_error(add_levelDefinition(dbName=tmpDbName, 'Phonetic', 'SEGMENT')) # already exists
    
    add_levelDefinition(dbName=tmpDbName, 'Phonetic2', 'SEGMENT')
    
    dbObj=.load.emuDB.DBI(name=tmpDbName)
    expect_equal(length(dbObj$DBconfig$levelDefinitions), 10)
    
  })
  
  test_that("list = C(R)UD", {
    df = list_levelDefinitions(dbName=tmpDbName)
    expect_equal(as.vector(df$name[8:10]), c('Tone','Foot', 'Phonetic2'))
    expect_equal(as.vector(df$type[8:10]), c('EVENT','ITEM', 'SEGMENT'))
    expect_equal(as.vector(df$nrOfAttrDefs[1:4]), c(1, 1, 1, 3))
  })
  
  test_that("modify = CR(U)D", {
    expect_error(modify_levelDefinition(dbName=tmpDbName, name = 'Phonetic2')) # newName and newType not set
    expect_error(modify_levelDefinition(dbName=tmpDbName, name = 'Phonetic', newType='ITEM')) # linkDef present
    
    modify_levelDefinition(dbName=tmpDbName, name = 'Phonetic2', newName = 'Phonetic3')
    dbObj=.load.emuDB.DBI(name=tmpDbName)
    
    expect_equal(dbObj$DBconfig$levelDefinitions[[10]]$name, "Phonetic3")
    
  })
  
  test_that("remove = CRU(D)", {
    
    expect_error(remove_levelDefinition(dbName=tmpDbName, name="asdf")) # bad name
    expect_error(remove_levelDefinition(dbName=tmpDbName, name="Phonetic")) # linkDef present
    dbObj = .load.emuDB.DBI(name=tmpDbName)
    
    dbGetQuery(getEmuDBcon(), paste0("INSERT INTO items VALUES ('",dbObj$DBconfig$UUID,
                                     "', '0001', 'fakeBundle', 1, 'Phonetic3', 'ITEM', 20000, 1, NULL, NULL, NULL)")) # add item
    
    expect_error(remove_levelDefinition(dbName=tmpDbName, name="Phonetic3")) # item present
    
    dbGetQuery(getEmuDBcon(), paste0("DELETE FROM items WHERE db_uuid='", 
                                     dbObj$DBconfig$UUID,"'")) # items present
    
    remove_levelDefinition(dbName=tmpDbName, name="Phonetic3")
    dbObj =.load.emuDB.DBI(name=tmpDbName)
    expect_equal(length(dbObj$DBconfig$levelDefinition), 9)
    
  })
  
  
  
  # clean up
  if(is.emuDB.loaded(tmpDbName)){
    UUID = get_emuDB_UUID(dbName = tmpDbName)
    purge_emuDB(dbName = tmpDbName, dbUUID = UUID, interactive = F)
  }
  
})  

##############################
test_that("CRUD operations work for attributeDefinitions", {
  # pre clean (just in case)
  unlink(file.path(tempdir(),tmpDbName), recursive = TRUE)
  
  # copy ae and rename
  file.copy(file.path(path2extdata, '/emu/DBs/ae/'), tempdir(), recursive = T)
  file.rename(file.path(tempdir(), 'ae'), file.path(tempdir(), 'ae_copy'))
  
  # make copy of ae to mess with (caution correct DBconfig not stored)
  fp = file.path(tempdir(), tmpDbName)
  duplicate.loaded.emuDB("ae", tmpDbName, fp)
  
  test_that("add = (C)RUD", {
    expect_error(add_attributeDefinition(tmpDbName, 'Word', 'Word')) # present attrDef
    
    add_attributeDefinition(tmpDbName, 'Word', 'testAttrDef')
    df = list_attributeDefinitions(tmpDbName, 'Word')
    expect_true('testAttrDef' %in% df$name)
  })
  
  test_that("list = C(R)UD", {
    df = list_attributeDefinitions(tmpDbName, 'Word')
    expect_equal(df$name, c('Word', 'Accent', 'Text', 'testAttrDef'))
    expect_equal(df$type, c('STRING', 'STRING', 'STRING', 'STRING'))
    expect_equal(df$hasLabelGroups, c(F, F, F, F))
    expect_equal(df$hasLegalLabels, c(F, F, F, F))
  })
  
  test_that("modify = CR(U)D", {
    # not implemented yet
    
  })
  
  test_that("remove = CRU(D)", {
    expect_error(remove_attributeDefinition(tmpDbName, 'Word', 'Word'))
    expect_error(remove_attributeDefinition(tmpDbName, 'Word', 'Accent'))
    remove_attributeDefinition(tmpDbName, 'Word', 'testAttrDef')
    df = list_attributeDefinitions(tmpDbName, 'Word')
    expect_equal(nrow(df), 3)
  })
  
  
  
  # clean up
  if(is.emuDB.loaded(tmpDbName)){
    UUID = get_emuDB_UUID(dbName = tmpDbName)
    purge_emuDB(dbName = tmpDbName, dbUUID = UUID, interactive = F)
  }
  
})  

##############################
test_that("CRUD operations work for legalLabels", {
  # pre clean (just in case)
  unlink(file.path(tempdir(),tmpDbName), recursive = TRUE)
  
  # copy ae and rename
  file.copy(file.path(path2extdata, '/emu/DBs/ae/'), tempdir(), recursive = T)
  file.rename(file.path(tempdir(), 'ae'), file.path(tempdir(), 'ae_copy'))
  
  # make copy of ae to mess with (caution correct DBconfig not stored)
  fp = file.path(tempdir(), tmpDbName)
  duplicate.loaded.emuDB("ae", tmpDbName, fp)
  
  test_that("set = (C)RUD", {
    set_legalLabels(tmpDbName, 
                    levelName = 'Word', 
                    attributeDefinitionName = 'Word',
                    legalLabels=c('A', 'B', 'C'))
  })
  
  test_that("get = C(R)UD", {
    ll = get_legalLabels(tmpDbName, 
                         levelName = 'Word', 
                         attributeDefinitionName = 'Word')
    
    expect_equal(ll, c('A', 'B', 'C'))
  })
  
  test_that("modify = CR(U)D", {
    # not implemented yet
  })
  
  test_that("remove = CRU(D)", {
    remove_legalLabels(tmpDbName, 
                       levelName = 'Word', 
                       attributeDefinitionName = 'Word')
    
    ll = get_legalLabels(tmpDbName, 
                         levelName = 'Word', 
                         attributeDefinitionName = 'Word')
    
    expect_true(is.na(ll))
  })
  
  
  
  # clean up
  if(is.emuDB.loaded(tmpDbName)){
    UUID = get_emuDB_UUID(dbName = tmpDbName)
    purge_emuDB(dbName = tmpDbName, dbUUID = UUID, interactive = F)
  }
  
})  

##############################
test_that("CRUD operations work for labelGroups", {
  # pre clean (just in case)
  unlink(file.path(tempdir(),tmpDbName), recursive = TRUE)
  
  # copy ae and rename
  file.copy(file.path(path2extdata, '/emu/DBs/ae/'), tempdir(), recursive = T)
  file.rename(file.path(tempdir(), 'ae'), file.path(tempdir(), 'ae_copy'))
  
  # make copy of ae to mess with (caution correct DBconfig not stored)
  fp = file.path(tempdir(), tmpDbName)
  duplicate.loaded.emuDB("ae", tmpDbName, fp)
  
  test_that("add = (C)RUD", {
    # bad call already def. labelGroup
    expect_error(add_attrDefLabelGroup(tmpDbName,
                                       levelName = 'Phoneme', 
                                       attributeDefinitionName = 'Phoneme',
                                       labelGroupName = 'vowel',
                                       labelGroupValues = c('sdf', 'f')))
    
    add_attrDefLabelGroup(tmpDbName,
                          levelName = 'Word', 
                          attributeDefinitionName = 'Word',
                          labelGroupName = 'newGroup',
                          labelGroupValues = c('sdf', 'f'))
    
  })
  
  test_that("list = C(R)UD", {
    df = list_attrDefLabelGroups(tmpDbName,
                                 levelName = 'Utterance', 
                                 attributeDefinitionName = 'Utterance')
    expect_equal(nrow(df), 0)
    
    df = list_attrDefLabelGroups(tmpDbName,
                                 levelName = 'Phoneme', 
                                 attributeDefinitionName = 'Phoneme')
    expect_equal(nrow(df), 6)
    expect_true(df[6,]$values == "H")
    
    df = list_attrDefLabelGroups(tmpDbName,
                                 levelName = 'Word', 
                                 attributeDefinitionName = 'Word')
    expect_true(df[1,]$name == "newGroup")
    expect_true(df[1,]$values == "sdf; f")
  })
  
  test_that("modify = CR(U)D", {
    # not implemented yet
  })
  
  test_that("remove = CRU(D)", {
    expect_error(remove_attrDefLabelGroup(tmpDbName,
                                          levelName = 'Word', 
                                          attributeDefinitionName = 'Word',
                                          labelGroupName = 'notThere'))
    
    remove_attrDefLabelGroup(tmpDbName,
                             levelName = 'Word', 
                             attributeDefinitionName = 'Word',
                             labelGroupName = 'newGroup')
    
    df = list_attrDefLabelGroups(tmpDbName,
                                 levelName = 'Word', 
                                 attributeDefinitionName = 'Word')
    expect_equal(nrow(df), 0)
  })
  
  
  
  # clean up
  if(is.emuDB.loaded(tmpDbName)){
    UUID = get_emuDB_UUID(dbName = tmpDbName)
    purge_emuDB(dbName = tmpDbName, dbUUID = UUID, interactive = F)
  }
  
})  

##############################
test_that("CRUD operations work for linkDefinitions", {
  # pre clean (just in case)
  unlink(file.path(tempdir(),tmpDbName), recursive = TRUE)
  
  # copy ae and rename
  file.copy(file.path(path2extdata, '/emu/DBs/ae/'), tempdir(), recursive = T)
  file.rename(file.path(tempdir(), 'ae'), file.path(tempdir(), 'ae_copy'))
  
  # make copy of ae to mess with (caution correct DBconfig not stored)
  fp = file.path(tempdir(), tmpDbName)
  duplicate.loaded.emuDB("ae", tmpDbName, fp)
  
  test_that("add = (C)RUD", {
    # bad call (bad type)
    expect_error(add_linkDefinition(tmpDbName, "ONE_TO_TWO"))
    # bad call (link exists)
    expect_error(add_linkDefinition(tmpDbName, "ONE_TO_ONE", 
                                    superlevelName ="Syllable", 
                                    sublevelName = "Tone"))
    # bad call undefined superlevelName 
    expect_error(add_linkDefinition(tmpDbName, "ONE_TO_MANY", 
                                    superlevelName ="undefinedLevel", 
                                    sublevelName = "Tone"))
    
    
    add_linkDefinition(tmpDbName, "ONE_TO_MANY", 
                       superlevelName ="Phoneme", 
                       sublevelName = "Tone")
    
  })
  
  test_that("list = C(R)UD", {
    df = list_linkDefinitions(tmpDbName)
    expect_equal(ncol(df), 3)
    expect_equal(nrow(df), 10)
    expect_true(df$type[10] == "ONE_TO_MANY")
    expect_true(df$superlevelName[10] == "Phoneme")
    expect_true(df$sublevelName[10] == "Tone")
  })
  
  test_that("modify = CR(U)D", {
    # not implemented yet
  })
  
  test_that("remove = CRU(D)", {
    # bad call -> bad superlevelName
    expect_error(remove_linkDefinition(tmpDbName, 
                                       superlevelName ="badName", 
                                       sublevelName = "Tone"))
    # bad call -> bad sublevelName
    expect_error(remove_linkDefinition(tmpDbName, 
                                       superlevelName ="Word", 
                                       sublevelName = "badName"))
    # bad call -> links present
    expect_error(remove_linkDefinition(tmpDbName, 
                                       superlevelName ="Syllable", 
                                       sublevelName = "Tone"))
    
    remove_linkDefinition(tmpDbName, 
                          superlevelName ="Phoneme", 
                          sublevelName = "Tone")
    
    df = list_linkDefinitions(tmpDbName)
    expect_equal(ncol(df), 3)
    expect_equal(nrow(df), 9)
    
  })
  
  
  
  # clean up
  if(is.emuDB.loaded(tmpDbName)){
    UUID = get_emuDB_UUID(dbName = tmpDbName)
    purge_emuDB(dbName = tmpDbName, dbUUID = UUID, interactive = F)
  }
  
})  

