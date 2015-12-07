##' testthat tests for database.DBconfig
##'
##' @author Raphael Winkelmann
context("testing database.DBconfig functions")

dbName = 'ae'

path2orig = file.path(tempdir(), "emuR_demoData", dbName)
path2testData = file.path(tempdir(), "emuR_testthat")
path2db = file.path(path2testData, dbName)


##############################
test_that("get.levelDefinition returns correct levelDef", {
  
  # purge, delete, copy and load
  if(is.emuDB.loaded(dbName)){
    purge_emuDB(dbName, interactive = F)
  }
  unlink(path2db, recursive = T)
  file.copy(path2orig, path2testData, recursive = T)
  load_emuDB(path2db, inMemoryCache = internalVars$testingVars$inMemoryCache, verbose = F)
  
  
  #########################
  # get dbObj
  dbUUID = get_UUID(dbName = "ae", dbUUID = NULL)
  dbObj = .load.emuDB.DBI(uuid = dbUUID)
  
  
  ld = get.levelDefinition(dbObj$DBconfig, 'Phonetic')
  expect_equal(ld$name, 'Phonetic')
  expect_equal(ld$type, 'SEGMENT')
  expect_equal(ld$attributeDefinitions[[1]]$name, 'Phonetic')
  expect_equal(ld$attributeDefinitions[[1]]$type, 'STRING')
})

##############################
test_that("CRUD operations work for ssffTrackDefinitions", {
  
  # purge, delete, copy and load
  purge_emuDB(dbName, interactive = F)
  unlink(path2db, recursive = T)
  file.copy(path2orig, path2testData, recursive = T)
  load_emuDB(path2db, inMemoryCache = internalVars$testingVars$inMemoryCache, verbose = F)
  
  test_that("add = (C)RUD", {
    expect_error(add_ssffTrackDefinition(dbName=dbName, 'fm'))
    expect_error(add_ssffTrackDefinition(dbName=dbName, 'fm', 'bla'))
    expect_error(add_ssffTrackDefinition(dbName=dbName, 'newTrackName', 'badColName', 'pit', 
                                         onTheFlyFunctionName = 'mhsF0', interactive = T))
    
    add_ssffTrackDefinition(dbName=dbName, 'newTrackName', 'pitch', 'pit', 
                            onTheFlyFunctionName = 'mhsF0', interactive = F)
    
    pitFilePaths = list.files(path2db, pattern = 'pit$', recursive = T)
    expect_equal(length(pitFilePaths), 7)
    
  })
  
  test_that("list = C(R)UD", {
    df = list_ssffTrackDefinitions(dbName=dbName)
    expect_equal(df$name, c('dft','fm', 'newTrackName'))
    expect_equal(df$columnName, c('dft','fm', 'pitch'))
    expect_equal(df$fileExtension, c('dft','fms', 'pit'))
  })
  
  test_that("modify = CR(U)D", {
    # currently not implemented
  })
  
  test_that("remove = CRU(D)", {
    # bad name
    expect_error(remove_ssffTrackDefinition(dbName=dbName, name="asdf"))
    remove_ssffTrackDefinition(dbName=dbName, name="newTrackName", deleteFiles = T)
    # check that _DBconfig entry is deleted
    uuid=get_UUID(dbName, NULL)
    dbObj = .load.emuDB.DBI(uuid = uuid)
    expect_equal(dbObj$DBconfig$ssffTrackDefinitions[[1]]$name, "dft")
    expect_equal(dbObj$DBconfig$ssffTrackDefinitions[[2]]$name, "fm")
    
    # check that files have been deleted
    filePaths = list_bundleFilePaths(dbName=dbName, "pit", dbUUID = NULL)
    expect_equal(length(filePaths), 0)
    
  })
  
})

##############################
test_that("CRUD operations work for levelDefinitions", {
  
  # purge, delete, copy and load
  purge_emuDB(dbName, interactive = F)
  unlink(path2db, recursive = T)
  file.copy(path2orig, path2testData, recursive = T)
  load_emuDB(path2db, inMemoryCache = internalVars$testingVars$inMemoryCache, verbose = F)
  
  
  test_that("add = (C)RUD", {
    expect_error(add_levelDefinition(dbName=dbName, 'Phonetic', 'SEGM')) # bad type
    expect_error(add_levelDefinition(dbName=dbName, 'Phonetic', 'SEGMENT')) # already exists
    
    add_levelDefinition(dbName=dbName, 'Phonetic2', 'SEGMENT')
    
    dbObj=.load.emuDB.DBI(name=dbName)
    expect_equal(length(dbObj$DBconfig$levelDefinitions), 10)
    
  })
  
  test_that("list = C(R)UD", {
    df = list_levelDefinitions(dbName=dbName)
    expect_equal(as.vector(df$name[8:10]), c('Tone','Foot', 'Phonetic2'))
    expect_equal(as.vector(df$type[8:10]), c('EVENT','ITEM', 'SEGMENT'))
    expect_equal(as.vector(df$nrOfAttrDefs[1:4]), c(1, 1, 1, 3))
  })
  
  test_that("modify = CR(U)D", {
    # currently not implemented
  })
  
  test_that("remove = CRU(D)", {
    
    expect_error(remove_levelDefinition(dbName=dbName, name="asdf")) # bad name
    expect_error(remove_levelDefinition(dbName=dbName, name="Phonetic")) # linkDef present
    dbUUID = get_UUID(dbName = "ae", dbUUID = NULL)
    
    dbGetQuery(get_emuDBcon(dbUUID), paste0("INSERT INTO items VALUES ('",dbUUID,
                                            "', '0001', 'fakeBundle', 1, 'Phonetic2', 'ITEM', 20000, 1, NULL, NULL, NULL)")) # add item
    
    expect_error(remove_levelDefinition(dbName=dbName, name="Phonetic2")) # item present
    
    dbGetQuery(get_emuDBcon(dbUUID), paste0("DELETE FROM items WHERE db_uuid='", 
                                            dbUUID,"'")) # items present
    
    remove_levelDefinition(dbName=dbName, name="Phonetic2")
    dbObj =.load.emuDB.DBI(name=dbName)
    expect_equal(length(dbObj$DBconfig$levelDefinition), 9)
    
  })
  
})  

##############################
test_that("CRUD operations work for attributeDefinitions", {
  
  # purge, delete, copy and load
  purge_emuDB(dbName, interactive = F)
  unlink(path2db, recursive = T)
  file.copy(path2orig, path2testData, recursive = T)
  load_emuDB(path2db, inMemoryCache = internalVars$testingVars$inMemoryCache, verbose = F)
  
  
  test_that("add = (C)RUD", {
    expect_error(add_attributeDefinition(dbName, 'Word', 'Word')) # present attrDef
    
    add_attributeDefinition(dbName, 'Word', 'testAttrDef')
    df = list_attributeDefinitions(dbName, 'Word')
    expect_true('testAttrDef' %in% df$name)
  })
  
  test_that("list = C(R)UD", {
    df = list_attributeDefinitions(dbName, 'Word')
    expect_equal(df$name, c('Word', 'Accent', 'Text', 'testAttrDef'))
    expect_equal(df$type, c('STRING', 'STRING', 'STRING', 'STRING'))
    expect_equal(df$hasLabelGroups, c(F, F, F, F))
    expect_equal(df$hasLegalLabels, c(F, F, F, F))
  })
  
  test_that("modify = CR(U)D", {
    # not implemented yet
    
  })
  
  test_that("remove = CRU(D)", {
    expect_error(remove_attributeDefinition(dbName, 'Word', 'Word'))
    expect_error(remove_attributeDefinition(dbName, 'Word', 'Accent'))
    remove_attributeDefinition(dbName, 'Word', 'testAttrDef')
    df = list_attributeDefinitions(dbName, 'Word')
    expect_equal(nrow(df), 3)
  })
  
})  

##############################
test_that("CRUD operations work for legalLabels", {
  
  # purge, delete, copy and load
  purge_emuDB(dbName, interactive = F)
  unlink(path2db, recursive = T)
  file.copy(path2orig, path2testData, recursive = T)
  load_emuDB(path2db, inMemoryCache = internalVars$testingVars$inMemoryCache, verbose = F)
  
  test_that("set = (C)RUD", {
    set_legalLabels(dbName, 
                    levelName = 'Word', 
                    attributeDefinitionName = 'Word',
                    legalLabels=c('A', 'B', 'C'))
  })
  
  test_that("get = C(R)UD", {
    ll = get_legalLabels(dbName, 
                         levelName = 'Word', 
                         attributeDefinitionName = 'Word')
    
    expect_equal(ll, c('A', 'B', 'C'))
  })
  
  test_that("modify = CR(U)D", {
    # not implemented yet
  })
  
  test_that("remove = CRU(D)", {
    remove_legalLabels(dbName, 
                       levelName = 'Word', 
                       attributeDefinitionName = 'Word')
    
    ll = get_legalLabels(dbName, 
                         levelName = 'Word', 
                         attributeDefinitionName = 'Word')
    
    expect_true(is.na(ll))
  })
  
})  

##############################
test_that("CRUD operations work for labelGroups", {
  
  # purge, delete, copy and load
  purge_emuDB(dbName, interactive = F)
  unlink(path2db, recursive = T)
  file.copy(path2orig, path2testData, recursive = T)
  load_emuDB(path2db, inMemoryCache = internalVars$testingVars$inMemoryCache, verbose = F)
  
  test_that("add = (C)RUD", {
    # bad call already def. labelGroup
    expect_error(add_attrDefLabelGroup(dbName,
                                       levelName = 'Phoneme', 
                                       attributeDefinitionName = 'Phoneme',
                                       labelGroupName = 'vowel',
                                       labelGroupValues = c('sdf', 'f')))
    
    add_attrDefLabelGroup(dbName,
                          levelName = 'Word', 
                          attributeDefinitionName = 'Word',
                          labelGroupName = 'newGroup',
                          labelGroupValues = c('sdf', 'f'))
    
  })
  
  test_that("list = C(R)UD", {
    df = list_attrDefLabelGroups(dbName,
                                 levelName = 'Utterance', 
                                 attributeDefinitionName = 'Utterance')
    expect_equal(nrow(df), 0)
    
    df = list_attrDefLabelGroups(dbName,
                                 levelName = 'Phoneme', 
                                 attributeDefinitionName = 'Phoneme')
    expect_equal(nrow(df), 6)
    expect_true(df[6,]$values == "H")
    
    df = list_attrDefLabelGroups(dbName,
                                 levelName = 'Word', 
                                 attributeDefinitionName = 'Word')
    expect_true(df[1,]$name == "newGroup")
    expect_true(df[1,]$values == "sdf; f")
  })
  
  test_that("modify = CR(U)D", {
    # not implemented yet
  })
  
  test_that("remove = CRU(D)", {
    expect_error(remove_attrDefLabelGroup(dbName,
                                          levelName = 'Word', 
                                          attributeDefinitionName = 'Word',
                                          labelGroupName = 'notThere'))
    
    remove_attrDefLabelGroup(dbName,
                             levelName = 'Word', 
                             attributeDefinitionName = 'Word',
                             labelGroupName = 'newGroup')
    
    df = list_attrDefLabelGroups(dbName,
                                 levelName = 'Word', 
                                 attributeDefinitionName = 'Word')
    expect_equal(nrow(df), 0)
  })
  
})  

##############################
test_that("CRUD operations work for linkDefinitions", {
  
  # purge, delete, copy and load
  purge_emuDB(dbName, interactive = F)
  unlink(path2db, recursive = T)
  file.copy(path2orig, path2testData, recursive = T)
  load_emuDB(path2db, inMemoryCache = internalVars$testingVars$inMemoryCache, verbose = F)
  
  test_that("add = (C)RUD", {
    # bad call (bad type)
    expect_error(add_linkDefinition(dbName, "ONE_TO_TWO"))
    # bad call (link exists)
    expect_error(add_linkDefinition(dbName, "ONE_TO_ONE", 
                                    superlevelName ="Syllable", 
                                    sublevelName = "Tone"))
    # bad call undefined superlevelName 
    expect_error(add_linkDefinition(dbName, "ONE_TO_MANY", 
                                    superlevelName ="undefinedLevel", 
                                    sublevelName = "Tone"))
    
    
    add_linkDefinition(dbName, "ONE_TO_MANY", 
                       superlevelName ="Phoneme", 
                       sublevelName = "Tone")
    
  })
  
  test_that("list = C(R)UD", {
    df = list_linkDefinitions(dbName)
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
    expect_error(remove_linkDefinition(dbName, 
                                       superlevelName ="badName", 
                                       sublevelName = "Tone"))
    # bad call -> bad sublevelName
    expect_error(remove_linkDefinition(dbName, 
                                       superlevelName ="Word", 
                                       sublevelName = "badName"))
    # bad call -> links present
    expect_error(remove_linkDefinition(dbName, 
                                       superlevelName ="Syllable", 
                                       sublevelName = "Tone"))
    
    remove_linkDefinition(dbName, 
                          superlevelName ="Phoneme", 
                          sublevelName = "Tone")
    
    df = list_linkDefinitions(dbName)
    expect_equal(ncol(df), 3)
    expect_equal(nrow(df), 9)
    
  })
  
})  


##############################
test_that("CRUD operations work for labelGroups", {
  
  # purge, delete, copy and load
  purge_emuDB(dbName, interactive = F)
  unlink(path2db, recursive = T)
  file.copy(path2orig, path2testData, recursive = T)
  load_emuDB(path2db, inMemoryCache = internalVars$testingVars$inMemoryCache, verbose = F)
  
  
  test_that("add = (C)RUD", {
    add_labelGroup(dbName, 
                   name = 'testLG',
                   values = c('a', 'b', 'c'))  
  })
  
  test_that("list = C(R)UD", {
    df = list_labelGroups(dbName)
    expect_true(df$name == 'testLG')
    expect_true(df$values =='a; b; c')
  })
  
  test_that("modify = CR(U)D", {
    # not implemented yet
  })
  
  test_that("remove = CRU(D)", {
    # bad call -> bad name
    expect_error(remove_labelGroup(dbName, 
                                   name = 'badName'))
    
    remove_labelGroup(dbName, 
                      name = 'testLG')
    df = list_labelGroups(dbName)
    expect_equal(nrow(df), 0)
  })
})  

# 
test_that("purge, delete", {
  purge_emuDB(dbName, interactive = F)
  unlink(path2db, recursive = T)
})


