##' testthat tests for autobuild
##'
##' @author Raphael Winkelmann
context("testing autobuild functions")

suppressMessages(require('jsonlite'))

dbName = "ae"

path2orig = file.path(tempdir(), "emuR_demoData", dbName)
path2testData = file.path(tempdir(), "emuR_testthat")
path2db = file.path(path2testData, dbName)


############################
test_that("bad calls to autobuild_linkFromTimes", {
  
  # purge, delete, copy and load
  if(is.emuDB.loaded(dbName)){
    purge_emuDB(dbName, interactive = F)
  }
  unlink(path2db, recursive = T)
  file.copy(path2orig, path2testData, recursive = T)
  load_emuDB(path2db, inMemoryCache = internalVars$testingVars$inMemoryCache, verbose = F)
  
  
  expect_error(autobuild_linkFromTimes(dbName, 'Phoneti', 'Tone'))
  expect_error(autobuild_linkFromTimes(dbName, 'Phonetic', 'Ton'))
  expect_error(autobuild_linkFromTimes(dbName, 'Phonetic', 'Tone'))
  
})


##############################
test_that("correct links are present after autobuild_linkFromTimes with EVENTS", {
  
  # purge, delete, copy and load
  purge_emuDB(dbName, interactive = F)
  unlink(path2db, recursive = T)
  file.copy(path2orig, path2testData, recursive = T)
  load_emuDB(path2db, inMemoryCache = internalVars$testingVars$inMemoryCache, verbose = F)
  
  
  dbUUID = get_emuDB_UUID(dbName)
  # add linkDef.
  add_linkDefinition(dbName, "ONE_TO_MANY", superlevelName = "Phonetic", sublevelName = "Tone")
  
  autobuild_linkFromTimes(dbName, 'Phonetic', 'Tone', FALSE)
  
  qr = dbGetQuery(get_emuDBcon(dbUUID), paste0("SELECT * FROM links WHERE db_uuid='", dbUUID,"'",
                                               "AND fromID=149 AND toID=181"))
  # 
  expect_equal(dim(qr)[1], 1)
  expect_equal(qr$session, '0000')
  expect_equal(qr$bundle, 'msajc003')
  expect_equal(qr$fromID, 149)
  expect_equal(qr$toID, 181)
  
  qr = dbGetQuery(get_emuDBcon(dbUUID), paste0("SELECT * FROM links WHERE db_uuid='", dbUUID,"'",
                                               "AND fromID=156 AND toID=182"))
  
  expect_equal(qr$session, '0000')
  expect_equal(qr$bundle, 'msajc003')
  expect_equal(qr$fromID, 156) # redundant
  expect_equal(qr$toID, 182) # redundant
  
})

#############################
test_that("no duplicates are present after autobuild_linkFromTimes with EVENTs", {
  
  # purge, delete, copy and load
  purge_emuDB(dbName, interactive = F)
  unlink(path2db, recursive = T)
  file.copy(path2orig, path2testData, recursive = T)
  load_emuDB(path2db, inMemoryCache = internalVars$testingVars$inMemoryCache, verbose = F)
  
  dbUUID = get_emuDB_UUID(dbName)
  # add linkDef.
  add_linkDefinition(dbName, "ONE_TO_MANY", superlevelName = "Phonetic", sublevelName = "Tone")
  
  # addlink that will also be automatically linked
  dbSendQuery(get_emuDBcon(dbUUID), paste0("INSERT INTO links VALUES ('", dbUUID, "', '0000', 'msajc003', 140, 181, NULL)"))
  
  autobuild_linkFromTimes(dbName, 'Phonetic', 'Tone', FALSE)
  
  # extract only one link to be present
  qr = dbGetQuery(get_emuDBcon(dbUUID), paste0("SELECT * FROM links WHERE db_uuid='", dbUUID,"'",
                                               "AND fromID=149 AND toID=181"))
  
  # extract only one link to be present
  expect_equal(dim(qr)[1], 1)
  
  # if re-run nothing should change (duplicate links)
  autobuild_linkFromTimes(dbName, 'Phonetic', 'Tone', FALSE)
  qr = dbGetQuery(get_emuDBcon(dbUUID), paste0("SELECT * FROM links WHERE db_uuid='", dbUUID,"'"))
  
  expect_equal(dim(qr)[1], 840)
  
})


##############################
test_that("correct links are present after autobuild_linkFromTimes with SEGMENTS linkDef type ONE_TO_MANY", {
  
  # purge, delete, copy and load
  purge_emuDB(dbName, interactive = F)
  unlink(path2db, recursive = T)
  file.copy(path2orig, path2testData, recursive = T)
  load_emuDB(path2db, inMemoryCache = internalVars$testingVars$inMemoryCache, verbose = F)
  
  dbUUID = get_emuDB_UUID(dbName)
  # add levelDef.
  add_levelDefinition(dbName, "Phonetic2", "SEGMENT")
  # add linkDef.
  add_linkDefinition(dbName, "ONE_TO_MANY", superlevelName = "Phonetic", sublevelName = "Phonetic2")
  
  
  # add item to Phonetic2 = left edge
  dbSendQuery(get_emuDBcon(dbUUID), paste0("INSERT INTO items VALUES ('", dbUUID, "', '0000', 'msajc003', 980, 'Phonetic2', 'SEGMENT', 1, 20000, NULL, 3749, 10)"))
  autobuild_linkFromTimes(dbName, 'Phonetic', 'Phonetic2', FALSE)
  qr = dbGetQuery(get_emuDBcon(dbUUID), paste0("SELECT * FROM links WHERE db_uuid='", dbUUID,"'",
                                               " AND toID = 980"))
  expect_equal(dim(qr)[1], 1)
  expect_equal(qr$fromID, 147)
  expect_equal(qr$toID, 980)
  
  # add item to Phonetic2 = exact match
  dbSendQuery(get_emuDBcon(dbUUID), paste0("INSERT INTO items VALUES ('", dbUUID, "', '0000', 'msajc003', 981, 'Phonetic2', 'SEGMENT', 1, 20000, NULL, 3749, 1389)"))
  autobuild_linkFromTimes(dbName, 'Phonetic', 'Phonetic2', FALSE)
  qr = dbGetQuery(get_emuDBcon(dbUUID), paste0("SELECT * FROM links WHERE db_uuid='", dbUUID,"'",
                                               " AND toID = 981"))
  expect_equal(dim(qr)[1], 1)
  expect_equal(qr$fromID, 147)
  expect_equal(qr$toID, 981)
  
  # add item to Phonetic2 = completely within
  dbSendQuery(get_emuDBcon(dbUUID), paste0("INSERT INTO items VALUES ('", dbUUID, "', '0000', 'msajc003', 982, 'Phonetic2', 'SEGMENT', 1, 20000, NULL, 3800, 200)"))
  autobuild_linkFromTimes(dbName, 'Phonetic', 'Phonetic2', FALSE)
  qr = dbGetQuery(get_emuDBcon(dbUUID), paste0("SELECT * FROM links WHERE db_uuid='", dbUUID,"'",
                                               " AND toID = 982"))
  expect_equal(dim(qr)[1], 1)
  expect_equal(qr$fromID, 147)
  expect_equal(qr$toID, 982)
  
  
  # add item to Phonetic2 = left overlap
  dbSendQuery(get_emuDBcon(dbUUID), paste0("INSERT INTO items VALUES ('", dbUUID, "', '0000', 'msajc003', 983, 'Phonetic2', 'SEGMENT', 1, 20000, NULL, 3500, 1000)"))
  autobuild_linkFromTimes(dbName, 'Phonetic', 'Phonetic2', FALSE)
  qr = dbGetQuery(get_emuDBcon(dbUUID), paste0("SELECT * FROM links WHERE db_uuid='", dbUUID,"'",
                                               " AND toID = 983"))
  expect_equal(dim(qr)[1], 0)
  
  
  # add item to Phonetic2 = right overlap
  dbSendQuery(get_emuDBcon(dbUUID), paste0("INSERT INTO items VALUES ('", dbUUID, "', '0000', 'msajc003', 984, 'Phonetic2', 'SEGMENT', 1, 20000, NULL, 3800, 2000)"))
  autobuild_linkFromTimes(dbName, 'Phonetic', 'Phonetic2', FALSE)
  qr = dbGetQuery(get_emuDBcon(dbUUID), paste0("SELECT * FROM links WHERE db_uuid='", dbUUID,"'",
                                               " AND toID = 984"))
  expect_equal(dim(qr)[1], 0)
  
})

##############################
test_that("correct links are present after autobuild_linkFromTimes with SEGMENTS linkDef type MANY_TO_MANY", {
  
  # purge, delete, copy and load
  purge_emuDB(dbName, interactive = F)
  unlink(path2db, recursive = T)
  file.copy(path2orig, path2testData, recursive = T)
  load_emuDB(path2db, inMemoryCache = internalVars$testingVars$inMemoryCache, verbose = F)
  
  dbUUID = get_emuDB_UUID(dbName)
  # add levelDef.
  add_levelDefinition(dbName, "Phonetic2", "SEGMENT")
  # add linkDef.
  add_linkDefinition(dbName, "MANY_TO_MANY", superlevelName = "Phonetic", sublevelName = "Phonetic2")
  
  
  
  # add item to Phonetic2 = completely within
  #   ae$items[737, ] = c('ae_0000_msajc003_999', '0000', 'msajc003', 'Phonetic2', 999, 'SEGMENT', 1, 20000, NA, 3800, 200)
  dbSendQuery(get_emuDBcon(dbUUID), paste0("INSERT INTO items VALUES ('", dbUUID, "', '0000', 'msajc003', 980, 'Phonetic2', 'SEGMENT', 1, 20000, NULL, 3800, 200)"))
  autobuild_linkFromTimes(dbName, 'Phonetic', 'Phonetic2', FALSE)
  qr = dbGetQuery(get_emuDBcon(dbUUID), paste0("SELECT * FROM links WHERE db_uuid='", dbUUID,"'",
                                               " AND toID = 980"))
  expect_equal(dim(qr)[1], 1)
  expect_equal(qr$fromID, 147)
  expect_equal(qr$toID, 980)
  
  # add item to Phonetic2 = left overlap
  #     ae$items[737, ] = c('ae_0000_msajc003_999', '0000', 'msajc003', 'Phonetic2', 999, 'SEGMENT', 1, 20000, NA, 3500, 1000)
  dbSendQuery(get_emuDBcon(dbUUID), paste0("INSERT INTO items VALUES ('", dbUUID, "', '0000', 'msajc003', 981, 'Phonetic2', 'SEGMENT', 1, 20000, NULL, 3500, 1000)"))
  autobuild_linkFromTimes(dbName, 'Phonetic', 'Phonetic2', FALSE)
  qr = dbGetQuery(get_emuDBcon(dbUUID), paste0("SELECT * FROM links WHERE db_uuid='", dbUUID,"'",
                                               " AND toID = 981"))
  expect_equal(dim(qr)[1], 1)
  expect_equal(qr$fromID, 147)
  expect_equal(qr$toID, 981)
  
  # add item to Phonetic2 = right overlap
  #   ae$items[737, ] = c('ae_0000_msajc003_999', '0000', 'msajc003', 'Phonetic2', 999, 'SEGMENT', 1, 20000, NA, 3800, 2000)
  dbSendQuery(get_emuDBcon(dbUUID), paste0("INSERT INTO items VALUES ('", dbUUID, "', '0000', 'msajc003', 982, 'Phonetic2', 'SEGMENT', 1, 20000, NULL, 3800, 2000)"))
  autobuild_linkFromTimes(dbName, 'Phonetic', 'Phonetic2', FALSE)
  qr = dbGetQuery(get_emuDBcon(dbUUID), paste0("SELECT * FROM links WHERE db_uuid='", dbUUID,"'",
                                               " AND toID = 982"))
  expect_equal(dim(qr)[1], 2)
  expect_equal(qr$fromID, c(147, 148))
  expect_equal(qr$toID, c(982, 982))
  
  
  # add item to Phonetic2 = left and right overlap
  #   ae$items[737, ] = c('ae_0000_msajc003_999', '0000', 'msajc003', 'Phonetic2', 999, 'SEGMENT', 1, 20000, NA, 3500, 2000)
  dbSendQuery(get_emuDBcon(dbUUID), paste0("INSERT INTO items VALUES ('", dbUUID, "', '0000', 'msajc003', 983, 'Phonetic2', 'SEGMENT', 1, 20000, NULL, 3500, 2000)"))
  autobuild_linkFromTimes(dbName, 'Phonetic', 'Phonetic2', FALSE)
  qr = dbGetQuery(get_emuDBcon(dbUUID), paste0("SELECT * FROM links WHERE db_uuid='", dbUUID,"'",
                                               " AND toID = 983"))
  expect_equal(dim(qr)[1], 2)
  expect_equal(qr$fromID, c(147, 148))
  expect_equal(qr$toID, c(983, 983))
  
  
  # add item to Phonetic2 = not within
  #   ae$items[737, ] = c('ae_0000_msajc003_999', '0000', 'msajc003', 'Phonetic2', 999, 'SEGMENT', 1, 20000, NA, 200, 200)
  dbSendQuery(get_emuDBcon(dbUUID), paste0("INSERT INTO items VALUES ('", dbUUID, "', '0000', 'msajc003', 984, 'Phonetic2', 'SEGMENT', 1, 20000, NULL, 200, 200)"))
  autobuild_linkFromTimes(dbName, 'Phonetic', 'Phonetic2', FALSE)
  qr = dbGetQuery(get_emuDBcon(dbUUID), paste0("SELECT * FROM links WHERE db_uuid='", dbUUID,"'",
                                               " AND toID = 984"))
  expect_equal(dim(qr)[1], 0)
  
  
})

##############################
test_that("correct links are present after autobuild_linkFromTimes with SEGMENTS linkDef type ONE_TO_ONE", {
  
  # purge, delete, copy and load
  purge_emuDB(dbName, interactive = F)
  unlink(path2db, recursive = T)
  file.copy(path2orig, path2testData, recursive = T)
  load_emuDB(path2db, inMemoryCache = internalVars$testingVars$inMemoryCache, verbose = F)
  
  dbUUID = get_emuDB_UUID(dbName)
  # add levelDef.
  add_levelDefinition(dbName, "Phonetic2", "SEGMENT")
  # add linkDef.
  add_linkDefinition(dbName, "ONE_TO_ONE", superlevelName = "Phonetic", sublevelName = "Phonetic2")
  
  
  # add item to Phonetic2 = exact match
  #   ae$items[737, ] = c('ae_0000_msajc003_999', '0000', 'msajc003', 'Phonetic2', 999, 'SEGMENT', 1, 20000, NA, 3749, 1389)
  dbSendQuery(get_emuDBcon(dbUUID), paste0("INSERT INTO items VALUES ('", dbUUID, "', '0000', 'msajc003', 980, 'Phonetic2', 'SEGMENT', 1, 20000, NULL, 3749, 1389)"))
  autobuild_linkFromTimes(dbName, 'Phonetic', 'Phonetic2', FALSE)
  qr = dbGetQuery(get_emuDBcon(dbUUID), paste0("SELECT * FROM links WHERE db_uuid='", dbUUID,"'",
                                               " AND toID = 980"))
  expect_equal(dim(qr)[1], 1)
  expect_equal(qr$fromID, 147)
  expect_equal(qr$toID, 980)
  
  # add item to Phonetic2 = left overlap
  #   ae$items[737, ] = c('ae_0000_msajc003_999', '0000', 'msajc003', 'Phonetic2', 999, 'SEGMENT', 1, 20000, NA, 3748, 1389)
  dbSendQuery(get_emuDBcon(dbUUID), paste0("INSERT INTO items VALUES ('", dbUUID, "', '0000', 'msajc003', 981, 'Phonetic2', 'SEGMENT', 1, 20000, NULL, 3748, 1389)"))
  autobuild_linkFromTimes(dbName, 'Phonetic', 'Phonetic2', FALSE)
  qr = dbGetQuery(get_emuDBcon(dbUUID), paste0("SELECT * FROM links WHERE db_uuid='", dbUUID,"'",
                                               " AND toID = 981"))
  expect_equal(dim(qr)[1], 0)
  
  # add item to Phonetic2 = right overlap
  #   ae$items[737, ] = c('ae_0000_msajc003_999', '0000', 'msajc003', 'Phonetic2', 999, 'SEGMENT', 1, 20000, NA, 3749, 1390)
  dbSendQuery(get_emuDBcon(dbUUID), paste0("INSERT INTO items VALUES ('", dbUUID, "', '0000', 'msajc003', 982, 'Phonetic2', 'SEGMENT', 1, 20000, NULL, 3749, 1390)"))
  autobuild_linkFromTimes(dbName, 'Phonetic', 'Phonetic2', FALSE)
  qr = dbGetQuery(get_emuDBcon(dbUUID), paste0("SELECT * FROM links WHERE db_uuid='", dbUUID,"'",
                                               " AND toID = 982"))
  expect_equal(dim(qr)[1], 0)
  
  
  
  # add item to Phonetic2 = within
  #   ae$items[737, ] = c('ae_0000_msajc003_999', '0000', 'msajc003', 'Phonetic2', 999, 'SEGMENT', 1, 20000, NA, 3750, 200)
  dbSendQuery(get_emuDBcon(dbUUID), paste0("INSERT INTO items VALUES ('", dbUUID, "', '0000', 'msajc003', 983, 'Phonetic2', 'SEGMENT', 1, 20000, NULL, 3750, 200)"))
  autobuild_linkFromTimes(dbName, 'Phonetic', 'Phonetic2', FALSE)
  qr = dbGetQuery(get_emuDBcon(dbUUID), paste0("SELECT * FROM links WHERE db_uuid='", dbUUID,"'",
                                               " AND toID = 983"))
  expect_equal(dim(qr)[1], 0)
  
  
  
  # add item to Phonetic2 = not within
  #     ae$items[737, ] = c('ae_0000_msajc003_999', '0000', 'msajc003', 'Phonetic2', 999, 'SEGMENT', 1, 20000, NA, 200, 200)
  dbSendQuery(get_emuDBcon(dbUUID), paste0("INSERT INTO items VALUES ('", dbUUID, "', '0000', 'msajc003', 984, 'Phonetic2', 'SEGMENT', 1, 20000, NULL, 200, 200)"))
  autobuild_linkFromTimes(dbName, 'Phonetic', 'Phonetic2', FALSE)
  qr = dbGetQuery(get_emuDBcon(dbUUID), paste0("SELECT * FROM links WHERE db_uuid='", dbUUID,"'",
                                               " AND toID = 984"))
  expect_equal(dim(qr)[1], 0)
  
  
})

##############################
test_that("backup works correctly", {
  
  # purge, delete, copy and load
  purge_emuDB(dbName, interactive = F)
  unlink(path2db, recursive = T)
  file.copy(path2orig, path2testData, recursive = T)
  load_emuDB(path2db, inMemoryCache = internalVars$testingVars$inMemoryCache, verbose = F)
  
  dbUUID = get_emuDB_UUID(dbName)
  # add levelDef.
  add_levelDefinition(dbName, "Phonetic2", "SEGMENT")
  # add linkDef.
  add_linkDefinition(dbName, "ONE_TO_ONE", superlevelName = "Phonetic", sublevelName = "Phonetic2")
  
  
  # add item to Phonetic2 = exact match
  dbSendQuery(get_emuDBcon(dbUUID), paste0("INSERT INTO items VALUES ('", dbUUID, "', '0000', 'msajc003', 980, 'Phonetic2', 'SEGMENT', 1, 20000, NULL, 3749, 1389)"))
  autobuild_linkFromTimes(dbName, 'Phonetic', 'Phonetic2', FALSE, TRUE)
  
  
  
  qr1 = dbGetQuery(get_emuDBcon(dbUUID), paste0("SELECT * FROM items WHERE db_uuid='", dbUUID,"' AND level='Phonetic'"))
  qr2 = dbGetQuery(get_emuDBcon(dbUUID), paste0("SELECT * FROM items WHERE db_uuid='", dbUUID,"' AND level='Phonetic-autobuildBackup'"))
  # same amount of of items
  expect_equal(dim(qr1), dim(qr2))
  # cols that should be the same are
  expect_equal(qr1$session, qr2$session)
  expect_equal(qr1$bundle, qr2$bundle)
  expect_equal(qr1$seqIdx, qr2$seqIdx)
  expect_equal(qr1$sampleRate, qr2$sampleRate)
  
  
  qr1 = dbGetQuery(get_emuDBcon(dbUUID), paste0("SELECT * FROM labels WHERE db_uuid='", dbUUID,"' AND name='Phonetic'"))
  qr2 = dbGetQuery(get_emuDBcon(dbUUID), paste0("SELECT * FROM labels WHERE db_uuid='", dbUUID,"' AND name='Phonetic-autobuildBackup'"))
  # same labels
  expect_equal(dim(qr1), dim(qr2))
  expect_equal(dim(qr1$label), dim(qr2$label))
  
  
  # itemIDs are the same in items and labels table
  qr1 = dbGetQuery(get_emuDBcon(dbUUID), paste0("SELECT * FROM items WHERE db_uuid='", dbUUID,"' AND level='Phonetic-autobuildBackup'"))
  qr2 = dbGetQuery(get_emuDBcon(dbUUID), paste0("SELECT * FROM labels WHERE db_uuid='", dbUUID,"' AND name='Phonetic-autobuildBackup'"))
  expect_equal(qr1$itemID, qr2$itemID)
  
  
  # new levelDefinition is present
  dbObj = .load.emuDB.DBI(uuid = dbUUID)
  expect_equal(dbObj$DBconfig$levelDefinitions[[length(dbObj$DBconfig$levelDefinitions)]]$name, 'Phonetic-autobuildBackup')
  expect_equal(dbObj$DBconfig$levelDefinitions[[length(dbObj$DBconfig$levelDefinitions)]]$type, 'SEGMENT')
  
})

##############################
test_that("rewrite works correctly", {
  
  # purge, delete, copy and load
  purge_emuDB(dbName, interactive = F)
  unlink(path2db, recursive = T)
  file.copy(path2orig, path2testData, recursive = T)
  load_emuDB(path2db, inMemoryCache = internalVars$testingVars$inMemoryCache, verbose = F)
  
  dbUUID = get_emuDB_UUID(dbName)
  # add levelDef.
  add_levelDefinition(dbName, "Phonetic2", "SEGMENT")
  # add linkDef.
  add_linkDefinition(dbName, "ONE_TO_ONE", superlevelName = "Phonetic", sublevelName = "Phonetic2")
  
  
  
  # add item to Phonetic2
  dbSendQuery(get_emuDBcon(dbUUID), paste0("INSERT INTO items VALUES ('", dbUUID, "', '0000', 'msajc003', 980, 'Phonetic2', 'SEGMENT', 1, 20000, NULL, 3750, 200)"))
  
  # add label to Phonetic2
  dbSendQuery(get_emuDBcon(dbUUID), paste0("INSERT INTO labels VALUES ('", dbUUID, "', '0000', 'msajc003', 980, 0, 'Phonetic2', 'testLabel12')"))
  
  autobuild_linkFromTimes(dbName, 'Phonetic', 'Phonetic2', TRUE, TRUE)
  
  
  # _DBconfig.json has new definitions
  dbJson = fromJSON(readLines(file.path(path2db, "ae_DBconfig.json")), simplifyVector=T)
  expect_equal(dbJson$levelDefinitions$name[11], "Phonetic-autobuildBackup")
  expect_equal(dbJson$linkDefinitions[10,]$type, "ONE_TO_ONE")
  expect_equal(dbJson$linkDefinitions[10,]$superlevelName, "Phonetic")
  expect_equal(dbJson$linkDefinitions[10,]$sublevelName, "Phonetic2")
  
  # annot.jsons has new fields
  annotJson = fromJSON(readLines(file.path(path2db, "0000_ses", "msajc003_bndl", "msajc003_annot.json")), simplifyVector=T)
  expect_equal(annotJson$levels$name[11], "Phonetic-autobuildBackup")
  
})

# purge, delete
purge_emuDB(dbName, interactive = F)
unlink(path2db, recursive = T)

