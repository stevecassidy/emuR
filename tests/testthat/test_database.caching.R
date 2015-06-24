##' testthat tests for autobuild
##'
##' @author Raphael Winkelmann
context("testing caching functions")

suppressMessages(require('jsonlite'))

dbName = "ae"

path2orig = file.path(tempdir(), "emuR_demoData", dbName)
path2testData = file.path(tempdir(), "emuR_testthat")
path2db = file.path(path2testData, dbName)

###########################
test_that("update_cache works", {
  
  # purge, delete, copy and load
  if(is.emuDB.loaded(dbName)){
    purge_emuDB(dbName, interactive = F)
  }
  unlink(path2db, recursive = T)
  file.copy(path2orig, path2testData, recursive = T)
  load_emuDB(path2db, inMemoryCache = internalVars$testingVars$inMemoryCache, verbose = F)
  
  dbUUID = get_emuDB_UUID(dbName)
  
  
  
  ################################
  # 
  test_that("DBconfig update is re-cached", {
    # change entry
    dbJson = fromJSON(readLines(file.path(path2db, "ae_DBconfig.json")), simplifyVector=T)
    
    dbJson$name = "ae_copy"
    
    pbpJSON=jsonlite::toJSON(dbJson,auto_unbox=TRUE,force=TRUE,pretty=TRUE)
    writeLines(pbpJSON,file.path(path2db, "ae_DBconfig.json"))
    
    update_cache(dbName, verbose=F)
    
    dbObj = .load.emuDB.DBI(uuid = dbUUID)
    
    expect_equal(dbObj$DBconfig$name, "ae_copy")
  })
  
  ################################
  # 
  test_that("new bundle in new session is re-cached", {
    dir.create(file.path(path2db, 'new_ses'))
    file.copy(from = file.path(path2db, '0000_ses', 'msajc010_bndl'), 
              to = file.path(path2db, 'new_ses'),
              recursive = T)
    
    update_cache(dbName, verbose=F)
    
    l = list_sessions(dbName)
    expect_true("new" %in% l$name)
    b = list_bundles(dbName)
    expect_true(any(b$session == "new" & b$name == 'msajc010'))
    
    sl = query(dbName, "Phonetic=n")
    expect_true(any(sl$session == "new"))
  })
  
  ################################
  # 
  test_that("change in _annot.json is re-cached", {
    # change entry
    annotJson = fromJSON(readLines(file.path(path2db, "new_ses", "msajc010_bndl", "msajc010_annot.json")), simplifyVector=T)
    
    annotJson$levels$items[[1]]$id = 666666
    
    pbpJSON=jsonlite::toJSON(annotJson,auto_unbox=TRUE,force=TRUE,pretty=TRUE)
    writeLines(pbpJSON,file.path(path2db, "new_ses", "msajc010_bndl", "msajc010_annot.json"))
    
    update_cache(dbName, verbose = F)
    
    res = dbGetQuery(get_emuDBcon(dbUUID), paste0("SELECT * FROM items WHERE db_uuid='", dbUUID, "' AND session='new' AND bundle='msajc010' AND level='Utterance'"))$itemID

    expect_true(res == 666666)
    
  })
  
  
  ################################
  # 
  test_that("deleted bundle is re-cached", {
    unlink(file.path(path2db, 'new_ses', 'msajc010_bndl'), recursive = TRUE)
    
    update_cache(dbName, verbose = F)
    
    res = dbGetQuery(get_emuDBcon(dbUUID), paste0("SELECT * FROM items WHERE db_uuid='", dbUUID, "' AND session='new' AND bundle='msajc010'"))
    
    expect_true(nrow(res) == 0)
    
    bndls = list_bundles(dbName)
    expect_false(any(bndls$session == "new"))
    
    
  })
  
  ################################
  # 
  test_that("deleted session is re-cached", {
    unlink(file.path(path2db, 'new_ses'), recursive = TRUE)
    ses = list_sessions(dbName)
    expect_true(any(ses$name == "new"))
    
    update_cache(dbName, verbose = F)
    ses = list_sessions(dbName)

    expect_false(any(ses$name == "new"))
    
  })
  
  # purge, delete, copy and load
  if(is.emuDB.loaded(dbName)){
    purge_emuDB(dbName, interactive = F)
  }
  unlink(path2db, recursive = T)
  
  
  
})

############################
test_that("sqlConnections CRUD operations work", {
  
  path2testDB = file.path(path2testData, paste0("testthat", database.cache.suffix))
  
  fileCon = NULL
  
  #########################
  test_that("add works", {
    # only single instance is added 
    add_emuDBcon(dbConnect(RSQLite::SQLite(), path2testDB), path2testDB)
    origLength = length(internalVars$sqlConnections)
    fileCon = add_emuDBcon(dbConnect(RSQLite::SQLite(), path2testDB), path2testDB)
    expect_equal(length(internalVars$sqlConnections), origLength)
    
  })

  #########################  
  test_that("get works", {
    # check that :memory: connection is returned by default
    purge_all_emuDBs(interactive = F)
    expect_true(length(internalVars$sqlConnections) == 0)
    
    inMemCon = get_emuDBcon()
    expect_true(length(internalVars$sqlConnections) == 1)
  
  })

  #########################
  test_that("remove works", {
    remove_emuDBcon(":memory:")    
    expect_true(length(internalVars$sqlConnections) == 0)
  })
  
  # cleanup 
  unlink(path2testDB)
  
})



