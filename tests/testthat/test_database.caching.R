##' testthat tests for autobuild
##'
##' @author Raphael Winkelmann
context("testing caching functions")

suppressMessages(require('jsonlite'))

tmpDbName = 'ae_copy'

path2ae = system.file("extdata/emu/DBs/ae/", package = "emuR")

# load database 
if(!is.emuDB.loaded("ae")){
  load_emuDB(path2ae, verbose = F)
}

############################
# test_that("update_cache works", {
#   # pre clean (just in case)
#   unlink(file.path(tempdir(),tmpDbName), recursive = TRUE)
#   
#   # make copy of ae to mess with
#   fp = file.path(tempdir(), tmpDbName)
#   duplicate.loaded.emuDB("ae", tmpDbName, fp)
#   
#   dbUUID = get_emuDB_UUID(dbName = tmpDbName, dbUUID = NULL)
#   dbObj = .load.emuDB.DBI(uuid = dbUUID)
#   
#   # actually store DB to fs
#   store("ae", tempdir(), showProgress=F)
#   file.rename(file.path(tempdir(), "ae"), file.path(tempdir(), tmpDbName))
#   file.rename(file.path(tempdir(), tmpDbName, "ae_DBconfig.json"), file.path(tempdir(), tmpDbName, "ae_copy_DBconfig.json"))
#   
#   ################################
#   # 
#   test_that("DBconfig update is re-cached", {
#     # change entry
#     dbJson = fromJSON(readLines(file.path(tempdir(), tmpDbName, "ae_copy_DBconfig.json")), simplifyVector=T)
#     
#     dbJson$name = "ae_copy"
#     
#     pbpJSON=jsonlite::toJSON(dbJson,auto_unbox=TRUE,force=TRUE,pretty=TRUE)
#     writeLines(pbpJSON,file.path(tempdir(), tmpDbName, "ae_copy_DBconfig.json"))
#     
#     update_cache(tmpDbName, verbose=F)
#     
#     dbObj = .load.emuDB.DBI(uuid = dbUUID)
#     
#     expect_equal(dbObj$name, "ae_copy")
#   })
#   
#   ################################
#   # 
#   test_that("new bundle in new session is re-cached", {
#     dir.create(file.path(tempdir(),'ae_copy', 'new_ses'))
#     file.copy(from = file.path(tempdir(),'ae_copy', '0000_ses', 'msajc012_bndl'), 
#               to = file.path(tempdir(),'ae_copy', 'new_ses'),
#               recursive = T)
#     
#     update_cache(tmpDbName, verbose=F)
#     
#     l = list_sessions(tmpDbName)
#     expect_true("new" %in% l$name)
#     b = list_bundles(tmpDbName)
#     expect_true(any(b$session == "new" & b$name == 'msajc012'))
#     
#     sl = query(tmpDbName, "Phonetic=n")
#     expect_true(any(sl$session == "new"))
#   })
#   
#   ################################
#   # 
#   test_that("change in _annot.json is re-cached", {
#     # change entry
#     annotJson = fromJSON(readLines(file.path(tempdir(), tmpDbName, "new_ses", "msajc012_bndl", "msajc012_annot.json")), simplifyVector=T)
#     
#     annotJson$levels$items[[1]]$id = 666666
#     
#     pbpJSON=jsonlite::toJSON(annotJson,auto_unbox=TRUE,force=TRUE,pretty=TRUE)
#     writeLines(pbpJSON,file.path(tempdir(), tmpDbName, "new_ses", "msajc012_bndl", "msajc012_annot.json"))
#     
#     update_cache(tmpDbName, verbose = F)
#     
#     res = dbGetQuery(get_emuDBcon(), paste0("SELECT * FROM items WHERE db_uuid='", dbUUID, "' AND session='new' AND bundle='msajc012' AND level='Utterance'"))$itemID
# 
#     expect_true(res == 666666)
#     
#   })
#   
#   
#   ################################
#   # 
#   test_that("deleted bundle is re-cached", {
#     unlink(file.path(tempdir(),'ae_copy', 'new_ses', 'msajc012_bndl'), recursive = TRUE)
#     
#     update_cache(tmpDbName)
#     
#     res = dbGetQuery(get_emuDBcon(), paste0("SELECT * FROM items WHERE db_uuid='", dbUUID, "' AND session='new' AND bundle='msajc012'"))
#     
#     expect_true(nrow(res) == 0)
#     
#   })
#   
#   
#   
#   # cleanup 
#   unlink(file.path(tempdir(),'ae_copy'), recursive = TRUE)
#   # clean up
#   if(is.emuDB.loaded(tmpDbName)){
#     UUID = get_emuDB_UUID(dbName = tmpDbName)
#     .purge.emuDB(UUID)
#   }
#   
#   
# })

############################
test_that("sqlConnections CRUD operations work", {
  
  #########################  
  test_that("add works", {
    # only single instance is added 
    origLength = length(internalVars$sqlConnections)
    # add_emuDBcon(dbConnect(RSQLite::SQLite(), file.path(tempdir(), paste0("testthat", database.cache.suffix))))
    # add_emuDBcon(dbConnect(RSQLite::SQLite(), file.path(tempdir(), paste0("testthat", database.cache.suffix))))
    # expect_equal(length(internalVars$sqlConnections), origLength + 1)
    
  })

  #########################  
  test_that("get works", {
    # check that :memory: connection is returned by default
    # containing loaded ae
    con = get_emuDBcon()
    res = dbGetQuery(con, "SELECT uuid FROM emuDB")
    expect_true(res == "0fc618dc-8980-414d-8c7a-144a649ce199")
    
  })

  #########################
  test_that("remove works", {
    
  })
  
    
    
})



