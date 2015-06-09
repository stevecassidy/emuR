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
test_that("update_cache works", {
  # pre clean (just in case)
  unlink(file.path(tempdir(),tmpDbName), recursive = TRUE)
  
  # make copy of ae to mess with
  fp = file.path(tempdir(), tmpDbName)
  duplicate.loaded.emuDB("ae", tmpDbName, fp)
  
  dbUUID = get_emuDB_UUID(dbName = tmpDbName, dbUUID = NULL)
  dbObj = .load.emuDB.DBI(uuid = dbUUID)
  
  # actually store DB to fs
  store("ae", tempdir(), showProgress=F)
  file.rename(file.path(tempdir(), "ae"), file.path(tempdir(), tmpDbName))
  file.rename(file.path(tempdir(), tmpDbName, "ae_DBconfig.json"), file.path(tempdir(), tmpDbName, "ae_copy_DBconfig.json"))
  
  ################################
  # DBconfig update is re-cached
  
  # change entry
  dbJson = fromJSON(readLines(file.path(tempdir(), tmpDbName, "ae_copy_DBconfig.json")), simplifyVector=T)
  
  dbJson$name = "ae_copy"
  
  pbpJSON=jsonlite::toJSON(dbJson,auto_unbox=TRUE,force=TRUE,pretty=TRUE)
  writeLines(pbpJSON,file.path(tempdir(), tmpDbName, "ae_copy_DBconfig.json"))
  
  update_cache(tmpDbName)
  
  dbObj = .load.emuDB.DBI(uuid = dbUUID)
  
  expect_equal(dbObj$name, "ae_copy")
  
  ################################
  # new bundle in new session is re-cached
  dir.create(file.path(tempdir(),'ae_copy', 'new_ses'))
  file.copy(from = file.path(tempdir(),'ae_copy', '0000_ses', 'msajc012_bndl'), 
            to = file.path(tempdir(),'ae_copy', 'new_ses'),
            recursive = T)
  
  update_cache(tmpDbName)
  
  l = list_sessions(tmpDbName)
  
  print(l)
  
  ################################
  # deleted bundle is re-cached
  # unlink(file.path(tempdir(),'ae_copy', '0000_ses', 'msajc012_bndl'), recursive = TRUE)
  
  # update_cache(tmpDbName)
  
    
  # cleanup 
  unlink(file.path(tempdir(),'ae_copy'), recursive = TRUE)
  # clean up
  if(is.emuDB.loaded(tmpDbName)){
    UUID = get_emuDB_UUID(dbName = tmpDbName)
    .purge.emuDB(UUID)
  }
  
  
})



