##' Update cache of emuDB
##' 
##' @param dbName name of EMU database
##' @param dbUUID optional UUID of emuDB
##' @author Raphael Winkelmann
##' @export
##' @keywords emuDB database Emu 
##' @examples
update_cache <- function(dbName, dbUUID=NULL){
  #########################
  # get dbObj
  dbUUID = get_emuDB_UUID(dbName = dbName, dbUUID = dbUUID)
  dbObj = .load.emuDB.DBI(uuid = dbUUID)
  
  ######################################
  # check if DBconfig needs reload
  
  dbCfgPattern=paste0('.*',database.schema.suffix,'$')
  dbCfgFiles=list.files(path=dbObj$basePath,dbCfgPattern)
  dbCfgFileCount=length(dbCfgFiles)
  if(dbCfgFileCount==0){
    stop("Could not find global DB config JSON file (regex pattern: ",dbCfgPattern,") in ",dbObj$basePath)
  }
  if(dbCfgFileCount>1){
    stop("Found multiple global DB config JSON files (regex pattern: ",dbCfgPattern,") in ",dbObj$basePath)
  }
  
  dbCfgPath=file.path(dbObj$basePath,dbCfgFiles[[1]])
  if(!file.exists(dbCfgPath)){
    stop("Could not find database info file: ",dbCfgPath,"\n")
  }
  # calc. md5 sum
  new.MD5DBconfigJSON = md5sum(normalizePath(dbCfgPath))
  
  old.MD5DBconfigJSON = dbGetQuery(getEmuDBcon(), paste0("SELECT MD5DBconfigJSON FROM emuDB WHERE uuid='", dbUUID, "'"))$MD5DBconfigJSON
  if(old.MD5DBconfigJSON != new.MD5DBconfigJSON){
    print('Reloading _DBconfig.json ...')
    # load DBconfig
    schema=load.emuDB.DBconfig(dbCfgPath)
    # set transient values
    schema=.update.transient.schema.values(schema)
    # create db object
    db=create.database(name = schema[['name']],basePath = normalizePath(databaseDir),DBconfig = schema)
    # store
    .store.emuDB.DBI(db, MD5DBconfigJSON)
  }
  
  ######################################
  # check which _annot.json files need reloading
  bt = dbReadTable(getEmuDBcon(), "bundle")
  print(bt)
  
  
}

# FOR DEVELOPMENT 
library('testthat') 
test_file('tests/testthat/test_database.caching.R')
