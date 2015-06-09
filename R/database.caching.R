##' Update cache of emuDB
##' 
##' Updates sqlite cache of loaded emuDB. This can and should be used
##' to update changes to precached/loaded DBs as it only updates the deltas 
##' in the cache which is considerably faster than reloading and therefore 
##' recacheing the entire DB.
##' @param dbName name of EMU database
##' @param dbUUID optional UUID of emuDB
##' @author Raphael Winkelmann
##' @export
##' @keywords emuDB database Emu 
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
  # calc. new md5 sum
  new.MD5DBconfigJSON = md5sum(normalizePath(dbCfgPath))[[1]]
  
  old.MD5DBconfigJSON = dbGetQuery(getEmuDBcon(), paste0("SELECT MD5DBconfigJSON FROM emuDB WHERE uuid='", dbUUID, "'"))[[1]]
  if(is.na(old.MD5DBconfigJSON) | old.MD5DBconfigJSON != new.MD5DBconfigJSON){
    print('Reloading _DBconfig.json ...')
    # load DBconfig
    schema=load.emuDB.DBconfig(dbCfgPath)
    # set transient values
    schema=.update.transient.schema.values(schema)
    # create db object
    db=create.database(name = schema[['name']],basePath = normalizePath(dbObj$basePath),DBconfig = schema)
    
    dbCfgJSON=jsonlite::toJSON(db$DBconfig, auto_unbox=TRUE, force=TRUE, pretty=TRUE)
    # update entry
    dbGetQuery(getEmuDBcon(), paste0("UPDATE emuDB SET DBconfigJSON = '", dbCfgJSON , "', ",
                                     "MD5DBconfigJSON = '", new.MD5DBconfigJSON, "' ",
                                     "WHERE uuid = '", dbUUID, "'"))
    
  }
  
  ######################################
  # check which _annot.json files need reloading
  
  # todo remove old entries in case of rename
  
  bt = dbReadTable(getEmuDBcon(), "bundle")
  bndls = list_bundles(dbName = dbName, dbUUID = dbUUID)
  
  sesPattern=paste0('.*', session.suffix, '$')
  sesPaths=list.files(path=dbObj$basePath, sesPattern)
  
  for(s in sesPaths){
    bndlPattern=paste0('.*', bundle.dir.suffix, '$')
    bndlPaths=list.files(path=file.path(dbObj$basePath, s), bndlPattern)
    for(b in bndlPaths){
      bn = gsub(bundle.dir.suffix,"", b)
      # calc. new md5 sum
      annotPath = file.path(dbObj$basePath, s, b, paste0(bn, bundle.annotation.suffix, ".json"))
      new.MD5annotJSON = md5sum(normalizePath(annotPath))
      
      old.MD5annotJSON = dbGetQuery(getEmuDBcon(), paste0("SELECT MD5annotJSON FROM bundle WHERE ",
                                                          "db_uuid='", dbUUID, "' AND ",
                                                          "session='", gsub(session.suffix,"", s), "' AND ",
                                                          "name='", bn, "'"))$MD5annotJSON
      
      # set to empty string if NA or empty character()
      if(length(old.MD5annotJSON) == 0){
        old.MD5annotJSON = ""
      }
      if(is.na(old.MD5annotJSON)){
        old.MD5annotJSON = ""
      }
      
      
      if(old.MD5annotJSON != new.MD5annotJSON){
        print('Reloading _annot.json ...')
        
        annoJSONLns=readLines(annotPath,encoding="UTF-8")
        annoJSON=paste(annoJSONLns,collapse='')
        bundle=jsonlite::fromJSON(annoJSON,simplifyVector=FALSE)
        #class(bundle) <- 'emuDB.bundle'
        bundle=as.bundle(bundle)
        # set missing fields
        namedLevels=set.list.names(bundle[['levels']],'name')
        bundle[['levels']]=namedLevels
        bundle[['mediaFilePath']]=file.path(file.path(dbObj$basePath, s, b),bundle[['annotates']])
        bundle[['db_UUID']]=dbObj$DBconfig[['UUID']]
        # set session name
        bundle[['session']]=gsub(session.suffix,"", s)
        
        
        # update MD5 value of DBI model in bundle table
        dbGetQuery(getEmuDBcon(), paste0("UPDATE bundle SET MD5annotJSON = '", new.MD5annotJSON , "' ",
                                         "WHERE db_uuid = '", dbUUID, "' AND ",
                                         "      session = '", gsub(session.suffix,"", s), "' AND ",
                                         "      name = '", bn, "'"))
        
        # delete old items/label/links entries
        .remove.bundle.annot.DBI(dbUUID=dbUUID,bundle=bundle)
        # and store them
        .store.bundle.annot.DBI(dbUUID=dbUUID,bundle=bundle)
        
      }
      
    }
    
  }
  
}

# FOR DEVELOPMENT 
library('testthat') 
test_file('tests/testthat/test_database.caching.R')
