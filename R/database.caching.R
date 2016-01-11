## Update cache of emuDB
## 
## Updates sqlite cache of loaded emuDB. This can be used
## to update changes to precached/loaded DBs as it only updates the deltas 
## in the cache which is considerably faster than reloading and therefore 
## recacheing the entire DB. This function is now called by load_emuDB if 
## load_emuDB finds a preexisting cache.
## @param emuDBhandle 
## @param verbose display infos
## @keywords emuDB database Emu 
update_cache <- function(emuDBhandle, verbose = TRUE){

  ######################################
  # check if DBconfig needs reload
  
  dbCfgPattern = paste0('.*', database.schema.suffix,'$')
  dbCfgFiles = list.files(path = emuDBhandle$basePath, dbCfgPattern)
  dbCfgFileCount = length(dbCfgFiles)
  if(dbCfgFileCount == 0){
    stop("Could not find global DB config JSON file (regex pattern: ", dbCfgPattern, ") in ", emuDBhandle$basePath)
  }
  if(dbCfgFileCount > 1){
    stop("Found multiple global DB config JSON files (regex pattern: ", dbCfgPattern, ") in ", emuDBhandle$basePath)
  }
  
  dbCfgPath=file.path(emuDBhandle$basePath, dbCfgFiles[[1]])
  if(!file.exists(dbCfgPath)){
    stop("Could not find database info file: ", dbCfgPath, "\n")
  }
  # calc. new md5 sum
  new.MD5DBconfigJSON = md5sum(normalizePath(dbCfgPath))[[1]]
  
  old.MD5DBconfigJSON = dbGetQuery(emuDBhandle$connection, paste0("SELECT MD5DBconfigJSON FROM emuDB WHERE uuid='", emuDBhandle$UUID, "'"))[[1]]
  if(is.na(old.MD5DBconfigJSON) | old.MD5DBconfigJSON != new.MD5DBconfigJSON){
    if(verbose){
      print('Reloading _DBconfig.json ...')
    }
    # load DBconfig
    DBconfig = jsonlite::toJSON(db$DBconfig, auto_unbox=TRUE, force=TRUE, pretty=TRUE)
    # update entry
    dbGetQuery(emuDBhandle$connection, paste0("UPDATE emuDB SET DBconfigJSON = '", DBconfig , "', ",
                                              "MD5DBconfigJSON = '", new.MD5DBconfigJSON, "' ",
                                              "WHERE uuid = '", emuDBhandle$UUID, "'"))
    
  }
  
  ######################################
  # check which _annot.json files need reloading
  bt = dbReadTable(emuDBhandle$connection, "bundle")
  bndls = list_bundles(emuDBhandle)
  
  sesPattern=paste0('.*', session.suffix, '$')
  sesPaths=list.files(path=emuDBhandle$basePath, sesPattern)
  
  curSes = list_sessions(emuDBhandle)
  curSes["found"] = F
  
  curBndls = list_bundles(emuDBhandle)
  curBndls["found"] = F
  
  progress = 0
  
  if(verbose){
    bndlPaths = list.dirs(emuDBhandle$basePath, recursive=T)
    nrOfBndls = length(bndlPaths[grepl(paste0(".", "*", session.suffix, ".*", bundle.dir.suffix, "$"), bndlPaths) == T])
    cat("INFO: Checking if cache needs update for ", nrOfBndls, " bundles...\n")
    pb = txtProgressBar(min = 0, max = nrOfBndls, initial = progress, style=3)
    setTxtProgressBar(pb, progress)
  }
  
  for(s in sesPaths){
    sn = gsub(session.suffix,"", s)
    bndlPattern=paste0('.*', bundle.dir.suffix, '$')
    bndlPaths=list.files(path=file.path(emuDBhandle$basePath, s), bndlPattern)
    
    if(!sn %in% curSes$name){
      dbGetQuery(emuDBhandle$connection, paste0("INSERT INTO session VALUES ('", emuDBhandle$UUID, "', '", sn, "')"))
    }
    
    # mark as found
    curSes$found[curSes$name == sn] = T
    
    for(b in bndlPaths){
      
      bn = gsub(bundle.dir.suffix,"", b)
      # mark as found
      curBndls$found[curBndls$session == sn & curBndls$name == bn] = T
      
      # calc. new md5 sum
      annotPath = file.path(emuDBhandle$basePath, s, b, paste0(bn, bundle.annotation.suffix, ".json"))
      new.MD5annotJSON = md5sum(normalizePath(annotPath))
      
      old.MD5annotJSON = dbGetQuery(emuDBhandle$connection, paste0("SELECT MD5annotJSON FROM bundle WHERE ",
                                                                   "db_uuid='", emuDBhandle$UUID, "' AND ",
                                                                   "session='", sn, "' AND ",
                                                                   "name='", bn, "'"))$MD5annotJSON
      
      # set to empty string if NA or empty character()
      if(length(old.MD5annotJSON) == 0){
        old.MD5annotJSON = ""
      }
      if(is.na(old.MD5annotJSON)){
        old.MD5annotJSON = ""
      }
      
      
      if(old.MD5annotJSON != new.MD5annotJSON){
        if(verbose){
          print(paste0("Reloading _annot.json for bundle in session : '", sn,"' with name: '", bn, "'"))
        }
        bundle=jsonlite::fromJSON(annotPath, simplifyVector=FALSE)
        bundle=as.bundle(bundle)
        # set missing fields
        namedLevels=set.list.names(bundle[['levels']],'name')
        bundle[['levels']]=namedLevels
        mediaFilePath=file.path(file.path(emuDBhandle$basePath, s, b),bundle[['annotates']])
        bundle[['db_UUID']]=emuDBhandle$UUID
        # set session name
        bundle[['session']]=sn
        
        # check if bundle entry exists
        if(!any(curBndls$session == sn & curBndls$name == bn)){
          sR = attr(read.AsspDataObj(mediaFilePath), "sampleRate")
          dbGetQuery(emuDBhandle$connection, paste0("INSERT INTO bundle (db_uuid,session,name,annotates,sampleRate,MD5annotJSON) VALUES 
                                                  ('",emuDBhandle$UUID,"', '", 
                                                    sn,"', '", 
                                                    bn,"', '", 
                                                    bundle$annotates,"', '", 
                                                    sR,"', '", 
                                                    new.MD5annotJSON,"')"))
        }else{
          # update MD5 value of DBI model in bundle table
          dbGetQuery(emuDBhandle$connection, paste0("UPDATE bundle SET MD5annotJSON = '", new.MD5annotJSON , "' ",
                                                    "WHERE db_uuid = '", emuDBhandle$UUID, "' AND ",
                                                    "      session = '", sn, "' AND ",
                                                    "      name = '", bn, "'"))
        }
        
        # delete old items/label/links entries
        .remove.bundle.annot.DBI(dbUUID=emuDBhandle$UUID,bundle=bundle)
        # and store them
        .store.bundle.annot.DBI(dbUUID=emuDBhandle$UUID,bundle=bundle)
        
        # only build redunant links if non-empty bundle
        qRes = dbGetQuery(emuDBhandle$connection, paste0("SELECT * FROM items WHERE ",
                                                         "db_uuid = '", emuDBhandle$UUID, "' AND ", 
                                                         "session = '", sn, "' AND ", 
                                                         "bundle = '", bn, "'"))
        if(nrow(qRes) > 0){
          build.redundant.links.all(database = dbObj,sessionName=sn,bundleName=bn)
        }
        
        calculate.postions.of.links(emuDBhandle$UUID)
        
      }
      
      # update pb
      progress = progress + 1
      if(verbose){
        setTxtProgressBar(pb, progress)
      }
      
    }
    
    
  }
  
  if(verbose){
    cat("\n")
  }
  
  # remove bundles from cache that where not found
  for(i in 1:nrow(curBndls)){
    if(!curBndls[i,]$found){
      hackBndl = list(name = curBndls[i,]$name, session = curBndls[i,]$session)
      .remove.bundle.annot.DBI(dbUUID=emuDBhandle$UUID,bundle=hackBndl)
      # delete bundle entry
      dbGetQuery(emuDBhandle$connection, paste0("DELETE FROM bundle WHERE db_uuid = '",emuDBhandle$UUID,"' ",
                                                "AND session = '",curBndls[i,]$session, "' ", 
                                                "AND name = '",curBndls[i,]$name,"'"))
    }
  }
  
  # remove session from cache that where not found
  for(i in 1:nrow(curSes)){
    if(!curSes[i,]$found){
      # delete session entry
      dbGetQuery(emuDBhandle$connection, paste0("DELETE FROM session WHERE db_uuid = '",emuDBhandle$UUID,"' ",
                                                "AND name = '",curSes[i,]$name, "' "))
      
    }
  }
}

# FOR DEVELOPMENT 
# library('testthat') 
# test_file('tests/testthat/test_aaa_initData.R')
# test_file('tests/testthat/test_database.caching.R')
