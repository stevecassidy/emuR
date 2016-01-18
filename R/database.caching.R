## Update cache of emuDB
## 
## Updates sqlite cache of loaded emuDB. This can be used
## to update changes to precached/loaded DBs as it only updates the deltas 
## in the cache which is considerably faster than reloading and therefore 
## recacheing the entire DB. This function is now called by load_emuDB if 
## load_emuDB finds a preexisting cache.
## @param emuDBhandle 
## @param verbose display infos
update_cache <- function(emuDBhandle, verbose = TRUE){
  
  DBconfig = load_DBconfig(emuDBhandle)
  
  # list sessions & bundles
  sessions = list_sessions(emuDBhandle)
  bundles = list_bundles(emuDBhandle)
  notUpdatedSessionDBI = list_sessionsDBI(emuDBhandle)
  notUpdatedBundlesDBI = list_bundlesDBI(emuDBhandle)
  
  # add column to sessions to track if already stored
  sessions$stored = F
  
  progress = 0
  
  if(verbose){
    cat("INFO: Checking if cache needs update for ", nrow(curBndls), " bundles...\n")
    pb = txtProgressBar(min = 0, max = nrow(curBndls), initial = progress, style=3)
    setTxtProgressBar(pb, progress)
  }
  
  for(bndlIdx in 1:nrow(bundles)){
    sessionsDBI = list_sessionsDBI(emuDBhandle)
    
    bndl = bundles[bndlIdx,]
    
    # check if session needs to be added
    if(!bndl$session %in% sessionsDBI$name){
      add_sessionDBI(emuDBhandle, bndl$session)
    }
    
    # construct path to annotJSON
    annotFilePath = normalizePath(file.path(emuDBhandle$basePath, paste0(bndl$session, session.suffix), 
                                            paste0(bndl$name, bundle.dir.suffix), 
                                            paste0(bndl$name, bundle.annotation.suffix, '.json')))
    
    # calculate new MD5 sum of bundle annotJSON
    newMD5annotJSON = md5sum(annotFilePath)
    names(newMD5annotJSON) = NULL
    
    # get old MD5 sum (NOTE: this returns an empty string if the bundle isn't present)
    oldMD5annotJSON = get_MD5annotJsonDBI(emuDBhandle, bndl$session, bndl$name)
    
    if(newMD5annotJSON != oldMD5annotJSON){
      # read annotJSON as charac 
      annotJSONchar = readChar(annotFilePath, file.info(annotFilePath)$size)
      
      # convert to bundleAnnotDFs
      bundleAnnotDFs = annotJSONcharToBundleAnnotDFs(annotJSONchar)
      
      if(length(oldMD5annotJSON) == 0){
        # add to bundle table
        add_bundleDBI(emuDBhandle, bndl$session, bndl$name, bundleAnnotDFs$annotates, bundleAnnotDFs$sampleRate, newMD5annotJSON)
      }else{
        # update bundle entry by
        # removing old bundle entry
        remove_bundleDBI(emuDBhandle, bndl$session, bndl$name)
        # and adding to bundle table
        add_bundleDBI(emuDBhandle, bndl$session, bndl$name, bundleAnnotDFs$annotates, bundleAnnotDFs$sampleRate, newMD5annotJSON)
        # and remove bundleAnnotDBI
        remove_bundleAnnotDBI(emuDBhandle, bndl$session, bndl$name)
      }
      # add to items, links, labels tables
      store_bundleAnnotDFsDBI(emuDBhandle, bundleAnnotDFs, bndl$session, bndl$name)
      
      # build redundat links and calc positions
      build_allRedundantLinks(dbHandle, bndl$session, bndl$name)
      calculate_postionsOfLinks(dbHandle)
    }
    
    # increase progress bar  
    progress=progress+1L
    if(verbose){
      setTxtProgressBar(pb,progress)
    }
  }
  
  # remove superfluous sessions from session table
  superfluousSessions = anti_join(notUpdatedSessionDBI, sessions, by = "name")
  if(nrow(superfluousSessions) > 0){
    for(sesIdx in 1:nrow(superfluousSessions)){
      remove_sessionDBI(emuDBhandle, superfluousSessions[sesIdx,]$name)
    }
  }
  # remove superfluous bundles from bundle table and bundleAnnotDBI values from items, labels and links tables
  superfluousBundles = anti_join(notUpdatedBundlesDBI, bundles, by = c("session", "name"))
  if(nrow(superfluousBundles) > 0){
    for(bndlIdx in 1:nrow(superfluousBundles)){
      remove_bundleDBI(emuDBhandle, superfluousBundles[bndlIdx,]$session, superfluousBundles[bndlIdx,]$name)
      remove_bundleAnnotDBI(emuDBhandle, superfluousBundles[bndlIdx,]$session, superfluousBundles[bndlIdx,]$name)
    }
  }
  
  #   for(s in sesPaths){
  #     sn = gsub(session.suffix,"", s)
  #     bndlPattern=paste0('.*', bundle.dir.suffix, '$')
  #     bndlPaths=list.files(path=file.path(emuDBhandle$basePath, s), bndlPattern)
  #     
  #     if(!sn %in% curSes$name){
  #       store_sessionDBI(emuDBhandle, sn)
  #     }
  #     
  #     # mark as found
  #     curSes$found[curSes$name == sn] = T
  #     
  #     for(b in bndlPaths){
  #       
  #       bn = gsub(bundle.dir.suffix,"", b)
  #       # mark as found
  #       curBndls$found[curBndls$session == sn & curBndls$name == bn] = T
  #       
  #       # calc. new md5 sum
  #       annotPath = file.path(emuDBhandle$basePath, s, b, paste0(bn, bundle.annotation.suffix, ".json"))
  #       new.MD5annotJSON = md5sum(normalizePath(annotPath))
  #       
  #       old.MD5annotJSON = dbGetQuery(emuDBhandle$connection, paste0("SELECT MD5annotJSON FROM bundle WHERE ",
  #                                                                    "db_uuid='", emuDBhandle$UUID, "' AND ",
  #                                                                    "session='", sn, "' AND ",
  #                                                                    "name='", bn, "'"))$MD5annotJSON
  #       
  #       # set to empty string if NA or empty character()
  #       if(length(old.MD5annotJSON) == 0){
  #         old.MD5annotJSON = ""
  #       }
  #       if(is.na(old.MD5annotJSON)){
  #         old.MD5annotJSON = ""
  #       }
  #       
  #       
  #       if(old.MD5annotJSON != new.MD5annotJSON){
  #         if(verbose){
  #           print(paste0("Reloading _annot.json for bundle in session : '", sn,"' with name: '", bn, "'"))
  #         }
  #         bundle=jsonlite::fromJSON(annotPath, simplifyVector=FALSE)
  #         mediaFilePath=file.path(file.path(emuDBhandle$basePath, s, b),bundle[['annotates']])
  # 
  #         # check if bundle entry doesn't exists
  #         if(!any(curBndls$session == sn & curBndls$name == bn)){
  #           sR = attr(read.AsspDataObj(mediaFilePath), "sampleRate")
  #           dbGetQuery(emuDBhandle$connection, paste0("INSERT INTO bundle (db_uuid,session,name,annotates,sampleRate,MD5annotJSON) VALUES ('",
  #                                                     emuDBhandle$UUID,"', '", 
  #                                                     sn,"', '", 
  #                                                     bn,"', '", 
  #                                                     bundle$annotates,"', '", 
  #                                                     sR,"', '", 
  #                                                     new.MD5annotJSON,"')"))
  #         }else{
  #           # update MD5 value of DBI model in bundle table
  #           dbGetQuery(emuDBhandle$connection, paste0("UPDATE bundle SET MD5annotJSON = '", new.MD5annotJSON , "' ",
  #                                                     "WHERE db_uuid = '", emuDBhandle$UUID, "' AND ",
  #                                                     "      session = '", sn, "' AND ",
  #                                                     "      name = '", bn, "'"))
  #         }
  #         
  #         # delete old items/label/links entries
  #         remove_bundleAnnotDBI(emuDBhandle, bundleName = bn , sessionName = sn)
  #         # and store them
  #         store_bundleDBI(emuDBhandle, sessionName = sn, bundleName = bn)
  #         
  #         # only build redunant links if non-empty bundle
  #         qRes = dbGetQuery(emuDBhandle$connection, paste0("SELECT * FROM items WHERE ",
  #                                                          "db_uuid = '", emuDBhandle$UUID, "' AND ", 
  #                                                          "session = '", sn, "' AND ", 
  #                                                          "bundle = '", bn, "'"))
  #         if(nrow(qRes) > 0){
  #           build.redundant.links.all(database = dbObj,sessionName=sn,bundleName=bn)
  #         }
  #         
  #         calculate.postions.of.links(emuDBhandle$UUID)
  #         
  #       }
  #       
  #       # update pb
  #       progress = progress + 1
  #       if(verbose){
  #         setTxtProgressBar(pb, progress)
  #       }
  #       
  #     }
  #     
  #     
  #   }
  #   
  #   if(verbose){
  #     cat("\n")
  #   }
  #   
  #   # remove bundles from cache that where not found
  #   for(i in 1:nrow(curBndls)){
  #     if(!curBndls[i,]$found){
  #       hackBndl = list(name = curBndls[i,]$name, session = curBndls[i,]$session)
  #       .remove.bundle.annot.DBI(dbUUID=emuDBhandle$UUID,bundle=hackBndl)
  #       # delete bundle entry
  #       dbGetQuery(emuDBhandle$connection, paste0("DELETE FROM bundle WHERE db_uuid = '",emuDBhandle$UUID,"' ",
  #                                                 "AND session = '",curBndls[i,]$session, "' ", 
  #                                                 "AND name = '",curBndls[i,]$name,"'"))
  #     }
  #   }
  #   
  #   # remove session from cache that where not found
  #   for(i in 1:nrow(curSes)){
  #     if(!curSes[i,]$found){
  #       # delete session entry
  #       dbGetQuery(emuDBhandle$connection, paste0("DELETE FROM session WHERE db_uuid = '",emuDBhandle$UUID,"' ",
  #                                                 "AND name = '",curSes[i,]$name, "' "))
  #       
  #     }
  #   }
}

# FOR DEVELOPMENT 
# library('testthat') 
# test_file('tests/testthat/test_aaa_initData.R')
# test_file('tests/testthat/test_database.caching.R')
