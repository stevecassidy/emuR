load.emuDB.DBconfig<-function(DBconfigFilePath){
  # with warn=TRUE and some files
  # R complains about incomplete (last) line
  # See https://stat.ethz.ch/pipermail/r-help/2006-July/108654.html
  # TODO does problem with jsonlite still exist ?
  dbCfgJSONLns=readLines(DBconfigFilePath,warn=FALSE)
  dbCfgJSON=paste(dbCfgJSONLns,collapse='')
  dbCfgPersisted=jsonlite::fromJSON(dbCfgJSON,simplifyVector=FALSE)
  
  # unmarshal schema object (set class names)
  schema=unmarshal.from.persistence(dbCfgPersisted,emuR.persist.class[['DBconfig']])
  return(schema)
}

get.levelDefinition <- function(DBconfig, name){
  res = NULL
  for(ld in DBconfig$levelDefinitions){
    if(ld$name == name){
      res = ld
      break
    }
  }
  return(res)
}

get.ssfftrack.names.used.by.webapp.config<-function(EMUwebAppConfig){
  nms=c()
  for(perspective in EMUwebAppConfig[['perspectives']]){
    scvss=perspective[['signalCanvases']]
    if(!is.null(scvss)){
      for(cvsNm in scvss[['order']]){
        if(cvsNm!='OSCI' & cvsNm!='SPEC'){
          nms=c(nms,cvsNm)
        }
      }
      for(ass in scvss[['assign']]){
        nms=c(nms,ass[['ssffTrackName']])
      }
      
    }
  }
  return(unique(nms))
}

# FOR DEVELOPMENT 
#library('testthat') 
#test_file('tests/testthat/test_database.DBconfig.R')
