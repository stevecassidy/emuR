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

# FOR DEVELOPMENT 
#library('testthat') 
#test_file('tests/testthat/test_database.DBconfig.R')
