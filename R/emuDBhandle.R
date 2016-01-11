
emuDBhandle = function(dbName, basePath, UUID, connectionPath){

  con <- dbConnect(RSQLite::SQLite(), connectionPath)
  
  if(connectionPath == ":memory:" || file.exists(file.path(basePath, paste0(dbName, database.cache.suffix)))){
    .initialize.DBI.database(con)
  }

  handle = list(dbName = dbName,
                basePath = basePath,
                UUID = UUID,
                connection = con)
  
  class(handle) = "emuDBhandle"
  
  return(handle)
}

##' @export
print.emuDBhandle = function(x, ...){
  print(paste0("<emuDBhandle> (dbName = '", x$dbName, "', basePath = '", x$basePath, "')"))
}

##' Print summary of loaded EMU database (emuDB).
##' @description Gives an overview of an EMU database.
##' Prints database name, UUID, base directory path, session and bundle count and informations about signal track, annotation level, attribute and link definitions.
##' @param dbName name of emuDB
##' @param dbUUID optional UUID of emuDB
##' @export
summary.emuDBhandle = function(object, ...){
  
  uuid=get_UUID(dbName,dbUUID)
  object=.load.emuDB.DBI(uuid)
  cat("Name:\t",object[['name']],"\n")
  cat("UUID:\t",object[['DBconfig']][['UUID']],"\n")
  cat("Directory:\t",object[['basePath']],"\n")
  sesss=.load.sessions.DBI(dbUUID = uuid)
  cat("Session count:",nrow(sesss),"\n")
  
  bndlCnt=.get.bundle.count.DBI(uuid)
  
  cat("Bundle count:",bndlCnt,"\n")
  itCntQ=paste0("SELECT count(*) FROM items WHERE db_uuid='",uuid,"'")
  itCntDf=dbGetQuery(get_emuDBcon(uuid),itCntQ)
  itemCnt=itCntDf[[1]]
  liCntQ=paste0("SELECT count(*) FROM links WHERE db_uuid='",uuid,"'")
  liCntDf=dbGetQuery(get_emuDBcon(uuid),liCntQ)
  linkCnt=liCntDf[[1]]
  cat("Annotation item count: ",itemCnt,", links count: ",linkCnt,"\n")
  cat("\nDatabase configuration:\n\n")
  summary(object[['DBconfig']])
  #cat("SSFF track definitions:\n")
  # TODO 
  
}

##########################
# FOR DEVELOPMENT
#handle = emuDBhandle(dbName = "test12", basePath = "/you/smell/like/poo", UUID = "3412D5E3-E0EA-4E81-9F1C-E0A864D0D403", ":memory:")
# ae1 = load_emuDB("~/Desktop/emuR_demoData/ae", inMemoryCache = T)
# ae2 = load_emuDB("~/Desktop/emuR_demoData/ae", inMemoryCache = F)
# print(summary(ae1))
