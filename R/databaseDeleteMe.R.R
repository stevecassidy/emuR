# file contains commented out functions that used to be in the database.R file


##########################################################
# CRUD like operations for internalVars$sqlConnections

# get_emuDBhandle <- function(dbUUID) {
#   # add in memory connection just to make sure it exists
#   
#   foundHandle = NULL
#   for(c in internalVars$sqlConnections){
#       #res = dbGetQuery(c$con, "SELECT uuid FROM emuDB")
#       #if(dbUUID %in% res$uuid){
#       if(c$dbUUID==dbUUID){
#         foundHandle = c
#         break
#       }
#   }
#   ## make sure :memory: connection is always there
#   #if(is.null(dbUUID) & is.null(foundHandle)){
#   #  con = dbConnect(RSQLite::SQLite(), ":memory:")
#   #  add_emuDBcon(con)
#   #  foundCon = con
#   #}
#   
#   return(foundHandle)
# }


# get_emuDBcon <- function(dbUUID) {
#   for(c in internalVars$sqlConnections){
#     if(c$dbUUID==dbUUID){
#       return(c$connection)
#     }
#   }
#   return(NULL)
# }

## @param name name of emuDB
## @param basePath base path of emuDB
## @param dbUUID UUID of database
## @param path to SQLiteDB
## @return new or already existing emuDB handle
# add_emuDBhandle <- function(name,basePath, dbUUID,path = NULL){
#   foundHandle = NULL
#   for(h in internalVars$sqlConnections){
#     if(h$dbUUID == dbUUID){
#       foundHandle=h
#       #stop("EmuDB already loaded")
#       return(h)
#     }
#   }
#   
#   # only add if not found to avoid duplicates
#   if(is.null(foundHandle)){
#     
#     if(is.null(path)){
#       # create empty db in memory and initialize tables
#       path=":memory:"
#       initialize=T
#       con = dbConnect(RSQLite::SQLite(), path)
#     }else{
#       initialize=(!file.exists(path))
#       con= dbConnect(RSQLite::SQLite(),path)
#       # by default RSQLite sets file permissions 0022 and ignores the umask
#       # We overwrite the permissions with the umask here
#       # Not yet enabled
#       #Sys.chmod(path,mode='0666',use_umask = TRUE)
#     }
#     if(initialize){
#       .initialize.DBI.database(con)
#     }
#   
#     newHandle=list(name=name,path = path,dbUUID=dbUUID,basePath=basePath,connection = con)
#     internalVars$sqlConnections[[length(internalVars$sqlConnections) + 1]] = newHandle
#     foundHandle = newHandle
#   }
#   return(foundHandle)
# }
# 
# remove_emuDBhandle <- function(dbUUID){
#   for(i in 1:length(internalVars$sqlConnections)){
#     if(internalVars$sqlConnections[[i]]$dbUUID == dbUUID){
#       dbDisconnect(internalVars$sqlConnections[[i]]$connection)
#       internalVars$sqlConnections[[i]] = NULL
#       break
#     }
#   }
# }


# database.DDL.emuDB_track='CREATE TABLE track (
#   db_uuid VARCHAR(36),
#   session TEXT,
#   bundle TEXT,
#   path TEXT,
#   FOREIGN KEY (db_uuid,session,bundle) REFERENCES bundle(db_uuid,session_name,name)
# );'


# .store.bundle.DBI<-function(database,bundle,MD5annotJSON=NULL){
#   dbCfg=database[['DBconfig']]
#   #dbCfgJSON=jsonlite::toJSON(dbCfg,auto_unbox=TRUE,force=TRUE,pretty=TRUE)
#   #dbSqlInsert=paste0("INSERT INTO emuDB(name,databaseDir,DBconfigJSON) VALUES('",dbCfg[['name']],"','",database[['databseDir']],"','",dbCfgJSON,"')")
#   if(is.null(MD5annotJSON)){
#     bSqlInsert=paste0("INSERT INTO bundle(db_uuid,session,name,annotates,sampleRate,MD5annotJSON) VALUES('",dbCfg[['UUID']],"','",bundle[['session']],"','",bundle[['name']],"','",bundle[['annotates']],"',",bundle[['sampleRate']],",'NULL')")
#   }else{
#     bSqlInsert=paste0("INSERT INTO bundle(db_uuid,session,name,annotates,sampleRate,MD5annotJSON) VALUES('",dbCfg[['UUID']],"','",bundle[['session']],"','",bundle[['name']],"','",bundle[['annotates']],"',",bundle[['sampleRate']],",'",MD5annotJSON,"')")
#   }
#   res <- dbSendQuery(get_emuDBcon(dbCfg$UUID),bSqlInsert)
#   dbClearResult(res)
# }

# .get.bundle.count.DBI<-function(dbUUID){
#   bCntQ=paste0("SELECT count(*) FROM bundle WHERE db_uuid='",dbUUID,"'")
#   bCntDf=dbGetQuery(get_emuDBcon(dbUUID),bCntQ)
#   if(length(bCntDf)==0){
#     stop("Could not get bundle count of emuDB ",dbUUID,"\n")
#   }
#   return(bCntDf[[1]])
# }

# .load.bundle.names.DBI<-function(dbUUID,sessionName){
#   bQ=paste0("SELECT name FROM bundle WHERE db_uuid='",dbUUID,"' AND session='",sessionName,"'")
#   bDf=dbGetQuery(get_emuDBcon(dbUUID),bQ)
#   return(bDf[['name']])
# }



# .load.bundle.DBI<-function(dbUUID,sessionName,bundleName){
#   bQ=paste0("SELECT db_uuid, session, name, annotates, sampleRate FROM bundle WHERE db_uuid='",dbUUID,"' AND session='",sessionName,"' AND name='",bundleName,"'")
#   bDf=dbGetQuery(get_emuDBcon(dbUUID),bQ)
#   bDfRows=nrow(bDf)
#   if(bDfRows==0){
#     return(NULL)
#   }else if(bDfRows==1){
#     bList=as.list(bDf)
#     return(bList)
#   }else{
#     stop("Ambigious result for bundle lookup")
#   }
# }


# get.database<-function(uuid=NULL,name=NULL){
#   return(.load.emuDB.DBI(uuid,name))
# }


# ex .load.emuDB.DBI function
# get_DBconfigDBI <- function(emuDBhandle){
#   if(is.null(uuid)){
#     uuid=get_UUID(name)
#   }
#   handle=get_emuDBhandle(uuid)
#   con=handle$connection
#   dbQ=paste0("SELECT * FROM emuDB WHERE uuid='",uuid,"'")
#   dbDf=dbGetQuery(con,dbQ)
#   dbCount=nrow(dbDf)
#   if(dbCount==0){
#     stop("Database not found !\n")
#   }else if (dbCount==1){
#     dbCfgObj=jsonlite::fromJSON(dbDf[['DBconfigJSON']],simplifyVector=FALSE)
#     dbCfg=unmarshal.from.persistence(x=dbCfgObj,classMap = emuR.persist.class.DBconfig)
#     db=create.database(name = dbDf[['name']],basePath = handle$basePath,DBconfig = dbCfg)
#   }else{
#     stop("Found ",dbCount," databases with same name: ",name,". Please specify database UUID!\n")
#   }
#   return(db)
#   
# }


##' Get UUID of emuDB
##' @description Returns UUID if emuDB is loaded, throws error otherwise. For 
##' more information on the emuDB format see \code{vignette(emuDB)}.
##' @param dbName name of emuDB
##' @param dbUUID optional UUID of emuDB
##' @return UUID string
##' @seealso  \code{\link{is.emuDB.loaded}}
##' @import DBI
##' @export
##' @examples 
##' \dontrun{
##' 
##' ##################################
##' # prerequisite: loaded "ae" emuDB 
##' # (see ?load_emuDB for more information)
##' 
##' get_UUID(dbName = "ae")
##' 
##' }
# get_UUID<-function(dbName,dbUUID=NULL){
#   if(is.null(dbUUID)){
#     if(!is.character(dbName)){
#       stop("Parameter dbName must be of type character vector!")
#     }
#     # find handle by name
#     dbCount=0
#     for(h in internalVars$sqlConnections){
#       if(h$name==dbName){
#         dbUUID=h$dbUUID
#         dbCount=dbCount+1;
#       }
#     }
#     if(dbCount==0){
#       stop("Database '",dbName,"' not found !\n")
#     }else if(dbCount>1){
#       stop("Found ",dbCount," databases with same name: ",dbName,". Please use database UUID!\n")
#     }
#   }
#   # check if loaded
#   dbCount=0
#   for(h in internalVars$sqlConnections){
#     if(h$dbUUID==dbUUID){
#       dbCount=dbCount+1
#     }
#   }
#   if(dbCount==0){
#     stop("Database with UUID '",dbUUID,"' not found !\n")
#   }else if (dbCount==1){
#     return(dbUUID)
#   }else{
#     stop("Internal error: Found ",dbCount," databases with same UUID: ",dbUUID,"\n")
#   }
# }

# .destroy.DBI.database<-function(){
#   if(!is.null(get_emuDBcon())){
#     dbDisconnect(get_emuDBcon())
#   }
#   #emuDBs.con<<-NULL
#   # setEmuDBcon(NULL)
#   remove_emuDBcon()
# }



##' List loaded emuDBs
##' @description Lists overview of loaded emuDBs as data.frame table. Listed columns are name,basePath and UUID 
##' @return list of emuDBS as data.frame object
##' @export
# list_emuDBs<-function(){
#   # .initialize.DBI.database()
#   df = data.frame(name = character(),
#                   basePath = character(), 
#                   uuid = character(),
#                   cachePath = character(),
#                   stringsAsFactors = F)
#   for(h in internalVars$sqlConnections){
#     dbs=dbGetQuery(h$connection, "SELECT name,uuid FROM emuDB")
#     if(nrow(dbs) > 0){
#       df = rbind(df, data.frame(name = dbs$name,
#                                 basePath = h$basePath,
#                                 uuid = dbs$uuid,
#                                 cachePath = h$path,
#                                 stringsAsFactors = F))
#     }
#   }
#   return(df)
# }

# .purge.emuDB<-function(dbUUID){
#   dbs=dbGetQuery(get_emuDBcon(dbUUID),paste0("DELETE FROM links WHERE db_uuid='",dbUUID,"'"))
#   dbs=dbGetQuery(get_emuDBcon(dbUUID),paste0("DELETE FROM linksExt WHERE db_uuid='",dbUUID,"'"))
#   dbs=dbGetQuery(get_emuDBcon(dbUUID),paste0("DELETE FROM labels WHERE db_uuid='",dbUUID,"'"))
#   dbs=dbGetQuery(get_emuDBcon(dbUUID),paste0("DELETE FROM items WHERE db_uuid='",dbUUID,"'"))
#   # dbs=dbGetQuery(get_emuDBcon(dbUUID),paste0("DELETE FROM track WHERE db_uuid='",dbUUID,"'"))
#   dbs=dbGetQuery(get_emuDBcon(dbUUID),paste0("DELETE FROM bundle WHERE db_uuid='",dbUUID,"'"))
#   dbs=dbGetQuery(get_emuDBcon(dbUUID),paste0("DELETE FROM session WHERE db_uuid='",dbUUID,"'"))
#   dbs=dbGetQuery(get_emuDBcon(dbUUID),paste0("DELETE FROM emuDB WHERE uuid='",dbUUID,"'"))
#   
# }

##' Purge emuDB
##' @description Purges emuDB from this R session. Does not delete any files of the emuDB.
##' @param dbName name of emuDB to purge
##' @param dbUUID optional UUID of emuDB
##' @param interactive ask user for confirmation
##' @seealso \code{\link{load_emuDB}},\code{\link{purge_all_emuDBs}}
##' @examples
##' \dontrun{
##'   # Purge emuDB 'ae'
##'   purge_emuDB('ae')
##' }
##' @export
# purge_emuDB<-function(dbName,dbUUID=NULL,interactive=TRUE){
#   # .initialize.DBI.database()  
#   dbUUID=get_UUID(dbName,dbUUID)
#   purged=FALSE
#   if(!is.null(dbUUID)){
#     if(interactive){
#       if(missing(dbName)){
#         dbRefName=dbUUID
#       }else{
#         dbRefName=dbName
#       }
#       ans=readline(paste0("Are you sure you want to purge emuDB '",dbRefName,"' from this R session? (y/n)"))
#     }else{
#       ans='y'
#     }
#     if(ans=='y'){
#       dbQ=paste0("SELECT uuid FROM emuDB WHERE uuid='",dbUUID,"'")
#       dbDf = data.frame()
#       for(c in internalVars$sqlConnections){
#         if(c$dbUUID==dbUUID){
#           dbDf=dbGetQuery(c$connection,dbQ)
#           if(nrow(dbDf) != 0){
#             if(c$path == ":memory:"){
#               .purge.emuDB(dbUUID)
#             }
#           }
#           remove_emuDBhandle(c$dbUUID)
#           purged=TRUE
#           break
#         }
#       }
#     }
#   }else{
#     stop("emuDB ",dbName," not found!")
#   }
#   return(purged)
# }

##' Purge all loaded emuDBs
##' @description Purges emuDB from this R session. Does not delete any files of the emuDB.
##' @param interactive ask user for confirmation
##' @export
# purge_all_emuDBs<-function(interactive=TRUE){
#   cleared=FALSE
#   if(interactive){
#     ans=readline('Are you sure you want to remove all databases from this R session? (y/n)')
#   }else{
#     ans='y'
#   }
#   if(ans=='y'){
#     for(c in internalVars$sqlConnections){
#       remove_emuDBhandle(c$dbUUID)
#     }
#     # .destroy.DBI.database()
#     # .initialize.DBI.database()
#     cleared=TRUE
#   }
#   return(cleared)
# }





# create.database <- function(name,basePath=NULL,DBconfig=create.schema.databaseDefinition(name = name),sessions=NULL,primaryExtension=NULL){
#   o <- list(name=name,basePath=basePath,DBconfig=DBconfig,sessions=sessions,primaryExtension=primaryExtension,apiLevel=emuDB.apiLevel)
#   class(o) <- c('emuDB','list')
#   invisible(o)
# }




# ##' Print summary of EMU database (emuDB).
# ##' @description Gives an overview of an EMU database.
# ##' Prints database name, UUID, base directory path, session and bundle count and informations about signal track, annotation level, attribute and link definitions.
# ##' @param dbName name of emuDB
# ##' @param dbUUID optional UUID of emuDB
# ##' @export
# summary_emuDB<-function(dbName,dbUUID=NULL){
#   uuid=get_UUID(dbName,dbUUID)
#   object=.load.emuDB.DBI(uuid)
#   cat("Name:\t",object[['name']],"\n")
#   cat("UUID:\t",object[['DBconfig']][['UUID']],"\n")
#   cat("Directory:\t",object[['basePath']],"\n")
#   sesss=.load.sessions.DBI(dbUUID = uuid)
#   cat("Session count:",nrow(sesss),"\n")
#   
#   bndlCnt=.get.bundle.count.DBI(uuid)
#   
#   cat("Bundle count:",bndlCnt,"\n")
#   itCntQ=paste0("SELECT count(*) FROM items WHERE db_uuid='",uuid,"'")
#   itCntDf=dbGetQuery(get_emuDBcon(uuid),itCntQ)
#   itemCnt=itCntDf[[1]]
#   liCntQ=paste0("SELECT count(*) FROM links WHERE db_uuid='",uuid,"'")
#   liCntDf=dbGetQuery(get_emuDBcon(uuid),liCntQ)
#   linkCnt=liCntDf[[1]]
#   cat("Annotation item count: ",itemCnt,", links count: ",linkCnt,"\n")
#   cat("\nDatabase configuration:\n\n")
#   summary(object[['DBconfig']])
#   #cat("SSFF track definitions:\n")
#   # TODO 
#   
#   
# }
# 
# "as.emuDB"<-function(o,class){
#   if(class=='emuDB'){
#     return(o)
#   }
#   if(class=='list'){
#     class(o) <- c('list')
#     return(o)
#   }
# }


# .update.transient.schema.values<-function(schema){
#   # get max label array size
#   #   maxLbls=0
#   #   for(lvlDef in schema[['levelDefinitions']]){
#   #     attrCnt=length(lvlDef[['attributeDefinitions']])
#   #     if(attrCnt > maxLbls){
#   #       maxLbls=attrCnt
#   #     }
#   #   }
#   schema[['maxNumberOfLabels']]=1L
#   for( ld in schema[['levelDefinitions']]){
#     attrCnt=length(ld[['attributeDefinitions']])
#     if(attrCnt>schema[['maxNumberOfLabels']]){
#       schema[['maxNumberOfLabels']]=attrCnt
#     }
#   }
#   return(schema)
# }
# 
# # persistent filters for bundle annotations
# # Transient properties which are not stored to JSON
# emuR.persist.filters.bundle=list()
# emuR.persist.filters.bundle[[1]]=c('db_uuid')
# emuR.persist.filters.bundle[[2]]=c('files')
# emuR.persist.filters.bundle[[3]]=c('signalpaths')
# emuR.persist.filters.bundle[[4]]=c('mediaFilePath')
# emuR.persist.filters.bundle[[5]]=c('legacyBundleID')
# emuR.persist.filters.bundle[[6]]=c('sessionName')
# emuR.persist.filters.bundle[[7]]=c('levels','*','sampleRate')
# emuR.persist.filters.bundle[[8]]=c('session')


# extractTrackdata <- function(db=NULL,segmentList=NULL,trackName=NULL){
#   schema=db[['DBconfig']]
#   signalExt=NULL
#   for(tr in schema[['tracks']]){
#     if(tr[['name']]==trackName){
#       signalExt=tr[['fileExtension']]
#     }
#   }
#   signalExtPatt=paste0('[.]',signalExt,'$')
#   currentUtt=''
#   currentAsspObj=NULL
#   utts=segmentList[['utts']]
#   
#   index <- matrix(ncol=2, nrow=length(utts))
#   colnames(index) <- c("start","end")
#   
#   ftime <- matrix(ncol=2, nrow=length(utts))
#   colnames(ftime) <- c("start","end")
#   
#   data <- NULL
#   origFreq <- NULL
#   
#   #########################
#   # LOOP OVER UTTS
#   curIndexStart = 1
#   for (i in 1:length(utts)){
#     
#     un=segmentList[['utts']][[i]]
#     if(currentUtt!=un){
#       #cat("Utt: ",un,"\n")
#       u=get.bundle(db,un)
#       for(sp in u[['signalpaths']]){ 
#         if(length(grep(signalExtPatt,sp))==1){
#           #cat("Signal path: ",sp,"\n")
#           currentAsspObj=read.AsspDataObj(sp)
#         }
#       }
#     }
#     # we should have the corresponding (complete) ASSP data obj for the segment here
#     completeData=currentAsspObj[[trackName]] 
#     ncols=ncol(completeData) 
#     if(is.null(data)){
#       data <- matrix(ncol=ncols, nrow=0)
#     }
#     
#     sampleRate=attr(currentAsspObj,"sampleRate")
#     #cat("Cols: ",ncols,"\n")
#     origFreq <- attr(currentAsspObj, "origFreq")
#     
#     curStart <- segmentList[['start']][i]
#     curEnd <- segmentList[['end']][i]
#     
#     fSampleRateInMS <- (1/sampleRate)*1000
#     fStartTime <- attr(currentAsspObj,"startTime")*1000
#     #cat("Seq: ",fStartTime, curEnd, fSampleRateInMS,"\n")
#     timeStampSeq <- seq(fStartTime, curEnd, fSampleRateInMS)
#     ###########################################
#     # search for first item larger than start time
#     breakVal <- -1
#     for (j in 1:length(timeStampSeq)){
#       if (timeStampSeq[j] >= curStart){
#         breakVal <- j
#         break
#       }
#     }
#     curStartDataIdx <- breakVal
#     curEndDataIdx <- length(timeStampSeq)
#     
#     ####################
#     # set index and ftime
#     curIndexEnd <- curIndexStart+curEndDataIdx-curStartDataIdx
#     index[i,] <- c(curIndexStart, curIndexEnd)
#     ftime[i,] <- c(timeStampSeq[curStartDataIdx], timeStampSeq[curEndDataIdx])
#     
#     #############################
#     # calculate size of and create new data matrix
#     #tmpData <- eval(parse(text=paste("curDObj$",colName,sep="")))
#     
#     
#     rowSeq <- seq(timeStampSeq[curStartDataIdx],timeStampSeq[curEndDataIdx], fSampleRateInMS) 
#     curData <- matrix(ncol=ncol(completeData), nrow=length(rowSeq))
#     colnames(curData) <- paste("T", 1:ncol(curData), sep="")
#     rownames(curData) <- rowSeq
#     curData[,] <- completeData[curStartDataIdx:curEndDataIdx,] 
#     
#     ##############################
#     # Append to global data matrix app
#     data <- rbind(data, curData)
#     
#     curIndexStart <- curIndexEnd+1
#     
#     curDObj = NULL
#   }
#   ########################################
#   #convert data, index, ftime to trackdata
#   FileExtAndtrackname=paste0(signalExt,':',trackName)
#   myTrackData <- as.trackdata(data, index=index, ftime, FileExtAndtrackname)
#   
#   if(any(trackName %in% c("dft", "css", "lps", "cep"))){
#     if(!is.null(origFreq)){
#       attr(myTrackData[['data']], "fs") <- seq(0, origFreq/2, length=ncol(myTrackData[['data']]))
#       class(myTrackData[['data']]) <- c(class(myTrackData[['data']]), "spectral")
#     }else{
#       stop("no origFreq entry in spectral data file!")
#     }
#   }
#   
#   #if(!is.null(OnTheFlyFunctionName)){
#   #  close(pb)
#   #}
#   
#   return(myTrackData)
# }




# set.list.names <-function(list,nameProperty){
#   elemNames=c()
#   for(le in list){
#     name=le[[nameProperty]]
#     elemNames=c(elemNames,name)
#   }
#   names(list)<-elemNames
#   return(list)
#   
# }

# apply.class<-function(val,path,class){
#   if(is.null(val)){
#     return(NULL)
#   }
#   if(is.null(path)){
#     class(val)<-c(class,class(val))
#   }else{
#     pLen=length(path)
#     if(pLen==0){
#       class(val)<-c(class,class(val))
#     }else{
#       
#       pathElem=path[1]
#       restpath=c()
#       
#       if(pLen>1){
#         restpath=path[2:pLen]
#       }
#       if(pathElem=='*'){
#         newVal=list()
#         for(ch in val){
#           newVal[[length(newVal)+1]]=apply.class(ch,restpath,class)
#         }
#         val=newVal
#       }else{
#         val[[pathElem]]=apply.class(val[[pathElem]],restpath,class)
#       }
#     }
#   }
#   return(val)
# }


# .store.bundle.annot.DBI<-function(dbUUID,bundle){
#   bName=bundle[['name']]
#   itCnt=0
#   for(lvl in bundle[['levels']]){
#     
#     seqIdx=0L
#     for(it in lvl[['items']]){
#       seqIdx=seqIdx+1L
#       
#       itemId=it[['id']]
#       if(is.null(itemId)){
#         itemId=NA
#       }
#       if(!is.null(bundle[['sampleRate']])){
#         srCol=bundle[['sampleRate']]
#       }else{
#         srCol='NULL'
#       }
#       sp=it[['samplePoint']]
#       if(!is.null(sp)){
#         spCol=as.integer(sp)
#       }else{
#         spCol='NULL'
#       }
#       ss=it[['sampleStart']]
#       if(!is.null(ss)){
#         ssCol=as.integer(ss)
#       }else{
#         ssCol='NULL'
#       }
#       sdur=it[['sampleDur']]
#       if(!is.null(sdur)){
#         sdurCol=sdur
#       }else{
#         sdurCol='NULL'
#       }
#       
#       sqlInsert=paste0("INSERT INTO items(db_uuid,session,bundle,itemID,level,type,seqIdx,sampleRate,samplePoint,sampleStart,sampleDur) VALUES('",dbUUID,"','",bundle[['session']],"','",bName,"',",itemId,",'",lvl[['name']],"','",lvl[['type']],"',",seqIdx,",",bundle[['sampleRate']],",",spCol,",",ssCol,",",sdurCol,")")
#       #cat('SQL:',sqlInsert,"\n")
#       res<-dbSendQuery(get_emuDBcon(dbUUID),sqlInsert)
#       dbClearResult(res)
#       itCnt=itCnt+1
#       
#       lbls=it[['labels']]
#       lblsLen=length(lbls)
#       for(i in 1:lblsLen){
#         rLbl=NA
#         #if(lblsLen>=i){
#         lbl=lbls[[i]]
#         if(!is.null(lbl)){
#           rLbl=lbl[['value']]
#           sqlEscapedLbl=str_replace_all(rLbl,"'","''")
#           #sqlInsert=paste0("INSERT INTO labels VALUES('",dbUUID,"','",bundle[['session']],"','",bName,"',",itemId,",",i-1L,",'",lbl[['name']],"',\"",rLbl,"\")")
#           sqlInsert=paste0("INSERT INTO labels VALUES('",dbUUID,"','",bundle[['session']],"','",bName,"',",itemId,",",i-1L,",'",lbl[['name']],"','",sqlEscapedLbl,"')")
#           #cat('SQL:',sqlInsert,"\n")
#           res<-dbSendQuery(get_emuDBcon(dbUUID),sqlInsert)
#           dbClearResult(res)
#         }
#         #}
#       } 
#       
#     }
#   }
#   
#   for(lk in bundle[['links']]){
#     lbl=lk[['label']]
#     if(is.null(lbl)){
#       lblCol='NULL'
#     }else{
#       lblCol=paste0("'",lbl,"'")
#     }
#     
#     sqlInsert=paste0("INSERT INTO links(db_uuid,session,bundle,fromID,toID,label) VALUES('",dbUUID,"','",bundle[['session']],"','",bName,"',",lk[['fromID']],",",lk[['toID']],",",lblCol,")")
#     
#     #cat('SQL:',sqlInsert,"\n")
#     res<-dbSendQuery(get_emuDBcon(dbUUID),sqlInsert)
#     dbClearResult(res)
#     
#     
#   }
#   
# }

# .load.bundle.levels.s3 <-function(dbUUID,sessionName,bundleName){
#   itCnt=0
#   db=.load.emuDB.DBI(dbUUID)
#   levelDefinitions=db[['DBconfig']][['levelDefinitions']]
#   find.levelDefinition<-function(name){
#     for(lvlDef in levelDefinitions){
#       if(name == lvlDef[['name']]){
#         return(lvlDef)
#       }
#     }
#   }
#   
#   # create all levels
#   levels=list()
#   lblsQ=paste0("SELECT * FROM labels WHERE db_uuid='",dbUUID,"' AND session='",sessionName,"' AND bundle='",bundleName,"'")
#   bundleLabels=dbGetQuery(get_emuDBcon(dbUUID),lblsQ)
#   
#   for(ld in levelDefinitions){
#     lvlNm=ld[['name']]
#     levels[[lvlNm]]=create.bundle.level(name=ld[['name']],type=ld[['type']])
#     
#     itsQ=paste0("SELECT * FROM items WHERE db_uuid='",dbUUID,"' AND session='",sessionName,"' AND bundle='",bundleName,"' AND level='",lvlNm,"' ORDER BY seqIdx")
#     itemsOfLevel=dbGetQuery(get_emuDBcon(dbUUID),itsQ)
#     nrows=nrow(itemsOfLevel)
#     
#     if(nrows>0){
#       for(r in 1:nrows){
#         
#         sr=NULL
#         srDf=itemsOfLevel[r,'sampleRate']
#         if(!is.na(srDf)){
#           sr=srDf
#         }
#         lvl=levels[[lvlNm]]
#         if(lvl[['type']]!=itemsOfLevel[r,'type']){
#           stop("Wrong item type ",itemsOfLevel[r,'type']," for level ",lvlNm," type ",lvl[['type']]," in bundle: ",sessionName,":",bundleName,"\n")
#         }
#         
#         levels[[lvlNm]][['sampleRate']]=sr
#         if(is.null(levels[[lvlNm]][['items']])){
#           levels[[lvlNm]][['items']]=list()
#         }
#         
#         id=itemsOfLevel[r,'itemID']
#         type=itemsOfLevel[r,'type']
#         
#         attrDefs=ld[['attributeDefinitions']]
#         attrDefsLen=length(attrDefs)
#         
#         #gid=items[r,'id']
#         #itemLabelSelector=bundleLabels[['itemID']]==gid
#         itemLabelSelector=bundleLabels[['itemID']]==id
#         labelRows=bundleLabels[itemLabelSelector,]
#         nLabelRows=nrow(labelRows)
#         labels=list()
#         for(j in 1:nLabelRows){
#           lblNm=labelRows[j,'name']
#           labels[[j]]=list(name=lblNm,value=labelRows[j,'label'])
#         }
#         
#         if(type=='SEGMENT'){
#           levels[[lvlNm]][['items']][[length(levels[[lvlNm]][['items']])+1L]]=create.interval.item(id=id,sampleStart=itemsOfLevel[r,'sampleStart'],sampleDur=itemsOfLevel[r,'sampleDur'],labels=labels)
#         }else if(type=='EVENT'){
#           levels[[lvlNm]][['items']][[length(levels[[lvlNm]][['items']])+1L]]=create.event.item(id=id,samplePoint=itemsOfLevel[r,'samplePoint'],labels=labels)
#         }else{
#           levels[[lvlNm]][['items']][[length(levels[[lvlNm]][['items']])+1L]]=create.item(id=id,labels=labels)  
#         }
#       }
#     }
#   }
#   return(levels)
# }

# .load.bundle.links.s3 <-function(dbUUID,sessionName,bundleName){
#   lksQ=paste0("SELECT * FROM links WHERE db_uuid='",dbUUID,"' AND session='",sessionName,"' AND bundle='",bundleName,"'")
#   linksDf=dbGetQuery(get_emuDBcon(dbUUID),lksQ)
#   nrows=nrow(linksDf)
#   
#   #links=vector(mode='list',length=nrows)
#   links=list()
#   if(nrows>0){
#     for(row in 1:nrows){
#       link=list()
#       link[['fromID']]=linksDf[row,'fromID']
#       
#       link[['toID']]=linksDf[row,'toID']
#       lbl=linksDf[row,'label']
#       if(!is.null(lbl) && !is.na(lbl)){
#         link[['label']]=lbl
#       }
#       links[[row]]=link
#       
#     }
#   }
#   return(links)
# }




# convert.bundle.links.to.data.frame <-function(links){
#   df=data.frame(stringsAsFactors=FALSE)
#   row=0
#   for(lk in links){
#     row=row+1L
#     df[row,'fromID']=lk[['fromID']]
#     df[row,'toID']=lk[['toID']]
#     lbl=lk[['label']]
#     if(!is.null(lbl)){
#       df[row,'label']=lbl
#     }
#   }
#   invisible(df)
# }



# ## Returns bundle as S3 object
# ## 
# ## @param db database
# ## @param sessionName sessionName
# ## @param bundleName name of bundle
# ## @return bundle in S3 format
# ## @author Klaus Jaensch
# ## @keywords emuDB database schema Emu bundle
# ## 
# get.bundle <- function(dbName=NULL,sessionName,bundleName,dbUUID=NULL){
#   
#   dbUUID=get_UUID(dbName,dbUUID)
#   b=.load.bundle.DBI(dbUUID,sessionName,bundleName)
#   if(is.null(b)){
#     return(b)
#   }
#   b[['levels']]=.load.bundle.levels.s3(dbUUID,sessionName,bundleName)
#   b[['links']]=.load.bundle.links.s3(dbUUID,sessionName,bundleName)
#   return(as.bundle(b))
# }

# get.bundle.stub<-function(db,bundleName){
#   sessCount=length(db[['sessions']])
#   bundleStub=NULL
#   for(s in 1:sessCount){
#     sbNms=names(db[['sessions']][[s]][['bundles']])
#     if(bundleName %in% sbNms){
#       # this session contains requested bundle
#       bundleStub=db[['sessions']][[s]][['bundles']][[bundleName]]
#       break
#     }
#   }
#   return(bundleStub)
# }


# emuDB.session <- function(name,path=NULL,bundles=list){
#   o <- list(name=name,path=path,bundles=bundles)
#   class(o) <- 'emuDB.session'
#   invisible(o)
# }

# ##' Test if EMU database is loaded
# ##' @param dbName name of emuDB
# ##' @param dbUUID optional UUID of EmuDB
# ##' @return TRUE if loaded, FALSE otherwise
# ##' @author Klaus Jaensch
# ##' @seealso \code{\link{load_emuDB}}
# ##' @keywords emuDB database Emu
# ##' @examples
# ##' \dontrun{
# ##' ## Test if database 'ae' is loaded
# ##' 
# ##'   is.emuDB.loaded('ae')
# ##' }
# is.emuDB.loaded<-function(dbName,dbUUID=NULL){
#   # .initialize.DBI.database()
#   if(is.null(dbUUID)){
#     q=paste0("SELECT * FROM emuDB WHERE name='",dbName,"'")
#   }else{
#     q=paste0("SELECT * FROM emuDB WHERE uuid='",dbUUID,"'")
#   }
#   dbsDf = data.frame()
#   for(c in internalVars$sqlConnections){
#     dbsDf=dbGetQuery(c$connection,q)
#     if(nrow(dbsDf)>0){
#       break
#     }
#   }
#   
#   return((nrow(dbsDf)>0))
# }



# # Create emuDB bundle object
# # @description A bundle typically contains media files and annotations of an utterance
# # @param name name of the bundle
# # @param sessionName session ID of the bundle
# # @param legacyBundleID legacy bundle ID
# # @param annotates annotated signal file
# # @param sampleRate sample rate
# # @param signalpaths pathes of signal files (legacy only)
# # @param mediaFilePath path pattern of samples track (legacy only)
# # @param levels list of annotation levels
# # @param links list of links containing the hierarchical information of the annotation levels
# # @return object of class emuDB.bundle
# # @author Klaus Jaensch
# # @keywords emuDB bundle Emu
# # 
# create.bundle <- function(name,sessionName=NULL,legacyBundleID=NULL,annotates=NULL,sampleRate,signalpaths=list(),mediaFilePath=NULL,levels=list(),links=list()){
#   o <- list(name=name,sessionName=sessionName,legacyBundleID=legacyBundleID,annotates=annotates,sampleRate=sampleRate,signalpaths=signalpaths,mediaFilePath=mediaFilePath,files=signalpaths,levels=levels,links=links)
#   return(as.bundle(o))
# }

# as.bundle <- function(bundleData){
#   class(bundleData) <- 'emuDB.bundle'
#   attr(bundleData,'ips.persist')<-list(typesJSON=list(levels='array'))
#   invisible(bundleData)
# }

# # Get media file full path
# # @param database database object
# # @param bundle bundle object
# # @return full path of media file
# # @author Klaus Jaensch
# get_media_file_path<-function(database,bundle){
#   basePath=database['basePath']
#   mfp=NULL
#   mfp=file.path(basePath, paste0(bundle$session, session.suffix), paste0(bundle$name, bundle.dir.suffix), bundle$annotates)
#   return(mfp)
# }
# 
# # Get track file path either by extnsion or by name
# # @param database database object
# # @param bundle bundle object
# # @param sffTrackExt track extension
# # @param ssffTrackName track name
# # @return full path to track file or null if the track is not defined
# # @author Klaus Jaensch
# get_ssfftrack_file_path<-function(database,bundle,ssffTrackExt=NULL,ssffTrackName=NULL){
#   basePath=database['basePath']
#   sp=NULL
#   if(!is.null(ssffTrackExt)){
#     for(ssffTrackDef in database[['DBconfig']][['ssffTrackDefinitions']]){
#       ssffTrackDefExt=ssffTrackDef[['fileExtension']]
#       if(ssffTrackExt==ssffTrackDefExt){
#         sp=file.path(basePath, paste0(bundle$session, session.suffix), paste0(bundle$name, bundle.dir.suffix), paste0(bundle$name, ".", ssffTrackExt))
#       }
#     }
#   }else if(!is.null(ssffTrackName)){
#     for(ssffTrackDef in database[['DBconfig']][['ssffTrackDefinitions']]){
#       ssffTrackDefName=ssffTrackDef[['name']]
#       if(ssffTrackName==ssffTrackDefName){
#         ssffTrackExt=ssffTrackDef[['fileExtension']]
#         sp=file.path(basePath, paste0(bundle$session, session.suffix), paste0(bundle$name, bundle.dir.suffix), paste0(bundle$name, ".", ssffTrackExt))
#       }
#     }
#   }else{
#     stop("Either track extension or track name must be given.")
#   }
#   return(sp)
# }

# get.level.name.for.attribute<-function(dbConfig,attributeName){
#   for(ld in dbConfig[['levelDefinitions']]){
#     for(ad in ld[['attributeDefinitions']]){
#       if(ad[['name']]==attributeName){
#         return(ld[['name']])
#       }
#     }
#   }
#   return(NULL)
# }
# 
# 
# is.relative.file.path<-function(nativeFilePathStr,forRunningPlatform=FALSE){
#   if(forRunningPlatform){
#     if(.Platform[['OS.type']]=='unix'){
#       if(.Platform[['file.sep']]==substr(nativeFilePathStr,1,1)){
#         # UNIX: "/dir/file"
#         # absolute path
#         return(FALSE)
#       }
#     }else if(.Platform[['OS.type']]=='windows'){
#       #See http://msdn.microsoft.com/en-us/library/windows/desktop/aa365247%28v=vs.85%29.aspx
#       if(substr(nativeFilePathStr,1,2)=='\\'){
#         # fully qualified MS UNC path (is this supported with R?): \\samba\bla
#         return(FALSE)
#       }else if(grepl('^[A-Z,a-z][:]',nativeFilePathStr)){
#         # fully qualified drive path: C:\Users\bla
#         return(FALSE)
#       }else if(.Platform[['file.sep']]==substr(nativeFilePathStr,1,1)){
#         # Windows: "\dir\file"
#         # absolute path
#         return(FALSE)
#       }
#     }
#   }else{
#     if(grepl('^[A-Z,a-z][:]',nativeFilePathStr)){
#       return(FALSE)
#     }
#     if(grepl('^[\\]',nativeFilePathStr)){
#       return(FALSE)
#     }
#     if(grepl('^/',nativeFilePathStr)){
#       return(FALSE)
#     }
#     
#   }
#   return(TRUE)
# }
# # Add level definition to EMU database
# # 
# # @param db EMU database object
# # @param levelDefinition
# # @author Klaus Jaensch
# # @keywords emuDB database schema Emu 
# add_levelDefinition_object<-function(dbName=NULL,levelDefinition,dbUUID=NULL){
#   db=.load.emuDB.DBI(uuid = dbUUID,name=dbName)
#   # check if level definition (name) already exists 
#   for(ld in db[['DBconfig']][['levelDefinitions']]){
#     if(ld[['name']]==levelDefinition[['name']]){
#       stop("Level definition:",levelDefinition[['name']]," already exists in database ",db[['name']])
#     }
#   }
#   # add
#   db[['DBconfig']][['levelDefinitions']][[length(db[['DBconfig']][['levelDefinitions']])+1]]=levelDefinition
#   
#   # update transient values
#   db[['DBconfig']]=.update.transient.schema.values(db[['DBconfig']])
#   
#   # store to disk
#   .store.schema(db)
#   invisible(NULL)
# }
# 
# 
# add.linkDefinition<-function(dbName,linkDefinition,dbUUID=NULL){
#   db=.load.emuDB.DBI(name=dbName,uuid = dbUUID)
#   # check existence of levels
#   
#   superFound=FALSE
#   for(ld in db[['DBconfig']][['levelDefinitions']]){
#     if(ld[['name']]==linkDefinition[['superlevelName']]){
#       superFound=TRUE
#     }
#   }
#   if(!superFound){
#     stop("Super level ",linkDefinition[['superlevelName']]," not found!")
#   }
#   
#   subFound=FALSE
#   for(ld in db[['DBconfig']][['levelDefinitions']]){
#     if(ld[['name']]==linkDefinition[['sublevelName']]){
#       subFound=TRUE
#     }
#     
#   }
#   if(!subFound){
#     stop("Sub level ",linkDefinition[['sublevelName']]," not found!")
#   }
#   
#   # check if link definition already exists 
#   for(lkd in db[['DBconfig']][['linkDefinitions']]){
#     if(lkd[['superlevelName']]==linkDefinition[['superlevelName']] & lkd[['sublevelName']]==linkDefinition[['sublevelName']]){
#       stop("Link definition:",lkd," already exists in database ",db[['name']])
#     }
#   }
#   
#   # add
#   db[['DBconfig']][['linkDefinitions']][[length(db[['DBconfig']][['linkDefinitions']])+1]]=linkDefinition
#   
#   # store 
#   .store.schema(db)
#   return(invisible(NULL))
# }
# 
# 
# remove.linkDefinition<-function(dbName,linkDefinitionSuperlevelName,linkDefinitionSublevelName,dbUUID=NULL){
#   db=.load.emuDB.DBI(uuid = dbUUID,name=dbName)
#   uuid=db[['DBconfig']][['UUID']]
#   # check if link definition exists 
#   exists=FALSE
#   for(lkd in db[['DBconfig']][['linkDefinitions']]){
#     if(lkd[['superlevelName']]== linkDefinitionSuperlevelName& lkd[['sublevelName']]==linkDefinitionSublevelName){
#       exists=TRUE
#       break
#     }
#   }
#   if(!exists){
#     stop("Link definition ",linkDefinitionSuperlevelName," -> ",linkDefinitionSublevelName," not found!")
#   }
#   
#   # check if links exist
#   lksDf=dbGetQuery(get_emuDBcon(uuid),paste0("SELECT * FROM links l,items f,items t WHERE \
#                         l.db_uuid='",uuid,"' AND f.db_uuid=l.db_uuid AND t.db_uuid=l.db_uuid AND \
#                         l.fromID=f.itemID AND l.toID=t.itemID AND \
#                         f.level='",linkDefinitionSuperlevelName,"' AND t.level='",linkDefinitionSublevelName,"'"))
#   lksCnt=nrow(lksDf)
#   if(lksCnt>0){
#     stop("There are ",lksCnt," links for this link definitons. Remove these links first to delete link definition")
#   }
#   
#   # do removal
#   newLkDefs=list()
#   for(lkDef in db[['DBconfig']][['linkDefinitions']]){
#     if(!(lkd[['superlevelName']]== linkDefinitionSuperlevelName& lkd[['sublevelName']]==linkDefinitionSublevelName)){
#       newLkDefs[[length(newLkDefs)+1]]=lkDef
#     }
#   }
#   db[['DBconfig']][['linkDefinitions']]=newLkDefs
#   
#   # update transient values
#   db[['DBconfig']]=.update.transient.schema.values(db[['DBconfig']])
#   
#   # store to disk
#   .store.schema(db)
#   
#   return(invisible(NULL))
# }

# bundle.iterator<-function(db,apply){
#   for(s in db[['sessions']]){
#     sessionName=s[['name']]
#     for(b in s[['bundles']]){
#       db=apply(db,b)
#     }
#   }
#   return(db)
# }


# 
# 
# 
# 
# ## Store EMU database bundle to file
# ## 
# ## @param db EMU database (in R workspace)
# ## @param bundle the bundle to save
# ## @import jsonlite
# ## @keywords emuDB database Emu 
# ## 
# store.bundle.annotation <- function(dbName=NULL,bundle,dbUUID=NULL){
#   db=get.database(uuid = dbUUID,name=dbName)
#   dbUUID=db[['DBconfig']][['UUID']]
#   dbDir=db[['basePath']]
#   # check target dir
#   if(!file.exists(dbDir)){
#     stop(dbDir," does not exist!")
#   }
#   sessionName=bundle[['session']]
#   bName=bundle[['name']]
#   initialize_emuDbDBI(get_emuDBcon(dbUUID), createTables=FALSE)
#   .remove.bundle.annot.DBI(dbUUID=dbUUID,bundle=bundle)
#   .store.bundle.annot.DBI(dbUUID=dbUUID,bundle=bundle)
#   # only build redunant links if non-empty bundle
#   qRes = dbGetQuery(get_emuDBcon(dbUUID), paste0("SELECT * FROM items WHERE ",
#                                                  "db_uuid = '", dbUUID, "' AND ", 
#                                                  "session = '", sessionName, "' AND ", 
#                                                  "bundle = '", bName, "'"))
#   if(nrow(qRes) > 0){
#     build.redundant.links.all(database = db,sessionName=sessionName,bundleName=bName)
#   }
#   #build.redundant.links.all(database = db)
#   #res<-dbSendQuery(get_emuDBcon(),'DELETE FROM linksExt')
#   #dbClearResult(res)
#   #linksTmp=dbReadTable(get_emuDBcon(),'linksTmp')
#   calculate.postions.of.links(dbUUID)
#   
#   # persist to filesystem
#   # TODO error handling
#   sessDirNm=paste0(sessionName,session.suffix)
#   sessPth=file.path(dbDir,sessDirNm)
#   
#   bDirNm=paste0(bName,bundle.dir.suffix)
#   bndlPth=file.path(sessPth,bDirNm)
#   
#   bndlFileNm=paste0(bName,bundle.annotation.suffix,'.json')
#   bndFilePth=file.path(bndlPth,bndlFileNm)
#   pFilter=emuR.persist.filters.bundle
#   bp=marshal.for.persistence(bundle,pFilter)
#   pbpJSON=jsonlite::toJSON(bp,auto_unbox=TRUE,force=TRUE,pretty=TRUE)
#   writeLines(pbpJSON,bndFilePth)
#   return(bundle)
# }

