require(stringr)
require(uuid)
require(wrassp)
require(DBI)
#require(data.table)


# constants

# API level of database object format
# increment this value if the internal database object format changes  
emuDB.apiLevel=3L

session.suffix='_ses'
bundle.dir.suffix='_bndl'
bundle.annotation.suffix='_annot'
database.schema.suffix='_DBconfig.json'

# global database connection
#emuDBs.con=NULL

database.DDL.emuDB='CREATE TABLE emuDB (
  uuid VARCHAR(36) NOT NULL,
  name TEXT,
  basePath TEXT,
  DBconfigJSON TEXT,
  PRIMARY KEY (uuid)
);'

database.DDL.emuDB_session='CREATE TABLE session (
  db_uuid VARCHAR(36),
  name TEXT,
  PRIMARY KEY (db_uuid,name),
  FOREIGN KEY (db_uuid) REFERENCES emuDB(uuid)
);'

database.DDL.emuDB_track='CREATE TABLE track (
  db_uuid VARCHAR(36),
  session TEXT,
  bundle TEXT,
  path TEXT,
  FOREIGN KEY (db_uuid,session,bundle) REFERENCES bundle(db_uuid,session_name,name)
);'

database.DDL.emuDB_bundle='CREATE TABLE bundle (
  db_uuid VARCHAR(36),
  session TEXT,
  name TEXT,
  annotates TEXT,
  sampleRate FLOAT,
  mediaFilePath TEXT,
  PRIMARY KEY (db_uuid,session,name),
  FOREIGN KEY (db_uuid,session) REFERENCES session(db_uuid,name)
);'

database.DDL.emuDB_items='CREATE TABLE items (
  db_uuid VARCHAR(36),
  session TEXT,
  bundle TEXT,
  itemID INTEGER,
  level TEXT,
  type TEXT,
  seqIdx INTEGER,
  sampleRate FLOAT,
  samplePoint INTEGER,
  sampleStart INTEGER,
  sampleDur INTEGER,
  PRIMARY KEY (db_uuid,session,bundle,itemID),
  FOREIGN KEY (db_uuid,session,bundle) REFERENCES bundle(db_uuid,session_name,name)
);'
database.DDL.emuDB_itemsIdx='CREATE INDEX items_idx ON items(db_uuid,session,bundle,itemID,level,type)'

database.DDL.emuDB_labels='CREATE TABLE labels (
  db_uuid VARCHAR(36),
  session TEXT,
  bundle TEXT,
  itemID TEXT,
  labelIdx INTEGER,
  name TEXT,
  label TEXT,
  FOREIGN KEY (db_uuid,session,bundle) REFERENCES bundle(db_uuid,session,name)
);'

database.DDL.emuDB_links='CREATE TABLE links (
  db_uuid VARCHAR(36) NOT NULL,
  session TEXT,
  bundle TEXT,
  fromID INTEGER,
  toID INTEGER,
  label TEXT,
  FOREIGN KEY (db_uuid,session,bundle) REFERENCES bundle(db_uuid,session,name)
);'
database.DDL.emuDB_linksIdx='CREATE INDEX links_idx ON links(db_uuid,session,bundle,fromID,toID)'

database.DDL.emuDB_linksTmp='CREATE TABLE linksTmp (
   db_uuid VARCHAR(36) NOT NULL,
  session TEXT,
  bundle TEXT,
  fromID INTEGER,
  toID INTEGER,
  label TEXT,
  FOREIGN KEY (db_uuid,session,bundle) REFERENCES bundle(db_uuid,session,name)
);'
database.DDL.emuDB_linksTmpIdx='CREATE INDEX linksTmp_idx ON linksTmp(db_uuid,session,bundle,fromID,toID)'

database.DDL.emuDB_linksExt='CREATE TABLE linksExt (
  db_uuid VARCHAR(36) NOT NULL,
  session TEXT,
  bundle TEXT,
  fromID INTEGER,
  toID INTEGER,
  seqIdx INTEGER,
  toLevel TEXT,
  type TEXT,
  toSeqIdx INTEGER,
  toSeqLen INTEGER,
  label TEXT,
  FOREIGN KEY (db_uuid,session,bundle) REFERENCES bundle(db_uuid,session,name)
);'


database.DDL.emuDB_linksExtIdx='CREATE INDEX linksExt_idx ON linksExt(db_uuid,session,bundle,fromID,toID,toLevel,type)'

database.DDL.emuDB_linksExtTmp='CREATE TABLE linksExtTmp (
  db_uuid VARCHAR(36) NOT NULL,
  session TEXT,
  bundle TEXT,
  fromID INTEGER,
  toID INTEGER,
  seqIdx INTEGER,
  toLevel TEXT,
  type TEXT,
  toSeqIdx INTEGER,
  toSeqLen INTEGER,
  label TEXT,
  FOREIGN KEY (db_uuid,session,bundle) REFERENCES bundle(db_uuid,session,name)
);'
database.DDL.emuDB_linksExtTmpIdx='CREATE INDEX linksExtTmp_idx ON linksExtTmp(db_uuid,session,bundle,fromID,toID,toLevel,type)'

database.DDL.emuDB_linksExtTmp2='CREATE TABLE linksExtTmp2 (
  db_uuid VARCHAR(36) NOT NULL,
  session TEXT,
  bundle TEXT,
  fromID INTEGER,
  toID INTEGER,
  seqIdx INTEGER,
  toLevel TEXT,
  type TEXT,
  toSeqIdx INTEGER,
  toSeqLen INTEGER,
  label TEXT,
  FOREIGN KEY (db_uuid,session,bundle) REFERENCES bundle(db_uuid,session,name)
);'
database.DDL.emuDB_linksExtTmpIdx2='CREATE INDEX linksExtTmp2_idx ON linksExtTmp2(db_uuid,session,bundle,fromID,toID,toLevel,type)'



orm.DBI.insert<-function(conn,obj){
  
}

.store.emuDB.DBI<-function(database){
  dbCfg=database[['DBconfig']]
  dbCfgJSON=jsonlite::toJSON(dbCfg,auto_unbox=TRUE,force=TRUE,pretty=TRUE)
  dbSqlInsert=paste0("INSERT INTO emuDB(uuid,name,basePath,DBconfigJSON) VALUES('",dbCfg[['UUID']],"','",dbCfg[['name']],"','",database[['basePath']],"','",dbCfgJSON,"')")
  res <- dbSendQuery(emuDBs.con,dbSqlInsert)
  dbClearResult(res)
  
}

get.database<-function(uuid=NULL,name=NULL){
  return(.load.emuDB.DBI(uuid,name))
}
.load.emuDB.DBI<-function(uuid=NULL,name=NULL){
  if(is.null(uuid)){
   uuid=get.database.uuid(name)
  }
  dbQ=paste0("SELECT * FROM emuDB WHERE uuid='",uuid,"'")
  dbDf=dbGetQuery(emuDBs.con,dbQ)
  dbCount=nrow(dbDf)
  if(dbCount==0){
    stop("Database not found !\n")
  }else if (dbCount==1){
    dbCfg=jsonlite::fromJSON(dbDf[['DBconfigJSON']],simplifyVector=FALSE)
    db=create.database(name = dbDf[['name']],basePath = dbDf[['basePath']],DBconfig = dbCfg)
  }else{
    stop("Found ",dbCount," databases with same name: ",name,". Please specify database UUID!\n")
  }
  return(db)
  
}

.store.session.DBI<-function(dbUUID,sessionName){
  insertSessionSql=paste0("INSERT INTO session(db_uuid,name) VALUES('",dbUUID,"','",sessionName,"')")
  res<-dbSendQuery(emuDBs.con,insertSessionSql)
  dbClearResult(res)
}

.load.sessions.DBI<-function(dbUUID){
  dbQ=paste0("SELECT * FROM session WHERE db_uuid='",dbUUID,"'")
  sesssDf=dbGetQuery(emuDBs.con,dbQ)
  return(sesssDf)
}

.store.bundle.DBI<-function(database,bundle){
  dbCfg=database[['DBconfig']]
  #dbCfgJSON=jsonlite::toJSON(dbCfg,auto_unbox=TRUE,force=TRUE,pretty=TRUE)
  #dbSqlInsert=paste0("INSERT INTO emuDB(name,databaseDir,DBconfigJSON) VALUES('",dbCfg[['name']],"','",database[['databseDir']],"','",dbCfgJSON,"')")
 
  bSqlInsert=paste0("INSERT INTO bundle(db_uuid,session,name,annotates,sampleRate,mediaFilePath) VALUES('",dbCfg[['UUID']],"','",bundle[['session']],"','",bundle[['name']],"','",bundle[['annotates']],"',",bundle[['sampleRate']],",'",bundle[['mediaFilePath']],"')")
  res <- dbSendQuery(emuDBs.con,bSqlInsert)
  dbClearResult(res)
  for(trackPath in bundle[['signalpaths']]){
    trSqlInsert=paste0("INSERT INTO track(db_uuid,session,bundle,path) VALUES('",dbCfg[['UUID']],"','",bundle[['session']],"','",bundle[['name']],"','",trackPath,"')")
    res <- dbSendQuery(emuDBs.con,trSqlInsert)
  }
}

.load.bundle.names.DBI<-function(dbUUID,sessionName){
  bQ=paste0("SELECT name FROM bundle WHERE db_uuid='",dbUUID,"' AND session='",sessionName,"'")
  bDf=dbGetQuery(emuDBs.con,bQ)
  return(bDf[['name']])
}



.load.bundle.DBI<-function(dbUUID,sessionName,bundleName){
  bQ=paste0("SELECT * FROM bundle WHERE db_uuid='",dbUUID,"' AND session='",sessionName,"' AND name='",bundleName,"'")
  bDf=dbGetQuery(emuDBs.con,bQ)
  spQ=paste0("SELECT * FROM track WHERE db_uuid='",dbUUID,"' AND session='",sessionName,"' AND bundle='",bundleName,"'")
  spDf=dbGetQuery(emuDBs.con,spQ)
  signalpaths=as.list(spDf[['path']])
  bDfRows=nrow(bDf)
  if(bDfRows==0){
    return(NULL)
  }else if(bDfRows==1){
    return(as.list(bDf))
  }else{
    stop("Ambigious result for bundle lookup")
  }
  
 
}


.initialize.DBI.database<-function(createTables=TRUE){
  if(is.null(emuDBs.con)){
   emuDBs.con<<-dbConnect(RSQLite::SQLite(), ":memory:")
  if(createTables & !dbExistsTable(emuDBs.con,'emuDB')){
    res <- dbSendQuery(emuDBs.con, database.DDL.emuDB)
    dbClearResult(res)
    res <- dbSendQuery(emuDBs.con, database.DDL.emuDB_session) 
    dbClearResult(res)
    res <- dbSendQuery(emuDBs.con, database.DDL.emuDB_track) 
    dbClearResult(res)
    res <- dbSendQuery(emuDBs.con, database.DDL.emuDB_bundle) 
    dbClearResult(res)
    res <- dbSendQuery(emuDBs.con, database.DDL.emuDB_items) 
    dbClearResult(res)
    res <- dbSendQuery(emuDBs.con, database.DDL.emuDB_itemsIdx) 
    dbClearResult(res)
    res <- dbSendQuery(emuDBs.con, database.DDL.emuDB_labels) 
    dbClearResult(res)
    res <- dbSendQuery(emuDBs.con, database.DDL.emuDB_links) 
    dbClearResult(res)
    res <- dbSendQuery(emuDBs.con, database.DDL.emuDB_linksIdx) 
    dbClearResult(res)
    res <- dbSendQuery(emuDBs.con, database.DDL.emuDB_linksExt) 
    dbClearResult(res)
    res <- dbSendQuery(emuDBs.con, database.DDL.emuDB_linksExtIdx) 
    dbClearResult(res)
   
  res <- dbSendQuery(emuDBs.con, database.DDL.emuDB_linksTmp) 
  dbClearResult(res)
  res <- dbSendQuery(emuDBs.con, database.DDL.emuDB_linksTmpIdx) 
  dbClearResult(res)
  res <- dbSendQuery(emuDBs.con, database.DDL.emuDB_linksExtTmp) 
  dbClearResult(res)
  res <- dbSendQuery(emuDBs.con, database.DDL.emuDB_linksExtTmpIdx) 
  dbClearResult(res)
  res <- dbSendQuery(emuDBs.con, database.DDL.emuDB_linksExtTmp2) 
  dbClearResult(res)
  res <- dbSendQuery(emuDBs.con, database.DDL.emuDB_linksExtTmpIdx2) 
  dbClearResult(res)
 
  }
  }
  #dbDisconnect(con)
  return(emuDBs.con)
}

.destroy.DBI.database<-function(){
  if(!is.null(emuDBs.con)){
    dbDisconnect(emuDBs.con)
  }
  emuDBs.con<<-NULL
}


.get.database.uuid<-function(name=NULL,uuid=NULL){
  if(is.null(uuid)){
    return(get.database.uuid(name))
  }else{
    return(uuid)
  }
}
get.database.uuid<-function(name){
  
  dbQ=paste0("SELECT uuid FROM emuDB WHERE name='",name,"'") 
  dbDf=dbGetQuery(emuDBs.con,dbQ)
  dbCount=nrow(dbDf)
  if(dbCount==0){
    stop("Database ",name," not found !\n")
  }else if (dbCount==1){
    return(dbDf[['uuid']])
  }else{
    stop("Found ",dbCount," databases with same name: ",name,". Please use database UUID!\n")
  }
}

list.databases<-function(){
  .initialize.DBI.database()
  dbs=dbGetQuery(emuDBs.con,"SELECT uuid,name,basePath FROM emuDB")
  return(dbs)
}

.clear.database<-function(uuid){
  .initialize.DBI.database()
  dbs=dbGetQuery(emuDBs.con,paste0("DELETE FROM links WHERE db_uuid='",uuid,"'"))
  dbs=dbGetQuery(emuDBs.con,paste0("DELETE FROM linksExt WHERE db_uuid='",uuid,"'"))
  dbs=dbGetQuery(emuDBs.con,paste0("DELETE FROM labels WHERE db_uuid='",uuid,"'"))
  dbs=dbGetQuery(emuDBs.con,paste0("DELETE FROM items WHERE db_uuid='",uuid,"'"))
  dbs=dbGetQuery(emuDBs.con,paste0("DELETE FROM track WHERE db_uuid='",uuid,"'"))
  dbs=dbGetQuery(emuDBs.con,paste0("DELETE FROM bundle WHERE db_uuid='",uuid,"'"))
  dbs=dbGetQuery(emuDBs.con,paste0("DELETE FROM session WHERE db_uuid='",uuid,"'"))
  dbs=dbGetQuery(emuDBs.con,paste0("DELETE FROM emuDB WHERE uuid='",uuid,"'"))
}

clear.all.databases<-function(){
  cleared=FALSE
  ans=readline('Are you sure you want to remove all databases from this R session? (y/n)')
  if(ans=='y'){
    .destroy.DBI.database()
    .initialize.DBI.database()
    cleared=TRUE
  }
  return(cleared)
}

list.sessions<-function(dbName=NULL,dbUUID=NULL){
  .initialize.DBI.database()
  uuid=.get.database.uuid(dbName,dbUUID)
  dbs=dbGetQuery(emuDBs.con,paste0("SELECT name FROM session WHERE db_uuid='",uuid,"'"))
  return(dbs)
}

list.bundles<-function(dbName=NULL,dbUUID=NULL){
  .initialize.DBI.database()
  uuid=.get.database.uuid(dbName,dbUUID)
  dbs=dbGetQuery(emuDBs.con,paste0("SELECT session,name FROM bundle WHERE db_uuid='",uuid,"'"))
  return(dbs)
}


create.database <- function(name,basePath=NULL,DBconfig=create.schema.databaseDefinition(name = name),sessions=NULL,primaryExtension=NULL){
  o <- list(name=name,basePath=basePath,DBconfig=DBconfig,sessions=sessions,primaryExtension=primaryExtension,apiLevel=emuDB.apiLevel)
  class(o) <- c('emuDB','list')
  invisible(o)
}

get.bundle.levels.data.frame <-function(db,bundle){
  # do not use this function to append multiple bundles (performance is bad)
  # growing data.frames with rbind() is unefficient
  itCount=200
  DBconfig=db[['DBconfig']]
  sessionName=bundle[['sessionName']]
  row=1 
  #bdf=data.table(matrix(ncol=length(db[['DBconfig']][['itemColNames']]),nrow=0))
  # caclculate bundle items length
  itCount=0
  for(lvl in bundle[['levels']]){
    lvlItCount=length(lvl[['items']])
    itCount=itCount+lvlItCount
  }
  bdf=data.frame(id=character(itCount),session=character(itCount),bundle=character(itCount),level=character(itCount),itemID=integer(itCount),type=character(itCount),seqIdx=integer(itCount),sampleRate=numeric(itCount),samplePoint=integer(itCount),sampleStart=integer(itCount),sampleDur=integer(itCount),stringsAsFactors=FALSE)
  #colnames(bdf)<-db[['DBconfig']][['itemColNames']]
  ldf=NULL
  lrow=1
  maxLbls=db[['DBconfig']][['maxNumberOfLabels']]
  #lblColNames=c('itemID','bundle','labelIdx','name','label')
  ldf=data.frame(itemID=character(0),session=character(0),bundle=character(0),labelIdx=integer(0),name=character(0),label=character(0),stringsAsFactors=FALSE)
  #colnames(ldf)<-lblColNames
  bName=bundle[['name']]
  for(lvl in bundle[['levels']]){
    
    seqIdx=0L
    for(it in lvl[['items']]){
      
      seqIdx=seqIdx+1L
      
      bdf[row,'session']=sessionName
      bdf[row,'bundle']=bName
      itemId=it[['id']]
      if(is.null(itemId)){
        # for instance aetobi has no .hlb files and therefore no links and item ids
        id=paste(db[['name']],sessionName,bName,sep='_')
        itemId=NA
      }else{
        id=paste(db[['name']],sessionName,bName,it['id'],sep='_')
      }
      bdf[row,'id']=id
      bdf[row,'itemID']=itemId
      bdf[row,'level']=lvl[['name']]
      bdf[row,'type']=lvl[['type']]
      if(!is.null(bundle[['sampleRate']])){
        bdf[row,'sampleRate']=bundle[['sampleRate']]
      }else{
        bdf[row,'sampleRate']=NA
      }
      bdf[row,'seqIdx']=seqIdx
      sp=it[['samplePoint']]
      if(!is.null(sp)){
        bdf[row,'samplePoint']=sp
      }else{
        bdf[row,'samplePoint']=NA
      }
      ss=it[['sampleStart']]
      if(!is.null(ss)){
        bdf[row,'sampleStart']=ss
      }else{
        bdf[row,'sampleStart']=NA
      }
      sd=it[['sampleDur']]
      if(!is.null(sd)){
        bdf[row,'sampleDur']=sd
      }else{
        bdf[row,'sampleDur']=NA
      }
      # TODO get label column names from schema and set them as last columns
      lbls=it[['labels']]
      lblsLen=length(lbls)
      #lbl0=it[['labels']][[1]][['value']]
      #bdf[row,'label']=lbl0
      for(i in 1:maxLbls){
        rLbl=NA
        if(lblsLen>=i){
          lbl=lbls[[i]]
          if(!is.null(lbl)){
            rLbl=lbl[['value']]
            ldf[lrow,'itemID']=id
            ldf[lrow,'session']=sessionName
            ldf[lrow,'bundle']=bName
            ldf[lrow,'labelIdx']=i-1
            ldf[lrow,'name']=lbl[['name']]
            ldf[lrow,'label']=rLbl
            lrow=lrow+1L
          }
        }
      } 
      row=row+1L
    }
  }
  if(replace){
    # remove old bundle data
    otherBundlesSelector=(!(db[['items']][['session']]==sessionName & db[['items']][['bundle']]==bName))
    db[['items']]=db[['items']][otherBundlesSelector,]
    
    otherBundlesSelector=(!(db[['labels']][['session']]==sessionName & db[['labels']][['bundle']]==bName))
    db[['labels']]=db[['labels']][otherBundlesSelector,]
    
    
    delSqlQuery=paste0("DELETE FROM items WHERE session='",sessionName,"' AND bundle='",bName,"'")
    res<-dbSendQuery(emuDBs.con,delSqlQuery)
    dbClearResult(res)
    delSqlQuery=paste0("DELETE FROM labels WHERE session='",sessionName,"' AND bundle='",bName,"'")
    res<-dbSendQuery(emuDBs.con,delSqlQuery)
    dbClearResult(res)
  }
  
  db[['items']]=rbind(db[['items']],bdf)
  if(!is.null(ldf)){
    db[['labels']]=rbind(db[['labels']],ldf)
  }
  return(db)
  
}
move.bundle.links.to.data.frame <-function(db,bundle,replace=TRUE){
  .initialize.DBI.database()
  dbWriteTable(emuDBs.con,'items',db[['items']])
  dbWriteTable(emuDBs.con,'labels',db[['labels']])
  dbWriteTable(emuDBs.con,'links',db[['links']])
  dbWriteTable(emuDBs.con,'linksExt',db[['linksExt']])
  
  row=1
  bdf=data.frame(session=character(0),bundle=character(0),fromID=integer(0),toID=integer(0),label=character(0),stringsAsFactors=FALSE)
  sessionName=bundle[['sessionName']]
  bName=bundle[['name']]
  row=0
  for(lk in bundle[['links']]){
    row=row+1L 
    bdf[row,'session']=sessionName
    bdf[row,'bundle']=bName
    bdf[row,'fromID']=lk[['fromID']]
    bdf[row,'toID']=lk[['toID']]
    lbl=lk[['label']]
    if(!is.null(lbl)){
      bdf[row,'label']=lbl
    }else{
      bdf[row,'label']=NA
    }
  }
  if(replace){
    # remove old bundle data
    #     otherBundlesSelector=(!(db[['links']][['session']]==sessionName & db[['links']][['bundle']]==bName))
    #     db[['links']]=db[['links']][otherBundlesSelector,]
    #     otherBundlesSelector=(!(db[['linksExt']][['session']]==sessionName & db[['linksExt']][['bundle']]==bName))
    #     db[['linksExt']]=db[['linksExt']][otherBundlesSelector,]
    #     
    delSqlQuery=paste0("DELETE FROM links WHERE session='",sessionName,"' AND bundle='",bName,"'")
    res<-dbSendQuery(emuDBs.con,delSqlQuery)
    dbClearResult(res)
    delSqlQuery=paste0("DELETE FROM linksExt WHERE session='",sessionName,"' AND bundle='",bName,"'")
    res<-dbSendQuery(emuDBs.con,delSqlQuery)
    dbClearResult(res)
    
  }
  
  # db[['links']]=rbind(db[['links']],bdf)
  for(lk in bundle[['links']]){
    row=row+1L 
    bdf[row,'session']=sessionName
    bdf[row,'bundle']=bName
    bdf[row,'fromID']=lk[['fromID']]
    bdf[row,'toID']=lk[['toID']]
    lbl=lk[['label']]
    if(!is.null(lbl)){
      bdf[row,'label']=lbl
      lblCol=paste0("'",lbl,"'")
    }else{
      bdf[row,'label']=NA
      lblCol='NULL'
    }
    sqlQuery=paste0("INSERT INTO links VALUES('",sessionName,"','",bName,"',",fromID,",",toID,",",lblCol)
    res<-dbSendQuery(emuDBs.con,sqlQuery)
  }
  
  build.redundant.links.all(database = db,sessionName=sessionName,bundleName=bName)
  #TODO  put level and links method together and use only items of the bundle
  calculate.postions.of.links()
  db[['links']]=dbReadTable(emuDBs.con,'links')
  db[['linksExt']]=dbReadTable(emuDBs.con,'linksExt')
  .destroy.DBI.database()
  return(db)
}



##' Print summary for EMU database object
##' @description Gives an overview of an EMU database object
##' Prints database name, base directory path and informations about annoation levels, attributes, links, and signal file tracks
##' @param object EMU database object
##' @param ... (not used)
##' @method summary emuDB
##' @export
summary.emuDB<-function(object,...){
 
  cat("Name:\t",object[['name']],"\n")
  cat("Directory:\t",object[['basePath']],"\n")
  cat("Session count:",length(object[['sessions']]),"\n")
  bndlCnt=0
  for(s in object[['sessions']]){
    bndlCnt=bndlCnt+length(s[['bundles']])
  }
  cat("Bundle count:",bndlCnt,"\n")
  cat("Annotation item count: ",nrow(object[['items']]),", links count: ",nrow(object[['links']]),"\n")
  cat("\nDatabase configuration:\n\n")
  summary(object[['DBconfig']])
  #cat("SSFF track definitions:\n")
  # TODO 

  
}

"as.emuDB"<-function(o,class){
  if(class=='emuDB'){
    return(o)
  }
  if(class=='list'){
    class(o) <- c('list')
    return(o)
  }
}


# Create emuDB bundle object
# @description A bundle typically contains media files and annoations of an utterance
# @param name name of the bundle
# @param sessionName session ID of the bundle
# @param legacyBundleID legacy bundle ID
# @param annotates annotated signal file
# @param sampleRate sample rate
# @param signalpaths pathes of signal files
# @param mediaFilePath path pattern of samples track
# @param levels list of annotation levels
# @param links list of links containing the hierarchical information of the annotation levels
# @return object of class emuDB.bundle
# @author Klaus Jaensch
# @keywords emuDB bundle Emu
# 
create.bundle <- function(name,sessionName=NULL,legacyBundleID=NULL,annotates=NULL,sampleRate,signalpaths=list(),mediaFilePath=NULL,levels=list(),links=list()){
  o <- list(name=name,sessionName=sessionName,legacyBundleID=legacyBundleID,annotates=annotates,sampleRate=sampleRate,signalpaths=signalpaths,mediaFilePath=mediaFilePath,files=signalpaths,levels=levels,links=links)
  return(as.bundle(o))
}

as.bundle <- function(bundleData){
  class(bundleData) <- 'emuDB.bundle'
  attr(bundleData,'ips.persist')<-list(typesJSON=list(levels='array'))
  invisible(bundleData)
}

build.redundant.links.all<-function(database,sessionName=NULL,bundleName=NULL){
  # Legacy EMU and query functions link collections contain links for each possible connection between levels
  # We consider links that do not follow link definition constraints as redundant and therefore we remove them from the
  # link data model
  #
 
  lfs=build.link.defs(database[['DBconfig']])
  maxLfLen=0
  for(lf in lfs){
    lfLen=length(lf)
    if(lfLen>maxLfLen){
      maxLfLen=lfLen
    }
  }
  return(build.redundant.links.for.pathes(database,lfs,sessionName,bundleName) )
  
}

build.redundant.links<-function(database,fromLevel,toLevel){
  # Legacy EMU and query functions link collections contain links for each possible connection between levels
  # We consider links that do not follow link definition constraints as redundant and therefore we remove them from the
  # link data model. For queries we build links for particular start and end level.
  #
  
  lfs=build.level.partial.pathes(database[['DBconfig']],fromLevel,toLevel)
 
  return(build.redundant.links.for.pathes(database,lfs) )
}
  

build.redundant.links.for.pathes<-function(database,lfs,sessionName='0000',bundleName=NULL){
  maxLfLen=0
  for(lf in lfs){
    lfLen=length(lf)
    if(lfLen>maxLfLen){
      maxLfLen=lfLen
    }
  }
  
  res<-dbSendQuery(emuDBs.con,'DELETE FROM linksTmp')
  dbClearResult(res)
  sqlQuery="INSERT INTO linksTmp(db_uuid,session,bundle,fromID,toID,label) SELECT DISTINCT f.db_uuid,f.session,f.bundle,f.itemID AS fromID,t.itemID AS toID, NULL AS label FROM items f,items t"
  sqlQuery=paste0(sqlQuery," WHERE f.db_uuid=t.db_uuid AND f.bundle=t.bundle AND f.session=t.session AND f.db_uuid='",database[['DBconfig']][['UUID']],"' AND ")
  #sqlQuery=paste0(sqlQuery," WHERE f.db_uuid=t.db_uuid AND f.bundle=t.bundle AND f.session=t.session AND ")
 
  if(!is.null(sessionName) & !is.null(bundleName)){
     # only for one bundle
    sqlQuery=paste0(sqlQuery,"f.session='",sessionName,"' AND f.bundle='",bundleName,"' AND ")
  }
  #if(maxLfLen>2){
  #  for( ic in 2:(maxLfLen-1)){
  #    sqlQuery=paste0(sqlQuery,'i',ic,'.bundle=f.bundle AND ')
  #  }
  #}
  sqlQuery=paste0(sqlQuery,' (')
  ## TEST
  #lfs=list(c('Phoneme','Phonetic'))
  # build query for each partial path
  lfsLen=length(lfs)
  for(i in 1:lfsLen){
    lf=lfs[[i]]
    #cat("Path: ",lf,"\n")
    lfLen=length(lf)
    sLf=lf[1]
    eLf=lf[lfLen]
    sqlQuery=paste0(sqlQuery,"(f.level='",sLf,"' AND t.level='",eLf,"'" )
    sqlQuery=paste0(sqlQuery," AND EXISTS (SELECT l1.* FROM ")
    for(li in 1:(lfLen-1)){
      sqlQuery=paste0(sqlQuery,'links l',li)
      if(li<(lfLen-1)){
        sqlQuery=paste0(sqlQuery,',')
      }
    }
    if(lfLen>2){
      for(ii in 2:(lfLen-1)){
        sqlQuery=paste0(sqlQuery,',items i',ii)
      }
    }
    sqlQuery=paste0(sqlQuery," WHERE ")
    if(lfLen==2){
      sqlQuery=paste0(sqlQuery,"l1.db_uuid=f.db_uuid AND l1.bundle=f.bundle AND l1.bundle=t.bundle AND l1.db_uuid=t.db_uuid AND l1.session=f.session AND l1.session=t.session AND f.itemID=l1.fromID AND t.itemID=l1.toID")
      #sqlQuery=paste0(sqlQuery,"l1.bundle=f.bundle AND l1.bundle=t.bundle AND f.itemID=l1.fromID AND t.itemID=l1.toID")
      #cat(sLf,eLf,"\n")
    }else{
      # TODO start and end connection
      # from start to first in-between item 
      eLf=lf[2]
      #cat(sLf,eLf,"\n")
      sqlQuery=paste0(sqlQuery,"l1.db_uuid=f.db_uuid AND l1.bundle=f.bundle AND l1.db_uuid=i2.db_uuid AND l1.bundle=i2.bundle AND l1.session=f.session AND l1.session=i2.session AND f.itemID=l1.fromID AND i2.itemID=l1.toID AND f.level='",sLf,"' AND i2.level='",eLf,"' AND ")
      #sqlQuery=paste0(sqlQuery,"l1.bundle=f.bundle AND l1.bundle=i2.bundle AND f.itemID=l1.fromID AND i2.itemID=l1.toID AND f.level='",sLf,"' AND i2.level='",eLf,"' AND ")
      if(lfLen>3){
        for(j in 2:(lfLen-2)){
          sLf=lf[j]
          eLf=lf[j+1L] 
          #cat(sLf,eLf,"\n")
          sqlQuery=paste0(sqlQuery,"l",j,".db_uuid=i",j,".db_uuid AND l",j,".db_uuid=i",(j+1),".db_uuid AND l",j,".bundle=i",j,".bundle AND l",j,".bundle=i",(j+1),".bundle AND l",j,".session=i",j,".session AND l",j,".session=i",(j+1),".session AND i",j,".itemID=l",j,".fromID AND i",(j+1L),".itemID=l",j,".toID AND i",j,".level='",sLf,"' AND i",(j+1L),".level='",eLf,"' AND ")
        }
      }
      # from last in-between item to end item
      sLf=lf[(lfLen-1)]
      eLf=lf[lfLen]
      #cat(sLf,eLf,(lfLen-1),"\n")
      j=lfLen-1
      sqlQuery=paste0(sqlQuery,"l",j,".db_uuid=i",j,".db_uuid AND l",j,".db_uuid=t.db_uuid AND l",j,".bundle=i",j,".bundle AND l",j,".bundle=t.bundle AND l",j,".session=i",j,".session AND l",j,".session=t.session AND i",j,".itemID=l",j,".fromID AND t.itemID=l",j,".toID AND i",j,".level='",sLf,"' AND t.level='",eLf,"'")
      #sqlQuery=paste0(sqlQuery,"l",j,".bundle=i",j,".bundle AND l",j,".bundle=t.bundle AND i",j,".itemID=l",j,".fromID AND t.itemID=l",j,".toID AND i",j,".level='",sLf,"' AND t.level='",eLf,"'")
    }
    sqlQuery=paste0(sqlQuery,"))")
    if(i<lfsLen){
      sqlQuery=paste0(sqlQuery," OR ")
    }
  }
  sqlQuery=paste0(sqlQuery,")")
  #cat(sqlQuery,"\n")
  # since version 2.8.x of sqlite the query is very slow without indices
 

  #cat(sqlQuery,"\n")
  #res<-dbSendQuery(emuDBs.con,comQuery)
  res<-dbSendQuery(emuDBs.con,sqlQuery)
  dbClearResult(res)
  #print(dbReadTable(emuDBs.con,'linksTmp'))
  
}

get.level.name.for.attribute<-function(db,attributeName){
  for(ld in db[['DBconfig']][['levelDefinitions']]){
    for(ad in ld[['attributeDefinitions']]){
      if(ad[['name']]==attributeName){
        return(ld[['name']])
      }
    }
  }
  return(NULL)
}

.remove.bundle.annot.DBI<-function(dbUUID,bundle){
  sessionName=bundle[['session']]
  bName=bundle[['name']]
  cat("Links br: ",nrow(dbReadTable(emuDBs.con,'links')),"\n")
  cat("LinksExt br: ",nrow(dbReadTable(emuDBs.con,'linksExt')),"\n")
  cntSqlQuery=paste0("SELECT * FROM items WHERE db_uuid='",dbUUID,"' AND session='",sessionName,"' AND bundle='",bName,"'")
  cat(cntSqlQuery,"\n")
  res<-dbGetQuery(emuDBs.con,cntSqlQuery)
  cat("items to delete: ",nrow(res),"\n")
  delSqlQuery=paste0("DELETE FROM items WHERE db_uuid='",dbUUID,"' AND session='",sessionName,"' AND bundle='",bName,"'")
  res<-dbSendQuery(emuDBs.con,delSqlQuery)
  dbClearResult(res)
  delSqlQuery=paste0("DELETE FROM labels WHERE db_uuid='",dbUUID,"' AND session='",sessionName,"' AND bundle='",bName,"'")
  res<-dbSendQuery(emuDBs.con,delSqlQuery)
  dbClearResult(res)
  delSqlQuery=paste0("DELETE FROM links WHERE db_uuid='",dbUUID,"' AND session='",sessionName,"' AND bundle='",bName,"'")
  cat(delSqlQuery,"\n")
  res<-dbSendQuery(emuDBs.con,delSqlQuery)
  dbClearResult(res)
  cntSqlQuery=paste0("SELECT * FROM linksExt WHERE db_uuid='",dbUUID,"' AND session='",sessionName,"' AND bundle='",bName,"'")
  res<-dbGetQuery(emuDBs.con,cntSqlQuery)
  cat("linksExt to delete: ",nrow(res),"\n")
  delSqlQuery=paste0("DELETE FROM linksExt WHERE db_uuid='",dbUUID,"' AND session='",sessionName,"' AND bundle='",bName,"'")
  cat(delSqlQuery,"\n")
  res<-dbSendQuery(emuDBs.con,delSqlQuery)
  dbClearResult(res)
  cat("Links ar: ",nrow(dbReadTable(emuDBs.con,'links')),"\n")
  cat("LinksExt ar: ",nrow(dbReadTable(emuDBs.con,'linksExt')),"\n")
}

.store.bundle.annot.DBI<-function(dbUUID,bundle){
  bName=bundle[['name']]
  for(lvl in bundle[['levels']]){
    
    seqIdx=0L
    for(it in lvl[['items']]){
      seqIdx=seqIdx+1L
      
      itemId=it[['id']]
      if(is.null(itemId)){
        itemId=NA
      }
      if(!is.null(bundle[['sampleRate']])){
        srCol=bundle[['sampleRate']]
      }else{
        srCol='NULL'
      }
      sp=it[['samplePoint']]
      if(!is.null(sp)){
        spCol=as.integer(sp)
      }else{
        spCol='NULL'
      }
      ss=it[['sampleStart']]
      if(!is.null(ss)){
        ssCol=as.integer(ss)
      }else{
        ssCol='NULL'
      }
      sdur=it[['sampleDur']]
      if(!is.null(sdur)){
        sdurCol=sdur
      }else{
        sdurCol='NULL'
      }
      
      sqlInsert=paste0("INSERT INTO items(db_uuid,session,bundle,itemID,level,type,seqIdx,sampleRate,samplePoint,sampleStart,sampleDur) VALUES('",dbUUID,"','",bundle[['session']],"','",bName,"',",itemId,",'",lvl[['name']],"','",lvl[['type']],"',",seqIdx,",",bundle[['sampleRate']],",",spCol,",",ssCol,",",sdurCol,")")
      #cat('SQL:',sqlInsert,"\n")
      res<-dbSendQuery(emuDBs.con,sqlInsert)
      dbClearResult(res)
      
      lbls=it[['labels']]
      lblsLen=length(lbls)
      for(i in 1:lblsLen){
        rLbl=NA
        #if(lblsLen>=i){
        lbl=lbls[[i]]
        if(!is.null(lbl)){
          rLbl=lbl[['value']]
          sqlInsert=paste0("INSERT INTO labels VALUES('",dbUUID,"','",bundle[['session']],"','",bName,"',",itemId,",",i-1L,",'",lbl[['name']],"',\"",rLbl,"\")")
          #cat('SQL:',sqlInsert,"\n")
          res<-dbSendQuery(emuDBs.con,sqlInsert)
          dbClearResult(res)
        }
        #}
      } 
      
    }
  }
  
  for(lk in bundle[['links']]){
    lbl=lk[['label']]
    if(is.null(lbl)){
      lblCol='NULL'
    }else{
      lblCol=paste0("'",lbl,"'")
    }
    
    sqlInsert=paste0("INSERT INTO links(db_uuid,session,bundle,fromID,toID,label) VALUES('",dbUUID,"','",bundle[['session']],"','",bName,"',",lk[['fromID']],",",lk[['toID']],",",lblCol,")")
   
    #cat('SQL:',sqlInsert,"\n")
    res<-dbSendQuery(emuDBs.con,sqlInsert)
    dbClearResult(res)
    
    
  }
  
}

.load.bundle.levels.s3 <-function(dbUUID,sessionName,bundleName){
  
  db=.load.emuDB.DBI(dbUUID)
  levelDefinitions=db[['DBconfig']][['levelDefinitions']]
  find.levelDefinition<-function(name){
    for(lvlDef in levelDefinitions){
      if(name == lvlDef[['name']]){
        return(lvlDef)
      }
    }
  }
  
  # create all levels
  levels=list()
  for(ld in levelDefinitions){
    levels[[ld[['name']]]]=create.bundle.level(name=ld[['name']],type=ld[['type']])
  }
  
  itsQ=paste0("SELECT * FROM items WHERE db_uuid='",dbUUID,"' AND session='",sessionName,"' AND bundle='",bundleName,"'")
  items=dbGetQuery(emuDBs.con,itsQ)
  lblsQ=paste0("SELECT * FROM labels WHERE db_uuid='",dbUUID,"' AND session='",sessionName,"' AND bundle='",bundleName,"'")
  bundleLabels=dbGetQuery(emuDBs.con,lblsQ)
  
  nrows=nrow(items)
  cLvl=NULL
  lvlDef=NULL
  lvlItems=list()
  
  if(nrows>0){
    for(r in 1:nrows){
      rLvl=items[r,'level']
      if(!is.null(cLvl) && cLvl[['name']]!=rLvl){
        cLvl[['items']]=lvlItems
        levels[[cLvl[['name']]]]=cLvl
        cLvl=NULL
      }
      
      if(is.null(cLvl)){
        
        lvlDef=find.levelDefinition(rLvl)
        lvlItems=list()
        sr=NULL
        srDf=items[r,'sampleRate']
        if(!is.na(srDf)){
          sr=srDf
        }
        lvl=levels[[rLvl]]
        if(lvl[['type']]!=items[r,'type']){
          stop("Wrong item type ",items[r,'type']," for level ",rLvl," type ",lvl[['type']]," in bundle: ",sessionName,":",bundleName,"\n")
        }
        # create new level object 
        cLvl=create.bundle.level(name=rLvl,type=lvl[['type']],sampleRate=sr,items=lvlItems)
      }
      id=items[r,'itemID']
      type=items[r,'type']
      
      attrDefs=lvlDef[['attributeDefinitions']]
      attrDefsLen=length(attrDefs)
      
      #gid=items[r,'id']
      #itemLabelSelector=bundleLabels[['itemID']]==gid
      itemLabelSelector=bundleLabels[['itemID']]==id
      labelRows=bundleLabels[itemLabelSelector,]
      nLabelRows=nrow(labelRows)
      labels=list()
      for(j in 1:nLabelRows){
        
        
        lblNm=labelRows[j,'name']
        labels[[j]]=list(name=lblNm,value=labelRows[j,'label'])
        
      }
      
      
      if(type=='SEGMENT'){
        lvlItems[[length(lvlItems)+1L]]=create.interval.item(id=id,sampleStart=items[r,'sampleStart'],sampleDur=items[r,'sampleDur'],labels=labels)
      }else if(type=='EVENT'){
        lvlItems[[length(lvlItems)+1L]]=create.event.item(id=id,samplePoint=items[r,'samplePoint'],labels=labels)
      }else{
        lvlItems[[length(lvlItems)+1L]]=create.item(id=id,labels=labels)  
      }
    }
    # add last level
    cLvl[['items']]=lvlItems
    levels[[cLvl[['name']]]]=cLvl
  }
  return(levels)
}

.load.bundle.links.s3 <-function(dbUUID,sessionName,bundleName){
  lksQ=paste0("SELECT * FROM links WHERE db_uuid='",dbUUID,"' AND session='",sessionName,"' AND bundle='",bundleName,"'")
  linksDf=dbGetQuery(emuDBs.con,lksQ)
  nrows=nrow(linksDf)
  
  #links=vector(mode='list',length=nrows)
  links=list()
  if(nrows>0){
    for(row in 1:nrows){
      link=list()
      link[['fromID']]=linksDf[row,'fromID']
      
      link[['toID']]=linksDf[row,'toID']
      lbl=linksDf[row,'label']
      if(!is.null(lbl) && !is.na(lbl)){
        link[['label']]=lbl
      }
      links[[row]]=link
      
    }
  }
  return(links)
}




convert.bundle.links.to.data.frame <-function(links){
  df=data.frame(stringsAsFactors=FALSE)
  row=0
  for(lk in links){
      row=row+1L
      df[row,'fromID']=lk[['fromID']]
      df[row,'toID']=lk[['toID']]
      lbl=lk[['label']]
      if(!is.null(lbl)){
        df[row,'label']=lbl
      }
  }
  invisible(df)
}



## Returns bundle as S3 object
## 
## @param db database
## @param sessionName sessionName
## @param bundleName name of bundle
## @return bundle in S3 format
## @author Klaus Jaensch
## @keywords emuDB database schema Emu bundle
## 
get.bundle <- function(dbName=NULL,sessionName,bundleName,dbUUID=NULL){
  
  dbUUID=.get.database.uuid(name = dbName,uuid = dbUUID)
  b=.load.bundle.DBI(dbUUID,sessionName,bundleName)
  b[['levels']]=.load.bundle.levels.s3(dbUUID,sessionName,bundleName)
  b[['links']]=.load.bundle.links.s3(dbUUID,sessionName,bundleName)
  return(b)
}

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

## Converts bundle in data frame format to S3 format
## 
## @param db database
## @param bundle bundle stub
## @return bundle in S3 format
## @author Klaus Jaensch
## @keywords emuDB database DBconfig Emu bundle
## 
get.bundle.s3 <- function(db,bundle){
  
  schema=db[['DBconfig']]
  bName=bundle[['name']]
  sName=bundle[['sessionName']]
  # convert levels to s3
  bundle[['levels']]=get.bundle.levels.s3(db,sName,bName)
  bundle[['itemsDataFrame']]=NULL
  # convert links
  bundle[['links']]=get.bundle.links.s3(db,sName,bName)
  bundle[['linksDataFrame']]=NULL
  return(bundle)
}



emuDB.print.bundle <- function(utt){
  cat("code=",utt[['name']],"\n")
  cat("signalurls:\n")
  for(mf in utt[['signalpaths']]){
    print(mf) 
  }
  cat("levels:\n")
  for(a in utt[['levels']]){
    print(a) 
  }
}

emuDB.session <- function(name,path=NULL,bundles=list){
  o <- list(name=name,path=path,bundles=bundles)
  class(o) <- 'emuDB.session'
  invisible(o)
}

is.relative.file.path<-function(nativeFilePathStr,forRunningPlatform=FALSE){
  if(forRunningPlatform){
    if(.Platform[['OS.type']]=='unix'){
      if(.Platform[['file.sep']]==substr(nativeFilePathStr,1,1)){
        # UNIX: "/dir/file"
        # absolute path
        return(FALSE)
      }
    }else if(.Platform[['OS.type']]=='windows'){
      #See http://msdn.microsoft.com/en-us/library/windows/desktop/aa365247%28v=vs.85%29.aspx
      if(substr(nativeFilePathStr,1,2)=='\\'){
        # fully qualified MS UNC path (is this supported with R?): \\samba\bla
        return(FALSE)
      }else if(grepl('^[A-Z,a-z][:]',nativeFilePathStr)){
        # fully qualified drive path: C:\Users\bla
        return(FALSE)
      }else if(.Platform[['file.sep']]==substr(nativeFilePathStr,1,1)){
        # Windows: "\dir\file"
        # absolute path
        return(FALSE)
      }
    }
  }else{
    if(grepl('^[A-Z,a-z][:]',nativeFilePathStr)){
      return(FALSE)
    }
    if(grepl('^[\\]',nativeFilePathStr)){
      return(FALSE)
    }
    if(grepl('^/',nativeFilePathStr)){
      return(FALSE)
    }
    
  }
  return(TRUE)
}

extractTrackdata <- function(db=NULL,segmentList=NULL,trackName=NULL){
  schema=db[['DBconfig']]
  signalExt=NULL
  for(tr in schema[['tracks']]){
    if(tr[['name']]==trackName){
      signalExt=tr[['fileExtension']]
    }
  }
  signalExtPatt=paste0('[.]',signalExt,'$')
  currentUtt=''
  currentAsspObj=NULL
  utts=segmentList[['utts']]
  
  index <- matrix(ncol=2, nrow=length(utts))
  colnames(index) <- c("start","end")
  
  ftime <- matrix(ncol=2, nrow=length(utts))
  colnames(ftime) <- c("start","end")
  
  data <- NULL
  origFreq <- NULL
  
  #########################
  # LOOP OVER UTTS
  curIndexStart = 1
  for (i in 1:length(utts)){
   
    un=segmentList[['utts']][[i]]
    if(currentUtt!=un){
      #cat("Utt: ",un,"\n")
      u=get.bundle(db,un)
      for(sp in u[['signalpaths']]){ 
        if(length(grep(signalExtPatt,sp))==1){
          #cat("Signal path: ",sp,"\n")
          currentAsspObj=read.AsspDataObj(sp)
        }
      }
    }
    # we should have the corresponding (complete) ASSP data obj for the segment here
    completeData=currentAsspObj[[trackName]] 
    ncols=ncol(completeData) 
    if(is.null(data)){
      data <- matrix(ncol=ncols, nrow=0)
    }
    
    sampleRate=attr(currentAsspObj,"sampleRate")
    #cat("Cols: ",ncols,"\n")
    origFreq <- attr(currentAsspObj, "origFreq")
    
    curStart <- segmentList[['start']][i]
    curEnd <- segmentList[['end']][i]
  
    fSampleRateInMS <- (1/sampleRate)*1000
    fStartTime <- attr(currentAsspObj,"startTime")*1000
    #cat("Seq: ",fStartTime, curEnd, fSampleRateInMS,"\n")
    timeStampSeq <- seq(fStartTime, curEnd, fSampleRateInMS)
    ###########################################
    # search for first item larger than start time
    breakVal <- -1
    for (j in 1:length(timeStampSeq)){
      if (timeStampSeq[j] >= curStart){
        breakVal <- j
        break
      }
    }
    curStartDataIdx <- breakVal
    curEndDataIdx <- length(timeStampSeq)
    
    ####################
    # set index and ftime
    curIndexEnd <- curIndexStart+curEndDataIdx-curStartDataIdx
    index[i,] <- c(curIndexStart, curIndexEnd)
    ftime[i,] <- c(timeStampSeq[curStartDataIdx], timeStampSeq[curEndDataIdx])
    
    #############################
    # calculate size of and create new data matrix
    #tmpData <- eval(parse(text=paste("curDObj$",colName,sep="")))
    
    
    rowSeq <- seq(timeStampSeq[curStartDataIdx],timeStampSeq[curEndDataIdx], fSampleRateInMS) 
    curData <- matrix(ncol=ncol(completeData), nrow=length(rowSeq))
    colnames(curData) <- paste("T", 1:ncol(curData), sep="")
    rownames(curData) <- rowSeq
    curData[,] <- completeData[curStartDataIdx:curEndDataIdx,] 
    
    ##############################
    # Append to global data matrix app
    data <- rbind(data, curData)
    
    curIndexStart <- curIndexEnd+1
    
    curDObj = NULL
  }
  ########################################
  #convert data, index, ftime to trackdata
  FileExtAndtrackname=paste0(signalExt,':',trackName)
  myTrackData <- as.trackdata(data, index=index, ftime, FileExtAndtrackname)
  
  if(any(trackName %in% c("dft", "css", "lps", "cep"))){
    if(!is.null(origFreq)){
      attr(myTrackData[['data']], "fs") <- seq(0, origFreq/2, length=ncol(myTrackData[['data']]))
      class(myTrackData[['data']]) <- c(class(myTrackData[['data']]), "spectral")
    }else{
      stop("no origFreq entry in spectral data file!")
    }
  }
  
  #if(!is.null(OnTheFlyFunctionName)){
  #  close(pb)
  #}
  
  return(myTrackData)
}




set.list.names <-function(list,nameProperty){
  elemNames=c()
  for(le in list){
    name=le[[nameProperty]]
    elemNames=c(elemNames,name)
  }
  names(list)<-elemNames
  return(list)
  
}

apply.class<-function(val,path,class){
  if(is.null(val)){
    return(NULL)
  }
  if(is.null(path)){
    class(val)<-c(class,class(val))
  }else{
    pLen=length(path)
    if(pLen==0){
      class(val)<-c(class,class(val))
    }else{
      
      pathElem=path[1]
      restpath=c()
      
      if(pLen>1){
        restpath=path[2:pLen]
      }
      if(pathElem=='*'){
        newVal=list()
        for(ch in val){
          newVal[[length(newVal)+1]]=apply.class(ch,restpath,class)
        }
        val=newVal
      }else{
        val[[pathElem]]=apply.class(val[[pathElem]],restpath,class)
      }
    }
  }
  return(val)
}



.update.transient.schema.values<-function(schema){
  # get max label array size
#   maxLbls=0
#   for(lvlDef in schema[['levelDefinitions']]){
#     attrCnt=length(lvlDef[['attributeDefinitions']])
#     if(attrCnt > maxLbls){
#       maxLbls=attrCnt
#     }
#   }
  schema[['maxNumberOfLabels']]=max(sapply(schema[['levelDefinitions']], function(ld) { length(ld[['attributeDefinitions']])} ))
  return(schema)
}

# persistent filters for bundle annotations
# Transient properties which are not stored to JSON
emuR.persist.filters.bundle=list()
emuR.persist.filters.bundle[[1]]=c('dbUUID')
emuR.persist.filters.bundle[[2]]=c('files')
emuR.persist.filters.bundle[[3]]=c('signalpaths')
emuR.persist.filters.bundle[[4]]=c('mediaFilePath')
emuR.persist.filters.bundle[[5]]=c('legacyBundleID')
emuR.persist.filters.bundle[[6]]=c('sessionName')
emuR.persist.filters.bundle[[7]]=c('levels','*','sampleRate')
emuR.persist.filters.bundle[[8]]=c('session')


## Store EMU database bundle to file
## 
## @param db EMU database (in R workspace)
## @param bundle the bundle to save
## @import jsonlite
## @keywords emuDB database Emu 
## 
store.bundle.annotation <- function(dbName=NULL,bundle,dbUUID=NULL){
  db=get.database(uuid = dbUUID,name=dbName)
  dbUUID=db[['DBconfig']][['UUID']]
  dbDir=db[['basePath']]
  # check target dir
  if(!file.exists(dbDir)){
   stop(dbDir," does not exist!")
  }
  sessionName=bundle[['session']]
  bName=bundle[['name']]
  #cat("Store bundle ",bName,"\n")
  #for(s in db[['sessions']]){
  .initialize.DBI.database(createTables=FALSE)
  cat("\nItems br: ",nrow(dbReadTable(emuDBs.con,'items')),"\n")
  cat("Links br: ",nrow(dbReadTable(emuDBs.con,'links')),"\n")
  cat("LinksExt br: ",nrow(dbReadTable(emuDBs.con,'linksExt')),"\n")
  .remove.bundle.annot.DBI(dbUUID=dbUUID,bundle=bundle)
  cat("Items ar: ",nrow(dbReadTable(emuDBs.con,'items')),"\n")
  cat("Links ar: ",nrow(dbReadTable(emuDBs.con,'links')),"\n")
  cat("LinksExt ar: ",nrow(dbReadTable(emuDBs.con,'linksExt')),"\n")
  .store.bundle.annot.DBI(dbUUID=dbUUID,bundle=bundle)
  cat("Items as: ",nrow(dbReadTable(emuDBs.con,'items')),"\n")
  cat("Links as: ",nrow(dbReadTable(emuDBs.con,'links')),"\n")
  cat("Links tmp before build rl: ",nrow(dbReadTable(emuDBs.con,'linksTmp')),"\n")
  #build.redundant.links.all(database = db,sessionName=sessionName,bundleName=bName)
  build.redundant.links.all(database = db)
  res<-dbSendQuery(emuDBs.con,'DELETE FROM linksExt')
  dbClearResult(res)
  #linksTmp=dbReadTable(emuDBs.con,'linksTmp')
  cat("Links tmp after build rl: ",nrow(dbReadTable(emuDBs.con,'linksTmp')),"\n")
  cat("Links ext after build rl: ",nrow(dbReadTable(emuDBs.con,'linksExt')),"\n")
  calculate.postions.of.links()
  cat("Links tmp after build cl: ",nrow(dbReadTable(emuDBs.con,'linksTmp')),"\n")
  cat("Links ext after build cl: ",nrow(dbReadTable(emuDBs.con,'linksExt')),"\n")
  
      # persist to filesystem
      # TODO error handling
      sessDirNm=paste0(sessionName,session.suffix)
      sessPth=file.path(dbDir,sessDirNm)
      
      bDirNm=paste0(bName,bundle.dir.suffix)
      bndlPth=file.path(sessPth,bDirNm)
      
      bndlFileNm=paste0(bName,bundle.annotation.suffix,'.json')
      bndFilePth=file.path(bndlPth,bndlFileNm)
      pFilter=emuR.persist.filters.bundle
      bp=marshal.for.persistence(bundle,pFilter)
      pbpJSON=jsonlite::toJSON(bp,auto_unbox=TRUE,force=TRUE,pretty=TRUE)
      writeLines(pbpJSON,bndFilePth)
      #db=move.bundle.levels.to.data.frame(db=db,bundle=bundle,replace=TRUE)

      #db=move.bundle.links.to.data.frame(db=db,bundle=bundle,replace=TRUE)
      
      
  return(bundle)
}

bundle.iterator<-function(db,apply){
  for(s in db[['sessions']]){
    sessionName=s[['name']]
    for(b in s[['bundles']]){
      db=apply(db,b)
    }
  }
  return(db)
}

create.emuDB<-function(name,targetDir,mediaFileExtension='wav'){
  path=file.path(targetDir,name)
  dbConfig=create.schema.databaseDefinition(name=name,mediafileExtension = mediaFileExtension)
  db=create.database(name=name,basePath=path,DBconfig = dbConfig)
  store.emuDB(db,targetDir)
  db=load.emuDB(path)
  return(db)
}

add.bundle<-function(db,sessionName,bundle){
  db[['sessionName']]
}

# Add level definition to EMU database
# 
# @param db EMU database object
# @param levelDefinition
# @return object of class emuDB
# @author Klaus Jaensch
# @export
# @keywords emuDB database schema Emu 
add.levelDefinition<-function(db,levelDefinition){
  # check if level definition (name) already exists 
  for(ld in db[['DBconfig']][['levelDefinitions']]){
    if(ld[['name']]==levelDefinition[['name']]){
      stop("Level definition:",levelDefinition[['name']]," already exists in database ",db[['name']])
    }
  }
  # add
  db[['DBconfig']][['levelDefinitions']][[length(db[['DBconfig']][['levelDefinitions']])+1]]=levelDefinition

  # update transient values
  db[['DBconfig']]=.update.transient.schema.values(db[['DBconfig']])
  
  # store to disk
  .store.schema(db)
  
  # add levels to existing bundles
  # not required!!
  
  #db=bundle.iterator(db,function(db,b){
  #  bs3=get.bundle(sessionName,b[['name']])
  #  bs3[['levels']][[levelDefinition[['name']]]]=create.bundle.level(name=levelDefinition[['name']],type = levelDefinition[['type']],items = list())
  #})
  
  return(db)
}

remove.levelDefinition<-function(db,levelDefinitionName){
  # check if level definition (name)  exists 
  if(!any(sapply(db[['DBconfig']][['levelDefinitions']],function(ld) ld[['name']]==levelDefinitionName))){
    stop("Level definition:",levelDefinitionName," does not exist in database ",db[['name']])
  }
  # check if level is referenced by link defintion
  for(lkd in db[['DBconfig']][['linkDefinitions']]){
    if(lkd[['superlevelName']]==levelDefinitionName |  lkd[['sublevelName']]==levelDefinitionName){
      lkdStr=toString(lkd)
      stop("Cannot remove level definition ",levelDefinitionName,". It is referenced by link definition: ",lkdStr)
    }
  }
  
  
  # check if level is empty
  itsSelector=(db[['items']][['level']]==levelDefinitionName)
  lvlIts=db[['items']][itsSelector,]
  if(nrow(lvlIts)>0){
    stop("Level is not empty. Remove items first to delete level ",levelDefinitionName)
  }
  
  # do removal
  newLvlDefs=list()
  for(lvlDef in db[['DBconfig']][['levelDefinitions']]){
    if(lvlDef[['name']]!=levelDefinitionName){
      newLvlDefs[[length(newLvlDefs)+1]]=lvlDef
    }
  }
  db[['DBconfig']][['levelDefinitions']]=newLvlDefs
  
  # update transient values
  db[['DBconfig']]=.update.transient.schema.values(db[['DBconfig']])
  
  # store to disk
  .store.schema(db)
  
  return(db)
}

add.linkDefinition<-function(db,linkDefinition){
  # check existence of levels
  
  superFound=FALSE
  for(ld in db[['DBconfig']][['levelDefinitions']]){
    if(ld[['name']]==linkDefinition[['superlevelName']]){
      superFound=TRUE
    }
  }
  if(!superFound){
    stop("Super level ",linkDefinition[['superlevelName']]," not found!")
  }
  
  subFound=FALSE
  for(ld in db[['DBconfig']][['levelDefinitions']]){
    if(ld[['name']]==linkDefinition[['sublevelName']]){
      subFound=TRUE
    }
   
  }
  if(!subFound){
    stop("Sub level ",linkDefinition[['sublevelName']]," not found!")
  }
  
  # check if link definition already exists 
  for(lkd in db[['DBconfig']][['linkDefinitions']]){
    if(lkd[['superlevelName']]==linkDefinition[['superlevelName']] & lkd[['sublevelName']]==linkDefinition[['sublevelName']]){
      stop("Link definition:",lkd," already exists in database ",db[['name']])
    }
  }
  
  
  
  # add
  db[['DBconfig']][['linkDefinitions']][[length(db[['DBconfig']][['linkDefinitions']])+1]]=linkDefinition
  
  # store to disk
  .store.schema(db)
  return(db)
}


##' Import media files to EMU database
##' @description Import media files to EMU database
##' @param db object of class emuDB
##' @param dir directory containing mediafiles or session directories
##' @param targetSessionName name of session in which to create the new bundles 
##' @return modified database object
##' @author Klaus Jaensch
##' @import sqldf stringr
##' @keywords emuDB database Emu
##' @examples
##' \dontrun{
##' ## Add mediafiles from directory
##' 
##'  import.mediaFiles(emuDB,'0000',dir="/data/mymedia")
##' 
##' }
"import.mediaFiles"<-function(db,dir,targetSessionName='0000'){
  UseMethod("import.mediaFiles")
}

"import.mediaFiles.emuDB"<-function(db,dir,targetSessionName='0000'){
  dbClass=class(db)
  if(dbClass=='emuDB'){

    dbCfg=db[['DBconfig']]
    if(is.null(dbCfg[['mediaFileExtension']])){
      pattern=NULL
      #stop("The DB has no media file extension defined.")
    }else{
      pattern=paste0('.*[.]',dbCfg[['mediaFileExtension']],'$')
    }
    mfList=list.files(dir,pattern=pattern)
    if(length(mfList)>0){
      # create session dir and session list object if required
      sessDir=file.path(db[['basePath']],paste0(targetSessionName,session.suffix))
      if(!file.exists(sessDir)){
        dir.create(sessDir)
      }
      if(is.null(db[['sessions']][[targetSessionName]])){
        db[['sessions']][[targetSessionName]]=emuDB.session(name=targetSessionName,path = sessDir,bundles=list())
      }
    }
    mediaAdded=FALSE
    for(mf in mfList){
      mfFullPath=file.path(dir,mf)
      bundleName=sub('[.][^.]*$','',mf)
      
      bundleDir=file.path(sessDir,paste0(bundleName,bundle.dir.suffix))
      dir.create(bundleDir)
      newMediaFileFullPath=file.path(bundleDir,mf)
      file.copy(from = mfFullPath,to=newMediaFileFullPath)
      
      pfAssp=read.AsspDataObj(newMediaFileFullPath,0,4000)
      sampleRate=attr(pfAssp,'sampleRate')
      b=create.bundle(name = bundleName,sessionName = targetSessionName,mediaFilePath = newMediaFileFullPath,annotates=mf,sampleRate=sampleRate)
      db[['sessions']][[targetSessionName]][['bundles']][[bundleName]]=b
      # TODO generate empty annotation and SSFFtracks if required
      annoAdded=FALSE
      for(ld in dbCfg[['levelDefinitions']]){
       
        b[['levels']]=list()
        b[['levels']][[ld[['name']]]]=create.bundle.level(name=ld[['name']],type = ld[['type']],items = list())
        
        ## TODO TEST only
        #labelAttrs=list(list(name=ld[['name']],value='Test Huhu!'))
        #b[['levels']][[ld[['name']]]][['items']][[1]]=create.interval.item(id = 1,sampleStart = 0,sampleDur = 50000,labels = labelAttrs )
        #db=move.bundle.levels.to.data.frame(db,b)
        annoAdded=TRUE
      }
      if(annoAdded){
        db=store.bundle.annotation(db,b)
      }
      mediaAdded=TRUE
    }
    
    perspectives=dbCfg[['EMUwebAppConfig']][['perspectives']]
    # create an EMUwebapp default perspective if media has been added 
    if(mediaAdded & (is.null(perspectives) | length(perspectives)==0)){
      sc=create.EMUwebAppConfig.signalCanvas(order=c("OSCI","SPEC"),assign=list(),contourLims=list())
      defPersp=create.EMUwebAppConfig.perspective(name='default',signalCanvases=sc,levelCanvases=list(order=list()),twoDimCanvases=list(order=list()))
      db[['DBconfig']][['EMUwebAppConfig']][['perspectives']]=list(defPersp)
    }
  
    return(db)
  }else{
    NextMethod()
  }
}

##' Store EMU database to directory
##' 
##' @details 
##' options is a list of key value pairs:
##' rewriteSSFFTracks if TRUE rewrite SSF tracks instead of file copy to get rid of big endian encoded SSFF files (SPARC), default: FALSE
##' ignoreMissingSSFFTrackFiles if TRUE missing SSFF track files are ignored, default: FALSE
##' symbolicLinkSignalFiles if TRUE signal files are symbolic linked instead of copied. Implies: rewriteSSFFTracks=FALSE, Default: FALSE
##' 
##' @param db EMU database (in R workspace)
##' @param targetDir target directory
##' @param options list of options
##' @param showProgress show progress bar
##' @author Klaus Jaensch
##' @import stringr uuid jsonlite
##' @export
##' @keywords emuDB database Emu
##' @seealso  \code{\link{load.emuDB}}
##' @examples
##' \dontrun{
##' # Store database object ae to directory /homes/mylogin/EMUnew/
##' 
##' store.emuDB(ae,"/homes/mylogin/EMUnew/")
##' 
##' }

store.emuDB <- function(dbName=NULL,targetDir,dbUUID=NULL,options=NULL,showProgress=TRUE){
  # TODO how to handle API level in DBI version?
#   dbApiLevel=db[['apiLevel']]
#   if(is.null(dbApiLevel)){
#     stop("Database API level differs from R package API level: ",apiLevel,"\nPlease reload the database: db=reload(db)")
#   }else if(dbApiLevel!=emuDB.apiLevel){
#     stop("Database API level: ",dbApiLevel," differs from R package API level: ",apiLevel,"\nPlease reload the database: db=reload(db)")
#   }
#   
  # default options
  # ignore missing SSFF track files
  # rewrite SSFF track files
  mergedOptions=list(ignoreMissingSSFFTrackFiles=TRUE,rewriteSSFFTracks=FALSE,symbolicLinkSignalFiles=FALSE)
  if(!is.null(options)){
    for(opt in names(options)){
      mergedOptions[[opt]]=options[[opt]]
    }
  }
  
  progress=0
  # check target dir
  if(file.exists(targetDir)){
    tdInfo=file.info(targetDir)
    if(!tdInfo[['isdir']]){
      stop(targetDir," exists and is not a directory.")
    }
  }else{
    # create target dir
    dir.create(targetDir)
  }
  # load emuDB metadata
  if(is.null(dbUUID)){
    db=.load.emuDB.DBI(name=dbName )
  }else{
    db=.load.emuDB.DBI(uuid = dbUUID)
  }
  # get UUID of DB
  dbUUID=db[['DBconfig']][['UUID']]
  
  # create database dir in targetdir
  pp=file.path(targetDir,db[['name']])
  # check existence
  if(file.exists(pp)){
    stop(pp," already exists.")
  }
 
  dir.create(pp)
  
  
  # set editable
  db[['DBconfig']][['EMUwebAppConfig']][['activeButtons']]=list(saveBundle=TRUE)

  
  # store db schema file
  .store.schema(db,projectDir=pp)
  progress=progress+1L
  
  bundleCount=0
  if(showProgress){
    # calculate bundle count
    #for(s in db[['sessions']]){
    #    sessBundleCount=length(s[['bundles']])
    #    bundleCount=bundleCount+sessBundleCount
    #}
    bundleCount=as.integer(dbGetQuery(emuDBs.con,"SELECT count(*) FROM bundle"))
    cat("INFO: Storing EMU database containing",bundleCount,"bundles...\n")
    pb=txtProgressBar(min=0,max=bundleCount+1L,style=3)
    setTxtProgressBar(pb,progress)
  }
  
  # convert sessions
  ss=.load.sessions.DBI(dbUUID = dbUUID)
  ssNms=ss[['name']]
  for(sn in ssNms){
    #cat(targetDir,s$name,"\n")
    sDir=paste0(sn,session.suffix)
    sfp=file.path(pp,sDir)
    #cat(targetDir,s$name,sfp,"\n")
    dir.create(sfp)
    # bundles
    
    bnms=.load.bundle.names.DBI(dbUUID,sn)
    for(bn in bnms){
      
      b=get.bundle(sessionName=sn,bundleName=bn,dbUUID=dbUUID)
      bDir=paste0(b[['name']],bundle.dir.suffix)
      bfp=file.path(sfp,bDir)
      dir.create(bfp)
      pFilter=emuR.persist.filters.bundle
      bp=marshal.for.persistence(b,pFilter)
  
      for(sf in b[['signalpaths']]){
        #cat("Signalpath: ",sf,"\n")
        bn=basename(sf)
        nsfp=file.path(bfp,bn)
        # check if SSFF type
        isSSFFFile=FALSE
        for(ssffTrDef in db[['DBconfig']][['ssffTrackDefinitions']]){
          ssffTrFileExt=ssffTrDef[['fileExtension']]
          fileExtPatt=paste0('[.]',ssffTrFileExt,'$')
          if(length(grep(fileExtPatt,sf))==1){
            isSSFFFile=TRUE
            break
          }
        }
        if(file.exists(sf)){
          if(mergedOptions[['symbolicLinkSignalFiles']]){
            file.symlink(from=sf,to=nsfp)
          }else if(mergedOptions[['rewriteSSFFTracks']] && isSSFFFile){
            # is SSFF track
            # read/write instead of copy to get rid of big endian encoded SSFF files (SPARC)
            pfAssp=read.AsspDataObj(sf)
            write.AsspDataObj(pfAssp,nsfp)
            #cat("Rewritten SSFF: ",sf," to ",nsfp,"\n")
          }else{
            # media file (likely a wav file)
            file.copy(from=sf,to=nsfp)
            #cat("Copied: ",sf," to ",nsfp,"\n")
          }
        }else{
          if(!mergedOptions[['ignoreMissingSSFFTrackFiles']]){
            stop("SSFF track file :'",sf,"' does not exist!")
          }
        }
      }
      
      # and metadata (annotations)
      ban=str_c(b[['name']],bundle.annotation.suffix,'.json')
      baJSONPath=file.path(bfp,ban)
      pbpJSON=jsonlite::toJSON(bp,auto_unbox=TRUE,force=TRUE,pretty=TRUE)
      writeLines(pbpJSON,baJSONPath)
      
      progress=progress+1L
      if(showProgress){
        setTxtProgressBar(pb,progress)
      }
    }
     
  }
  if(showProgress){
    setTxtProgressBar(pb,progress)
    cat("\n")
  }
  
}

calculate.postions.of.links<-function(){
 
  # for all position related functions we need to calculate the sequence indices of dominated items grouped to one dominance item 
  # Extend links table with sequence index of the targeted (dominated) item
  #links2=sqldf("SELECT k.*,i.seqIdx FROM links k,items i WHERE i.bundle=k.bundle AND k.toID=i.itemID")
  
  # since version 2.8.x of sqlite the query is very slow without indices
  #res<-dbSendQuery(emuDBs.con,'CREATE INDEX items_idx2 ON items(db_uuid,session,bundle,level,itemID,seqIdx)')
  #dbClearResult(res)
 
  res<-dbSendQuery(emuDBs.con,"DELETE FROM linksExtTmp")
  dbClearResult(res)
  #print(dbReadTable(emuDBs.con,'linksTmp'))
  res<-dbSendQuery(emuDBs.con,"INSERT INTO linksExtTmp(db_uuid,session,bundle,fromID,toID,seqIdx,toLevel,type,label) SELECT k.db_uuid,k.session,k.bundle,k.fromID,k.toID,i.seqIdx,i.level AS toLevel,i.type,NULL AS label FROM linksTmp k,items i WHERE i.db_uuid=k.db_uuid AND i.session=k.session AND i.bundle=k.bundle AND k.toID=i.itemID")
  dbClearResult(res)
  cat(nrow(dbReadTable(emuDBs.con,'linksExtTmp')),"\n")
  #dbSendQuery(emuDBs.con,"DELETE FROM linksExt")
  # extend links table with relative sequence index
  
  res<-dbSendQuery(emuDBs.con,"INSERT INTO linksExtTmp2(db_uuid,session,bundle,seqIdx,fromID,toID,toLevel,type,label,toSeqIdx) SELECT k.db_uuid,k.session,k.bundle,k.seqIdx,k.fromID,k.toID,k.toLevel,k.type,k.label,k.seqIdx-(SELECT MIN(m.seqIdx) FROM linksExtTmp m WHERE m.fromID=k.fromID AND m.db_uuid=k.db_uuid AND m.session=k.session AND m.bundle=k.bundle AND k.toLevel=m.toLevel GROUP BY m.db_uuid,m.session,m.bundle,m.fromID,m.toLevel) AS toSeqIdx FROM linksExtTmp k")
  dbClearResult(res)
  cat(nrow(dbReadTable(emuDBs.con,'linksExtTmp2')),"\n")
  res<-dbSendQuery(emuDBs.con,"DELETE FROM linksExtTmp")
  dbClearResult(res)
  # Add length of dominance group sequence
  #links3IdxSql='CREATE INDEX links3_idx ON links3(session,bundle,fromID,toID,toLevel,type)'
  res<-dbSendQuery(emuDBs.con,"INSERT INTO linksExt(db_uuid,session,bundle,seqIdx,fromID,toID,toSeqIdx,toLevel,type,label,toSeqLen) SELECT k.db_uuid,k.session,k.bundle,k.seqIdx,k.fromID,k.toID,k.toSeqIdx,k.toLevel,k.type,k.label,(SELECT MAX(m.seqIdx)-MIN(m.seqIdx)+1 FROM linksExt m WHERE m.fromID=k.fromID AND m.db_uuid=k.db_uuid AND m.session=k.session AND m.bundle=k.bundle AND k.toLevel=m.toLevel GROUP BY m.db_uuid,m.session,m.bundle,m.fromID,m.toLevel) AS toSeqLen FROM linksExtTmp2 k")
  dbClearResult(res)
  cat(nrow(dbReadTable(emuDBs.con,'linksExt')),"\n")
  res<-dbSendQuery(emuDBs.con,"DELETE FROM linksExtTmp2")
  dbClearResult(res)
  #res<-dbSendQuery(emuDBs.con,"INSERT INTO linksExt SELECT * FROM linksExtTmp")
  #dbClearResult(res)
  
}

##' Load EMU database
##' 
##' @param databaseDir directory of the EMU database
##' @param verbose be verbose
##' @return object of class emuDB
##' @author Klaus Jaensch
##' @import jsonlite
##' @export
##' @keywords emuDB database schema Emu 
##' @examples
##' \dontrun{
##' ## Load database 'ae' in directory /homes/mylogin/EMUnew/ae
##' 
##' ae=load.emuDB("ae","/homes/mylogin/EMUnew/ae")
##' 
##' }
load.emuDB <- function(databaseDir,verbose=TRUE){
  progress=0

  # check database dir
  if(!file.exists(databaseDir)){
    stop("Database dir ",databaseDir," does not exist!")
  }
    dbDirInfo=file.info(databaseDir)
    if(!dbDirInfo[['isdir']]){
      stop(databaseDir," exists, but is not a directory.")
    }
 
 
  # load db schema file
  dbCfgPattern=paste0('.*',database.schema.suffix,'$')
  dbCfgFiles=list.files(path=databaseDir,dbCfgPattern)
  dbCfgFileCount=length(dbCfgFiles)
  if(dbCfgFileCount==0){
    stop("Could not find global DB config JSON file (regex pattern: ",dbCfgPattern,") in ",databaseDir)
  }
  if(dbCfgFileCount>1){
    stop("Found multiple global DB config JSON files (regex pattern: ",dbCfgPattern,") in ",databaseDir)
  }
  
  dbCfgPath=file.path(databaseDir,dbCfgFiles[[1]])
  if(!file.exists(dbCfgPath)){
    stop("Could not find database info file: ",dbCfgPath,"\n")
  }
  # load DBconfig
  schema=load.emuDB.DBconfig(dbCfgPath)
  # set transient values
  schema=.update.transient.schema.values(schema)
  # create db object
  db=create.database(name = schema[['name']],basePath = normalizePath(databaseDir),DBconfig = schema)
  
  .initialize.DBI.database()
  # TODO check if already exists
  
#   createDbSql=paste0("INSERT INTO emuDB(uuid,name) VALUES('",schema[['UUID']],"','",schema[['name']],"')")
#   res<-dbSendQuery(emuDBs.con,createDbSql)
#   dbClearResult(res)
 
  .store.emuDB.DBI(db)
  if(verbose){
    cat("INFO: Loading EMU database from ",databaseDir,"...\n")
  }
  

  sessions=list()
  # sessions
  #sessPattern=paste0('^[0-9]{4}',session.suffix,'$')
  # if legacy EMU uses globpattern in path directive session name can be an arbitrary string
  sessPattern=paste0('^.*',session.suffix,'$')
  sessDirs=dir(databaseDir,pattern=sessPattern)
 
  # calculate bundle count
  bundleCount=0
  for(sd in sessDirs){
    absSd=file.path(databaseDir,sd)
    bundleDirs=dir(absSd,pattern=paste0('.*',bundle.dir.suffix,'$'))
    bundleCount=bundleCount+length(bundleDirs)
  }
  
  # progress distribution  
  
  # progress part to build a data frame 1%
  ppBuildDataFrame=as.integer(bundleCount/1L)
  # thress data frames items, labels and links
  ppBuildDataFrames=ppBuildDataFrame*3L
  # progress part to build redundant links 10%
  ppBuildRedLinks=as.integer(bundleCount/10L)
  # progress part to calaculate ext links 10%
  ppBuildExtLinks=as.integer(bundleCount/10L)
  pMax=bundleCount+ppBuildDataFrames+ppBuildRedLinks+ppBuildExtLinks
  if(pMax==0){
    pMax=1
  }
  if(verbose){ 
    pb=txtProgressBar(min=0L,max=pMax,style=3)
    setTxtProgressBar(pb,progress)
  }
  bundleNames=character(bundleCount)
  db[['bundleNamesUnique']]=TRUE
  bundleIdx=0L
  for(sd in sessDirs){
    sessionName=gsub(pattern = paste0(session.suffix,'$'),replacement = '',x = sd)
    bundles=list()
    absSd=file.path(databaseDir,sd)
    bundleDirs=dir(absSd,pattern=paste0('.*',bundle.dir.suffix,'$'))
    # bundles
    for(bd in bundleDirs){
      #cat("Loading bundle ",bd,"\n")
      absBd=file.path(absSd,bd)
      #b=create.bundle(name=bd)
      # bundle files
      bName=gsub(paste0(bundle.dir.suffix,'$'),'',bd)
      # bundle files must start with bundle name
      bundleFilePattern=paste0('^',bName,'.*$')
      bfs=list.files(absBd,pattern=bundleFilePattern)
      
      signalpaths=list()
      bundle=NULL
      for(bf in bfs){
        annotFile=paste0(bName,bundle.annotation.suffix,'.json')
        absBf=file.path(absBd,bf)
        
        if(bf==annotFile){
          
          # and metadata (annotations)
          annoJSONLns=readLines(absBf,encoding="UTF-8")
          annoJSON=paste(annoJSONLns,collapse='')
          bundle=jsonlite::fromJSON(annoJSON,simplifyVector=FALSE)
          #class(bundle) <- 'emuDB.bundle'
          bundle=as.bundle(bundle)
          namedLevels=set.list.names(bundle[['levels']],'name')
          bundle[['levels']]=namedLevels
          bundle[['mediaFilePath']]=file.path(absBd,bundle[['annotates']])
        }else{
          
          for(ssffTr in schema[['ssffTrackDefinitions']]){
            ssffExt=ssffTr[['fileExtension']]
            ssffFn=paste0(bName,'.',ssffExt)
            
            if(ssffFn==bf){
              signalpaths[[length(signalpaths)+1L]]=absBf
            }
          }
          
        }
      }
      bundle[['db_UUID']]=schema[['UUID']]
      # set session name
      bundle[['session']]=sessionName
      
      # add media file path to signalpaths
      sps=list(bundle[['mediaFilePath']])
      sps=c(sps,signalpaths)
      bundle[['signalpaths']]=sps
      
      schema=db[['DBconfig']]
      #maxLbls=db[['DBconfig']][['maxNumberOfLabels']]
      .store.bundle.DBI(db,bundle)
      .store.bundle.annot.DBI(schema[['UUID']],bundle)
      
      bundle[['levels']]=NULL
      bundle[['links']]=NULL
      bundles[[bName]]=bundle
      
      bundleIdx=bundleIdx+1
      if(db[['bundleNamesUnique']]){
        if(bName %in% bundleNames){
          db[['bundleNamesUnique']]=FALSE
        }else{
          bundleNames[bundleIdx]=bName
        }
      }
      
      progress=progress+1L
      if(verbose){
        setTxtProgressBar(pb,progress)
      }
    }
    sessSuffixPattern=paste0(session.suffix,'$')
    sNm=gsub(sessSuffixPattern,'',sd)
    s=emuDB.session(name=sNm,path=absSd,bundles=bundles)
    .store.session.DBI(schema[['UUID']],sessionName)
    sessions[[sNm]]=s
    
  }
  db[['sessions']]=sessions 
  
  #itemsIdx=db[['itemsIdx']]
  #tmpDf=data.frame(db[['items']],stringsAsFactors = FALSE)
  #db[['items']]=tmpDf[0:itemsIdx,]
  
  db[['items']]=dbReadTable(emuDBs.con,'items')
  progress=progress+ppBuildDataFrame
  if(verbose){
    setTxtProgressBar(pb,progress)
  }

  #labelsIdx=db[['labelsIdx']]
  #tmpLblsDf=data.frame(db[['labels']],stringsAsFactors = FALSE)
  #db[['labels']]=tmpLblsDf[0:labelsIdx,]
  
  #db[['labels']]=dbReadTable(emuDBs.con,'labels')
  progress=progress+ppBuildDataFrame
  if(verbose){
    setTxtProgressBar(pb,progress)
  }

  #linksIdx=db[['linksIdx']]
  #tmpLksdf=data.frame(db[['links']],stringsAsFactors = FALSE)
  
  #db[['links']]=tmpLksdf[0:linksIdx,]
  
  db[['links']]=dbReadTable(emuDBs.con,'links')
 
  progress=progress+ppBuildDataFrame
  if(verbose){
    setTxtProgressBar(pb,progress)
  }

  # assume no redunant links in new format 
  build.redundant.links.all(db)
  progress=progress+ppBuildRedLinks
  if(verbose){
    setTxtProgressBar(pb,progress)
  }
  calculate.postions.of.links()
  #db[['linksExt']]=dbReadTable(emuDBs.con,'linksExt')
  progress=progress+ppBuildExtLinks
  if(verbose){
    setTxtProgressBar(pb,progress)
    
    cat("\n")
  }
  #.destroy.DBI.database()
  return()
  
}


##' Reload EMU database
##' @description Reload an EMU database from disk storage
##' @param db object of class emuDB

##' @return refreshed database object
##' @author Klaus Jaensch
##' @import sqldf stringr
##' @seealso \code{\link{load.emuDB}}
##' @keywords emuDB database query Emu
##' @examples
##' \dontrun{
##' ## Reload database object ae
##' 
##' ae=reload(ae)
##' 
##' }
"reload"<-function(db){
  UseMethod("reload")
}


"reload.emuDB"<-function(db){
  return(load.emuDB(db[['basePath']]))
}
