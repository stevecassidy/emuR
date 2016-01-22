require(stringr)
require(uuid)
require(wrassp)
require(DBI)
require(tidyjson)

# constants

# API level of database object format
# increment this value if the internal database object format changes  
emuDB.apiLevel = 3L

# vars used by testthat tests
testingVars = list(inMemoryCache = F)

#############################################
# file/folder suffixes of emuDB format

emuDB.suffix = '_emuDB'
session.suffix = '_ses'
bundle.dir.suffix = '_bndl'
bundle.annotation.suffix = '_annot'
database.schema.suffix = '_DBconfig.json'
database.cache.suffix = '_emuDBcache.sqlite'

#############################################
# create table / index definitions for DBI

database.DDL.emuDB = 'CREATE TABLE emuDB (
  uuid VARCHAR(36) NOT NULL,
  name TEXT,
  basePath TEXT,
  DBconfigJSON TEXT,
  MD5DBconfigJSON TEXT,
  PRIMARY KEY (uuid)
);'

database.DDL.emuDB_session = 'CREATE TABLE session (
  db_uuid VARCHAR(36),
  name TEXT,
  PRIMARY KEY (db_uuid,name),
  FOREIGN KEY (db_uuid) REFERENCES emuDB(uuid)
);'

database.DDL.emuDB_bundle = 'CREATE TABLE bundle (
  db_uuid VARCHAR(36),
  session TEXT,
  name TEXT,
  annotates TEXT,
  sampleRate FLOAT,
  MD5annotJSON TEXT,
  PRIMARY KEY (db_uuid,session,name),
  FOREIGN KEY (db_uuid,session) REFERENCES session(db_uuid,name)
);'

database.DDL.emuDB_items = 'CREATE TABLE items (
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
  PRIMARY KEY (db_uuid,session,bundle,level,itemID,type),
  FOREIGN KEY (db_uuid,session,bundle) REFERENCES bundle(db_uuid,session_name,name)
);'

# Important note:
# The primary key of items contains mor columns then needed to identify a particular item.
# PRIMARY KEY (db_uuid,session,bundle,itemID) would be sufficient but teh extended primary key 
# is necessary to speed up the build.redundnat.links.for.pathes SQL query.
# It did not work to create an index like the one in the comment line below.
# It seems teh query uses always the index of the primary key.
#database.DDL.emuDB_itemsIdx='CREATE UNIQUE INDEX items_level_idx ON items(db_uuid,session,bundle,level,itemID,type)'

database.DDL.emuDB_labels = 'CREATE TABLE labels (
  db_uuid VARCHAR(36),
  session TEXT,
  bundle TEXT,
  itemID INTEGER,
  labelIdx INTEGER,
  name TEXT,
  label TEXT,
  FOREIGN KEY (db_uuid,session,bundle) REFERENCES bundle(db_uuid,session,name)
);'

database.DDL.emuDB_links = 'CREATE TABLE links (
  db_uuid VARCHAR(36) NOT NULL,
  session TEXT,
  bundle TEXT,
  fromID INTEGER,
  toID INTEGER,
  label TEXT,
  FOREIGN KEY (db_uuid,session,bundle) REFERENCES bundle(db_uuid,session,name)
);'
database.DDL.emuDB_linksIdx = 'CREATE INDEX links_idx ON links(db_uuid,session,bundle,fromID,toID)'

database.DDL.emuDB_linksTmp = 'CREATE TABLE linksTmp (
   db_uuid VARCHAR(36) NOT NULL,
  session TEXT,
  bundle TEXT,
  fromID INTEGER,
  toID INTEGER,
  label TEXT,
  FOREIGN KEY (db_uuid,session,bundle) REFERENCES bundle(db_uuid,session,name)
);'
database.DDL.emuDB_linksTmpIdx = 'CREATE INDEX linksTmp_idx ON linksTmp(db_uuid,session,bundle,fromID,toID)'

database.DDL.emuDB_linksExt = 'CREATE TABLE linksExt (
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


database.DDL.emuDB_linksExtIdx = 'CREATE INDEX linksExt_idx ON linksExt(db_uuid,session,bundle,fromID,toID,toLevel,type)'

database.DDL.emuDB_linksExtTmp = 'CREATE TABLE linksExtTmp (
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
database.DDL.emuDB_linksExtTmpIdx = 'CREATE INDEX linksExtTmp_idx ON linksExtTmp(db_uuid,session,bundle,fromID,toID,toLevel,type)'

database.DDL.emuDB_linksExtTmp2 = 'CREATE TABLE linksExtTmp2 (
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

database.DDL.emuDB_linksExtTmpIdx2 = 'CREATE INDEX linksExtTmp2_idx ON linksExtTmp2(db_uuid,session,bundle,fromID,toID,toLevel,type)'

####################################
######### DBI functions ############
####################################

####################################
# init functions (create tables and indices)

initialize_emuDbDBI <- function(emuDBhandle, createTables=TRUE, createIndices=TRUE){
  
  if(createTables & !dbExistsTable(emuDBhandle$connection, 'emuDB')){
    res <- dbSendQuery(emuDBhandle$connection, database.DDL.emuDB)
    dbClearResult(res)
    res <- dbSendQuery(emuDBhandle$connection, database.DDL.emuDB_session) 
    dbClearResult(res)
    res <- dbSendQuery(emuDBhandle$connection, database.DDL.emuDB_bundle) 
    dbClearResult(res)
    res <- dbSendQuery(emuDBhandle$connection, database.DDL.emuDB_items) 
    dbClearResult(res)
    res <- dbSendQuery(emuDBhandle$connection, database.DDL.emuDB_labels) 
    dbClearResult(res)
    res <- dbSendQuery(emuDBhandle$connection, database.DDL.emuDB_links) 
    dbClearResult(res)
    res <- dbSendQuery(emuDBhandle$connection, database.DDL.emuDB_linksExt) 
    dbClearResult(res)
    res <- dbSendQuery(emuDBhandle$connection, database.DDL.emuDB_linksTmp) 
    dbClearResult(res)
    res <- dbSendQuery(emuDBhandle$connection, database.DDL.emuDB_linksExtTmp) 
    dbClearResult(res)
    res <- dbSendQuery(emuDBhandle$connection, database.DDL.emuDB_linksExtTmp2) 
    dbClearResult(res)
    if(createIndices){  
      create_emuDBindicesDBI(emuDBhandle)
    }
  }
}

create_emuDBindicesDBI<-function(emuDBhandle){
  
  res <- dbSendQuery(emuDBhandle$connection, database.DDL.emuDB_linksIdx) 
  dbClearResult(res)
  res <- dbSendQuery(emuDBhandle$connection, database.DDL.emuDB_linksExtIdx) 
  dbClearResult(res)
  res <- dbSendQuery(emuDBhandle$connection, database.DDL.emuDB_linksTmpIdx) 
  dbClearResult(res)
  res <- dbSendQuery(emuDBhandle$connection, database.DDL.emuDB_linksExtTmpIdx) 
  dbClearResult(res)
  res <- dbSendQuery(emuDBhandle$connection, database.DDL.emuDB_linksExtTmpIdx2) 
  dbClearResult(res)
  
}


####################################
# emuDB table DBI functions

add_emuDbDBI <- function(emuDBhandle){
  dbSqlInsert = paste0("INSERT INTO emuDB(uuid,name,basePath,DBconfigJSON,MD5DBconfigJSON) VALUES('", emuDBhandle$UUID, "','", emuDBhandle$dbName, "',NULL,'", "DEPRICATED COLUMN", "', 'DEPRICATED COLUMN'", ")")
  res <- dbSendQuery(emuDBhandle$connection, dbSqlInsert)
  dbClearResult(res)
}

get_emuDbDBI <- function(emuDBhandle){
  query = paste0("SELECT * FROM emuDB WHERE uuid='", emuDBhandle$UUID, "'")
  res <- dbGetQuery(emuDBhandle$connection, query)
  return(res)
}


####################################
# session table DBI functions

add_sessionDBI <- function(emuDBhandle, sessionName){
  insertSessionSql = paste0("INSERT INTO session(db_uuid, name) VALUES('", emuDBhandle$UUID,"','", sessionName, "')")
  res<-dbSendQuery(emuDBhandle$connection, insertSessionSql)
  dbClearResult(res)
}

list_sessionsDBI <- function(emuDBhandle){
  dbs=dbGetQuery(emuDBhandle$connection, paste0("SELECT name FROM session WHERE db_uuid='", emuDBhandle$UUID, "'"))
  return(dbs)
}


remove_sessionDBI <- function(emuDBhandle, sessionName){
  dbGetQuery(emuDBhandle$connection, paste0("DELETE FROM session WHERE ", "db_uuid='", emuDBhandle$UUID, "' AND session='", sessionName, "'"))
}

####################################
# bundle table DBI functions

add_bundleDBI <- function(emuDBhandle, sessionName, name, annotates, sampleRate, MD5annotJSON){
  insertBundleSql = paste0("INSERT INTO bundle(db_uuid, session, name, annotates, sampleRate, MD5annotJSON) VALUES('", 
                           emuDBhandle$UUID, "', '", sessionName, "', '", name, "', '", annotates, "', ", sampleRate, ", '", MD5annotJSON, "')")
  dbGetQuery(emuDBhandle$connection, insertBundleSql)
}

list_bundlesDBI <- function(emuDBhandle){
  bundle = dbGetQuery(emuDBhandle$connection, paste0("SELECT session, name FROM bundle WHERE db_uuid='", emuDBhandle$UUID, "'"))
  return(bundle)
}

remove_bundleDBI <- function(emuDBhandle, sessionName, name){
  dbGetQuery(emuDBhandle$connection, paste0("DELETE FROM bundle WHERE ", "db_uuid='", emuDBhandle$UUID, "' AND session='", sessionName, "' AND name='", name, "'"))
}

# MD5annotJSON
get_MD5annotJsonDBI <- function(emuDBhandle, sessionName, name){
  MD5annotJSON = dbGetQuery(emuDBhandle$connection, paste0("SELECT MD5annotJSON FROM bundle WHERE db_uuid='", emuDBhandle$UUID, "' AND session='", sessionName, "' AND name='", name, "'"))$MD5annotJSON
  return(MD5annotJSON)
}

####################################
# items, links, labels DBI functions

store_bundleAnnotDFsDBI <- function(emuDBhandle, bundleAnnotDFs, sessionName, 
                                    bundleName) {
  
  # insert items table entries (fist exanding it with db_uuid, session and bundle columns)
  bundleAnnotDFs$items = data.frame(db_uuid = emuDBhandle$UUID, 
                                    session = sessionName,
                                    bundle = bundleName,
                                    bundleAnnotDFs$items)
  
  dbWriteTable(emuDBhandle$connection, "items", bundleAnnotDFs$items, append = T)
  
  # insert labels table entries (fist exanding it with db_uuid, session and bundle columns)
  bundleAnnotDFs$labels =  data.frame(db_uuid = emuDBhandle$UUID, 
                                      session = sessionName,
                                      bundle = bundleName,
                                      bundleAnnotDFs$labels)
  
  dbWriteTable(emuDBhandle$connection, "labels", bundleAnnotDFs$labels, append = T)
  
  # insert links table entries (fist exanding it with db_uuid, session and bundle columns)
  bundleAnnotDFs$links =  data.frame(db_uuid = emuDBhandle$UUID,
                                     session = sessionName,
                                     bundle = bundleName,
                                     bundleAnnotDFs$links,
                                     label = NA)
  
  dbWriteTable(emuDBhandle$connection, "links", bundleAnnotDFs$links, append = T)
  
}

load_bundleAnnotDFsDBI <- function(emuDBhandle, sessionName, bundleName){
  
  DBconfig = load_DBconfig(emuDBhandle)
  
  # meta infos
  annotates = paste0(bundleName, ".", DBconfig$mediafileExtension)
  sampleRateQuery = paste0("SELECT sampleRate FROM bundle WHERE db_uuid='", emuDBhandle$UUID, "' AND session='", sessionName, "' AND name='", bundleName,"'")
  sampleRate = dbGetQuery(emuDBhandle$connection, sampleRateQuery)$sampleRate
  
  # items
  itemsQuery = paste0("SELECT itemID, level, type, seqIdx, sampleRate, samplePoint, sampleStart, sampleDur  FROM items WHERE db_uuid='", 
                      emuDBhandle$UUID, "' AND session='", sessionName, "' AND bundle='", bundleName,"'")
  items = dbGetQuery(emuDBhandle$connection, itemsQuery)
  
  # labels 
  labelsQuery = paste0("SELECT itemID, labelIdx, name, label FROM labels WHERE db_uuid='", emuDBhandle$UUID, "' AND session='", sessionName, "' AND bundle='", bundleName,"'")
  labels = dbGetQuery(emuDBhandle$connection, labelsQuery)
  
  # links 
  
  linksQuery = paste0("SELECT fromID, toID, label FROM links WHERE db_uuid='", emuDBhandle$UUID, "' AND session='", sessionName, "' AND bundle='", bundleName,"'")
  links = dbGetQuery(emuDBhandle$connection, linksQuery)
  
  
  return(list(name = bundleName, annotates = annotates, sampleRate = sampleRate, items = items, links = links, labels = labels))
}


remove_bundleAnnotDBI<-function(emuDBhandle, bundleName, sessionName){
  cntSqlQuery=paste0("SELECT * FROM items WHERE db_uuid='", emuDBhandle$UUID, "' AND session='", sessionName, "' AND bundle='", bundleName,"'")
  res<-dbGetQuery(emuDBhandle$connection, cntSqlQuery)
  delSqlQuery=paste0("DELETE FROM items WHERE db_uuid='", emuDBhandle$UUID, "' AND session='", sessionName, "' AND bundle='", bundleName, "'")
  res<-dbSendQuery(emuDBhandle$connection, delSqlQuery)
  dbClearResult(res)
  delSqlQuery=paste0("DELETE FROM labels WHERE db_uuid='", emuDBhandle$UUID, "' AND session='", sessionName, "' AND bundle='", bundleName,"'")
  res<-dbSendQuery(emuDBhandle$connection, delSqlQuery)
  dbClearResult(res)
  delSqlQuery=paste0("DELETE FROM links WHERE db_uuid='", emuDBhandle$UUID, "' AND session='", sessionName, "' AND bundle='", bundleName, "'")
  res<-dbSendQuery(emuDBhandle$connection, delSqlQuery)
  dbClearResult(res)
  cntSqlQuery=paste0("SELECT * FROM linksExt WHERE db_uuid='", emuDBhandle$UUID, "' AND session='", sessionName, "' AND bundle='", bundleName, "'")
  res<-dbGetQuery(emuDBhandle$connection, cntSqlQuery)
  delSqlQuery=paste0("DELETE FROM linksExt WHERE db_uuid='", emuDBhandle$UUID, "' AND session='", sessionName, "' AND bundle='", bundleName,"'")
  res<-dbSendQuery(emuDBhandle$connection,delSqlQuery)
  dbClearResult(res)
}



###################################################
# create redundant links functions

## Legacy EMU and query functions link collections contain links for each possible connection between levels
## We consider links that do not follow link definition constraints as redundant and therefore we remove them from the
## link data model
build_allRedundantLinks <- function(emuDBhandle, sessionName=NULL, bundleName=NULL){
  
  hierarchyPaths = build_allHierarchyPaths(load_DBconfig(emuDBhandle))
  
  return(build_redundantLinksForPaths(emuDBhandle, hierarchyPaths, sessionName, bundleName) )
}

## Legacy EMU and query functions link collections contain links for each possible connection between levels
## We consider links that do not follow link definition constraints as redundant and therefore we remove them from the
## link data model. For queries we build links for particular start and end level.
# build_redundantLinks<-function(emuDBhandle, fromLevel, toLevel){
#   
#   hierarchyPaths = build_levelPartialPathes(get_DBconfig(emuDBhandle), fromLevel, toLevel)
#   
#   return(build_redundantLinksForPaths(database, hierarchyPaths))
# }


build_redundantLinksForPaths <- function(emuDBhandle, hierarchyPaths, sessionName='0000', bundleName=NULL){
  
  # delete any previous redundant links
  res <- dbSendQuery(emuDBhandle$connection, 'DELETE FROM linksTmp')
  dbClearResult(res)
  
  hierarchyPathsLen = length(hierarchyPaths)
  if(hierarchyPathsLen > 0){
    
    sqlQuery = "INSERT INTO linksTmp(db_uuid,session,bundle,fromID,toID,label) SELECT DISTINCT f.db_uuid,f.session,f.bundle,f.itemID AS fromID,t.itemID AS toID, NULL AS label FROM items f,items t"
    sqlQuery = paste0(sqlQuery," WHERE f.db_uuid='", emuDBhandle$UUID, "' AND f.db_uuid=t.db_uuid AND f.session=t.session AND f.bundle=t.bundle AND ")
    #sqlQuery=paste0(sqlQuery," WHERE f.db_uuid=t.db_uuid AND f.bundle=t.bundle AND f.session=t.session AND ")
    
    if(!is.null(sessionName) & !is.null(bundleName)){
      # only for one bundle
      sqlQuery = paste0(sqlQuery,"f.session='",sessionName,"' AND f.bundle='",bundleName,"' AND ")
    }
    
    sqlQuery=paste0(sqlQuery,' (')
    # build query for each partial path
    
    for(i in 1:hierarchyPathsLen){
      hp = hierarchyPaths[[i]]
      #cat("Path: ",hp,"\n")
      hpLen = length(hp)
      sHp = hp[1]
      eHp = hp[hpLen]
      sqlQuery = paste0(sqlQuery, "(f.level='", sHp, "' AND t.level='", eHp, "'" )
      sqlQuery = paste0(sqlQuery, " AND EXISTS (SELECT l1.* FROM ")
      for(li in 1:(hpLen - 1)){
        sqlQuery = paste0(sqlQuery, 'links l', li)
        if(li < (hpLen - 1)){
          sqlQuery = paste0(sqlQuery, ',')
        }
      }
      if(hpLen > 2){
        for(ii in 2:(hpLen-1)){
          sqlQuery=paste0(sqlQuery,',items i',ii)
        }
      }
      sqlQuery=paste0(sqlQuery," WHERE ")
      if(hpLen==2){
        sqlQuery=paste0(sqlQuery,"l1.db_uuid=f.db_uuid AND l1.db_uuid=t.db_uuid AND l1.session=f.session AND l1.session=t.session AND l1.bundle=f.bundle AND l1.bundle=t.bundle AND f.itemID=l1.fromID AND t.itemID=l1.toID")
        #sqlQuery=paste0(sqlQuery,"l1.bundle=f.bundle AND l1.bundle=t.bundle AND f.itemID=l1.fromID AND t.itemID=l1.toID")
        #cat(sHp,eHp,"\n")
      }else{
        # TODO start and end connection
        # from start to first in-between item 
        eHp=hp[2]
        #cat(sHp,eHp,"\n")
        sqlQuery=paste0(sqlQuery,"l1.db_uuid=f.db_uuid AND l1.db_uuid=i2.db_uuid AND l1.session=f.session AND l1.session=i2.session AND l1.bundle=f.bundle AND l1.bundle=i2.bundle AND f.itemID=l1.fromID AND i2.itemID=l1.toID AND f.level='",sHp,"' AND i2.level='",eHp,"' AND ")
        #sqlQuery=paste0(sqlQuery,"l1.bundle=f.bundle AND l1.bundle=i2.bundle AND f.itemID=l1.fromID AND i2.itemID=l1.toID AND f.level='",sHp,"' AND i2.level='",eHp,"' AND ")
        if(hpLen>3){
          for(j in 2:(hpLen-2)){
            sHp=hp[j]
            eHp=hp[j+1L] 
            #cat(sHp,eHp,"\n")
            sqlQuery=paste0(sqlQuery,"l",j,".db_uuid=i",j,".db_uuid AND l",j,".db_uuid=i",(j+1),".db_uuid AND l",j,".session=i",j,".session AND l",j,".session=i",(j+1),".session AND l",j,".bundle=i",j,".bundle AND l",j,".bundle=i",(j+1),".bundle AND i",j,".itemID=l",j,".fromID AND i",(j+1L),".itemID=l",j,".toID AND i",j,".level='",sHp,"' AND i",(j+1L),".level='",eHp,"' AND ")
          }
        }
        # from last in-between item to end item
        sHp=hp[(hpLen-1)]
        eHp=hp[hpLen]
        #cat(sHp,eHp,(hpLen-1),"\n")
        j=hpLen-1
        sqlQuery=paste0(sqlQuery,"l",j,".db_uuid=i",j,".db_uuid AND l",j,".db_uuid=t.db_uuid AND l",j,".session=i",j,".session AND l",j,".session=t.session AND l",j,".bundle=i",j,".bundle AND l",j,".bundle=t.bundle AND i",j,".itemID=l",j,".fromID AND t.itemID=l",j,".toID AND i",j,".level='",sHp,"' AND t.level='",eHp,"'")
        #sqlQuery=paste0(sqlQuery,"l",j,".bundle=i",j,".bundle AND l",j,".bundle=t.bundle AND i",j,".itemID=l",j,".fromID AND t.itemID=l",j,".toID AND i",j,".level='",sHp,"' AND t.level='",eHp,"'")
      }
      sqlQuery=paste0(sqlQuery,"))")
      if(i<hierarchyPathsLen){
        sqlQuery=paste0(sqlQuery," OR ")
      }
    }
    sqlQuery=paste0(sqlQuery,")")
    #cat(sqlQuery,"\n")
    # since version 2.8.x of sqlite the query is very slow without indices
    
    
    #cat(sqlQuery,"\n")
    #res<-dbSendQuery(get_emuDBcon(),comQuery)
    res<-dbSendQuery(emuDBhandle$connection, sqlQuery)
    dbClearResult(res)
  }
  #print(dbReadTable(get_emuDBcon(),'linksTmp'))
  
}

calculate_postionsOfLinks<-function(emuDBhandle){
  
  # for all position related functions we need to calculate the sequence indices of dominated items grouped to one dominance item 
  # Extend links table with sequence index of the targeted (dominated) item
  #links2=sqldf("SELECT k.*,i.seqIdx FROM links k,items i WHERE i.bundle=k.bundle AND k.toID=i.itemID")
  
  # since version 2.8.x of sqlite the query is very slow without indices
  #res<-dbSendQuery(get_emuDBcon(),'CREATE INDEX items_idx2 ON items(db_uuid,session,bundle,level,itemID,seqIdx)')
  #dbClearResult(res)
  
  res<-dbSendQuery(emuDBhandle$connection,"DELETE FROM linksExtTmp")
  dbClearResult(res)
  #print(dbReadTable(get_emuDBcon(),'linksTmp'))
  res<-dbSendQuery(emuDBhandle$connection,"INSERT INTO linksExtTmp(db_uuid,session,bundle,fromID,toID,seqIdx,toLevel,type,label) SELECT k.db_uuid,k.session,k.bundle,k.fromID,k.toID,i.seqIdx,i.level AS toLevel,i.type,NULL AS label FROM linksTmp k,items i WHERE i.db_uuid=k.db_uuid AND i.session=k.session AND i.bundle=k.bundle AND k.toID=i.itemID")
  dbClearResult(res)
  #dbSendQuery(get_emuDBcon(),"DELETE FROM linksExt")
  # extend links table with relative sequence index
  
  res<-dbSendQuery(emuDBhandle$connection,"INSERT INTO linksExtTmp2(db_uuid,session,bundle,seqIdx,fromID,toID,toLevel,type,label,toSeqIdx) SELECT k.db_uuid,k.session,k.bundle,k.seqIdx,k.fromID,k.toID,k.toLevel,k.type,k.label,k.seqIdx-(SELECT MIN(m.seqIdx) FROM linksExtTmp m WHERE m.fromID=k.fromID AND m.db_uuid=k.db_uuid AND m.session=k.session AND m.bundle=k.bundle AND k.toLevel=m.toLevel GROUP BY m.db_uuid,m.session,m.bundle,m.fromID,m.toLevel) AS toSeqIdx FROM linksExtTmp k")
  dbClearResult(res)
  
  res<-dbSendQuery(emuDBhandle$connection,"DELETE FROM linksExtTmp")
  dbClearResult(res)
  # Add length of dominance group sequence
  #links3IdxSql='CREATE INDEX links3_idx ON links3(session,bundle,fromID,toID,toLevel,type)'
  res<-dbSendQuery(emuDBhandle$connection,"INSERT INTO linksExt(db_uuid,session,bundle,seqIdx,fromID,toID,toSeqIdx,toLevel,type,label,toSeqLen) SELECT k.db_uuid,k.session,k.bundle,k.seqIdx,k.fromID,k.toID,k.toSeqIdx,k.toLevel,k.type,k.label,(SELECT MAX(m.seqIdx)-MIN(m.seqIdx)+1 FROM linksExtTmp2 m WHERE m.fromID=k.fromID AND m.db_uuid=k.db_uuid AND m.session=k.session AND m.bundle=k.bundle AND k.toLevel=m.toLevel GROUP BY m.db_uuid,m.session,m.bundle,m.fromID,m.toLevel) AS toSeqLen FROM linksExtTmp2 k")
  dbClearResult(res)
  
  res<-dbSendQuery(emuDBhandle$connection,"DELETE FROM linksExtTmp2")
  dbClearResult(res)
  #res<-dbSendQuery(get_emuDBcon(),"INSERT INTO linksExt SELECT * FROM linksExtTmp")
  #dbClearResult(res)
  
}

##########################################
################# emuDB ##################
##########################################

#############################################
# function that use emuDB files (vs. DBI)

##' List sessions of emuDB
##' @description List session names of emuDB
##' @param emuDBhandle emuDB handle
##' @return data.frame object with session names
##' @export
list_sessions <- function(emuDBhandle){
  sesPattern = paste0("^.*", session.suffix ,"$")
  sesDirs = dir(emuDBhandle$basePath, pattern = sesPattern)
  sesDirs = gsub(paste0(session.suffix, "$"), "", sesDirs)
  return(data.frame(name = sesDirs, stringsAsFactors = F))
}

##' List bundles of emuDB
##' 
##' List all bundles of emuDB or of particular session.
##' @param emuDBhandle emuDB handle
##' @param session optional session
##' @param dbUUID optional UUID of emuDB
##' @return data.frame object with columns session and name of bundles
##' @export
##' @examples 
##' \dontrun{
##' 
##' ##################################
##' # prerequisite: loaded ae emuDB
##' # (see ?load_emuDB for more information)
##' 
##' # list bundles of session "0000" of ae emuDB
##' list_bundles(emuDBhandle = ae,
##'              session = "0000")
##' 
##' }
##' 
list_bundles <- function(emuDBhandle, session=NULL){
  sesDf = list_sessions(emuDBhandle)
  bndlPattern = paste0("^.*", bundle.dir.suffix ,"$")
  res = data.frame(session = character(), name = character(), stringsAsFactors = F)
  
  for(ses in sesDf$name){
    bndlDirs = dir(file.path(emuDBhandle$basePath, paste0(ses, session.suffix)), pattern = bndlPattern)
    bndlNames = gsub(paste0(bundle.dir.suffix, "$"), "", bndlDirs)
    res = rbind(res, data.frame(session = ses, name = bndlNames, stringsAsFactors = F))
  }
  return(res)
}



##' List file paths of emuDBs bundles
##' 
##' List file paths of files belonging to emuDB.  For 
##' more information on the structural elements of an emuDB 
##' see \code{vignette{emuDB}}.
##' @param emuDBhandle
##' @param fileExtention file extention of files
##' @param sessionPattern A (regex) pattern matching sessions of emuDB
##' @param bundlePattern A (regex) pattern matching bundles of emuDB
##' @return file paths as character vector
##' @export
##' @examples 
##' \dontrun{
##' 
##' ##################################
##' # prerequisite: loaded ae emuDB 
##' # (see ?load_emuDB for more information)
##' 
##' # list all .fms file paths of ae emuDB
##' list_bundleFilePaths(emuDBhandle = ae, 
##'                      fileExtention = "fms") 
##' 
##' }
##' 
list_bundleFilePaths <- function(emuDBhandle, fileExtention, 
                                 sessionPattern='.*', bundlePattern='*'){
  
  dbConfig = load_DBconfig(emuDBhandle)
  
  bndls = list_bundles(emuDBhandle)
  postPatternBndls = bndls[grepl(sessionPattern, bndls$session) & grepl(bundlePattern, bndls$name),]
  if(dim(postPatternBndls)[1] == 0){
    stop("No files belonging to bundles found in '", dbName, "' with fileExtention '", fileExtention, "' and the sessionPattern '", 
         sessionPattern, "' and the bundlePattern '", bundlePattern, "'")
  }
  
  fp = file.path(emuDBhandle$basePath, paste0(postPatternBndls$session,'_ses'), paste0(postPatternBndls$name, '_bndl'), paste0(postPatternBndls$name, '.', fileExtention))
  
  # return only files that exist (should maybe issue warning)
  fpExist = fp[file.exists(fp)]
  
  return(fpExist)
}




rewrite_allAnnots <- function(emuDBhandle, verbose=TRUE){
  
  bndls = list_bundles(emuDBhandle)
  
  progress = 0
  if(verbose){
    bundleCount=nrow(bndls)
    cat("INFO: Rewriting", bundleCount, "_annot.json files to file system...\n")
    pb=txtProgressBar(min=0,max=bundleCount,style=3)
    setTxtProgressBar(pb,progress)
  }
  
  for(i in 1:nrow(bndls)){
    bndl = bndls[i,]
    bundleAnnotDFs = load_bundleAnnotDFsDBI(emuDBhandle, bndl$session, bndl$name)
    annotJSONchar = bundleAnnotDFsToAnnotJSONchar(emuDBhandle, bundleAnnotDFs)
    
    # construct path to annotJSON
    annotFilePath = file.path(emuDBhandle$basePath, paste0(bndl$session, session.suffix), 
                              paste0(bndl$name, bundle.dir.suffix), 
                              paste0(bndl$name, bundle.annotation.suffix, '.json'))
    
    writeLines(annotJSONchar, annotFilePath)
    
    progress=progress+1L
    if(verbose){
      setTxtProgressBar(pb,progress)
    }
  } 
}



#########################################################
# store / create / load functions

##' Store EMU database to directory
##' 
##' @details 
##' options is a list of key value pairs:
##' rewriteSSFFTracks if TRUE rewrite SSF tracks instead of file copy to get rid of big endian encoded SSFF files (SPARC), default: FALSE
##' ignoreMissingSSFFTrackFiles if TRUE missing SSFF track files are ignored, default: FALSE
##' symbolicLinkSignalFiles if TRUE signal files are symbolic linked instead of copied. Implies: rewriteSSFFTracks=FALSE, Default: FALSE
##' 
##' @param dbName name of emuDB
##' @param targetDir target directory
##' @param dbUUID optional: UUID of database
##' @param options list of options
##' @param showProgress show progress bar
##' @author Klaus Jaensch
##' @import stringr uuid jsonlite
##' @export
##' @keywords emuDB database Emu
##' @seealso  \code{\link{load_emuDB}}
##' @examples
##' \dontrun{
##' # Store database 'ae' to directory /homes/mylogin/EMUnew/
##' 
##'   store('ae',"/homes/mylogin/EmuStore/")
##' 
##' }
##' 
store<-function(dbName=NULL,targetDir,dbUUID=NULL,options=NULL,showProgress=TRUE){
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
  
  # build db dir name
  dbDirName=paste0(db[['name']],emuDB.suffix)
  # create database dir in targetdir
  pp=file.path(targetDir,dbDirName)
  # check existence
  if(file.exists(pp)){
    stop(pp," already exists.")
  }
  
  dir.create(pp)
  
  
  # set editable + showHierarchy
  db[['DBconfig']][['EMUwebAppConfig']][['activeButtons']]=list(saveBundle=TRUE,
                                                                showHierarchy=TRUE)
  
  
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
    bundleCount=as.integer(dbGetQuery(get_emuDBcon(dbUUID),paste0("SELECT count(*) FROM bundle WHERE db_uuid='",dbUUID,"'")))
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
      
      # store or link media file
      mfPath=get_media_file_path(db,b)
      mfBn=basename(mfPath)
      newMfPath=file.path(bfp,mfBn)
      if(file.exists(mfPath)){
        if(mergedOptions[['symbolicLinkSignalFiles']]){
          file.symlink(from=mfPath,to=newMfPath)
        }else{
          file.copy(from=mfPath,to=newMfPath)
        }
      }else{
        stop("Media file :'",mfPath,"' does not exist!")
      }
      
      # store or link SSFF tracks
      for(ssffTrDef in db[['DBconfig']][['ssffTrackDefinitions']]){
        ssffTrFileExt=ssffTrDef[['fileExtension']]
        trPath=get_ssfftrack_file_path(db,b,ssffTrackExt = ssffTrFileExt)
        trBn=basename(trPath)
        # build path for copy
        newTrPath=file.path(bfp,trBn)
        if(file.exists(trPath)){
          if(mergedOptions[['symbolicLinkSignalFiles']]){
            # link to original file
            file.symlink(from=trPath,to=newTrPath)
          }else if(mergedOptions[['rewriteSSFFTracks']]){
            # read/write instead of copy to get rid of big endian encoded SSFF files (SPARC)
            pfAssp=read.AsspDataObj(trPath)
            write.AsspDataObj(pfAssp,newTrPath)
          }else{
            # copy
            file.copy(from=trPath,to=newTrPath)
          }
        }else{
          if(!mergedOptions[['ignoreMissingSSFFTrackFiles']]){
            stop("SSFF track file :'",trPath,"' does not exist!")
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


##' @title Create empty emuDB
##' @description Creates an empty emuDB in the target directory specified
##' @details Creates a new directory [name]_emuDB in targetDir. By default the emuDB is created in the R session, written to the filesystem and then purged from the R session.
##' @param name of new emuDB
##' @param targetDir target directory to store the emuDB to
##' @param mediaFileExtension defines mediaFileExtention (NOTE: currently only 
##' 'wav' (the default) is supported by all components of EMU)
##' @param purge purge newly created emuDB from R session
##' @param store store new created emuDB to file system
##' @param verbose display infos & show progress bar
##' @export
create_emuDB<-function(name, targetDir, mediaFileExtension='wav', 
                       store=TRUE, verbose=TRUE){
  dbDirName=paste0(name,emuDB.suffix)
  basePath=file.path(targetDir,dbDirName)
  dbConfig=create.schema.databaseDefinition(name=name,mediafileExtension = mediaFileExtension)
  db=create.database(name=name,basePath=basePath,DBconfig = dbConfig)
  # .initialize.DBI.database()
  dbUUID=dbConfig[['UUID']]
  add_emuDBhandle(name=name,basePath = basePath,dbUUID=dbUUID)
  .store.emuDB.DBI(get_emuDBcon(dbUUID), database = db)
  if(store){
    store(targetDir=targetDir,dbUUID=dbUUID, showProgress = verbose)
  }
  
  return(invisible())
}

##' Load emuDB
##' 
##' @description Function loads emuDB from filesystem to R session
##' @details In order to use an emuDB from R it is necessary to load the annotation and configuration files to an emuR internal database format.
##' The function expects an emuDB file structure in directory \code{databaseDir}. The emuDB configuration file is loaded first. On success the function iterates through session and bundle directories and loads found annotation files.
##' Parameter \code{inMemoryCache} determines where the internal database is stored:
##' If \code{FALSE} a databse cache file in \code{databaseDir} is used. When the database is loaded for the first time the function will create a new cache file and store the data to it. On subsequent loading of the same database the cache is only updated if files have changed, therefore the loading is then much faster.
##' The user needs write permissions to \code{databaseDir} and the cache file.
##' The database is loaded to a volatile in-memory database if \code{inMemoryCache} is set to \code{TRUE}.
##' If the requested emuDB is already loaded to the R session, data of changed annotation or configuration files get updated. The reloaded representation of the database is then in sync with the filesystem. 
##' @param databaseDir directory of the emuDB
##' @param inMemoryCache cache the loaded DB in memory
##' @param verbose be verbose
##' @return name of emuDB
##' @import jsonlite DBI
##' @export
##' @keywords emuDB database DBconfig
##' @examples
##' \dontrun{
##' ## Load database ae in directory /homes/mylogin/EMUnew/ae 
##' ## assuming an existing emuDB structure in this directory
##' 
##' ae = load_emuDB("/homes/mylogin/EMU/ae")
##' 
##' ## Load database ae from demo data
##' 
##' # create demo data in temporary directory
##' create_emuRdemoData()
##' # build base path to demo emuDB
##' demoDatabaseDir = file.path(tempdir(), "emuR_demoData", "ae")
##' 
##' # load demo emuDB
##' ae = load_emuDB(demoDatabaseDir)
##' 
##' }
load_emuDB <- function(databaseDir, inMemoryCache = FALSE, verbose=TRUE){
  progress = 0
  
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
  DBconfig = jsonlite::fromJSON(dbCfgPath, simplifyVector=FALSE)
  # normalize base path
  basePath = normalizePath(databaseDir)
  
  # shorthand vars
  dbName = DBconfig$name
  dbUUID = DBconfig$UUID
  
  # create dbHandle
  if(inMemoryCache){
    dbHandle = emuDBhandle(dbName, basePath, dbUUID, connectionPath = ":memory:")
  }else{
    cachePath = file.path(normalizePath(databaseDir), paste0(dbName, database.cache.suffix))
    dbHandle = emuDBhandle(dbName, basePath, dbUUID, cachePath)
  }
  
  # check if cache exist -> update cache if true
  dbsDf = get_emuDbDBI(dbHandle)
  if(nrow(dbsDf)>0){
    update_cache(dbHandle, verbose = verbose)
    return(dbHandle)
  }
  
  # write to DBI emuDB table
  add_emuDbDBI(dbHandle)
  
  # list sessions & bundles
  sessions = list_sessions(dbHandle)
  bundles = list_bundles(dbHandle)
  # add column to sessions to track if already stored
  sessions$stored = F
  # calculate bundle count
  bundleCount = nrow(bundles)
  # create progress bar
  pMax = bundleCount
  if(pMax == 0){
    pMax = 1
  }
  if(verbose){ 
    cat(paste0("INFO: Loading EMU database from ", databaseDir, "... (", bundleCount , " bundles found)\n"))
    pb=txtProgressBar(min = 0L, max = pMax, style = 3)
    setTxtProgressBar(pb, progress)
  }
  
  # bundles
  for(bndlIdx in 1:nrow(bundles)){
    bndl = bundles[bndlIdx,]
    # check if session has to be added to DBI
    if(!(sessions$stored[sessions$name == bndl$session])){
      add_sessionDBI(dbHandle, bndl$session)
      sessions$stored[sessions$name == bndl$session] = TRUE
    }
    
    # construct path to annotJSON
    annotFilePath = normalizePath(file.path(dbHandle$basePath, paste0(bndl$session, session.suffix), 
                                            paste0(bndl$name, bundle.dir.suffix), 
                                            paste0(bndl$name, bundle.annotation.suffix, '.json')))
    
    # calculate MD5 sum of bundle annotJSON
    newMD5annotJSON = md5sum(annotFilePath)
    names(newMD5annotJSON) = NULL
    
    # read annotJSON as charac 
    annotJSONchar = readChar(annotFilePath, file.info(annotFilePath)$size)
    
    # convert to bundleAnnotDFs
    bundleAnnotDFs = annotJSONcharToBundleAnnotDFs(annotJSONchar)
    
    # add to bundle table
    add_bundleDBI(dbHandle, bndl$session, bndl$name, bundleAnnotDFs$annotates, bundleAnnotDFs$sampleRate, newMD5annotJSON)
    # add to items, links, labels tables
    store_bundleAnnotDFsDBI(dbHandle, bundleAnnotDFs, bndl$session, bndl$name)
    
    # increase progress bar  
    progress=progress+1L
    if(verbose){
      setTxtProgressBar(pb,progress)
    }
    
  }
  
  # build redundat links and calc positions
  cat("\nbuilding redundant links and position of links... (this may take a while)\n")
  build_allRedundantLinks(dbHandle)
  calculate_postionsOfLinks(dbHandle)
  
  return(dbHandle)
  
}

#######################
# FOR DEVELOPMENT
# library('testthat')
# test_file('tests/testthat/test_duplicate.loaded.emuDB.R')
# test_file('tests/testthat/test_database.caching.R')
