require(sqldf)
require(stringr)

###########################################################################
# create table / index definitions for DBI that are used at query time

# tabels that store "filtered" items, labels and linksExt (when session/bundlePatterns are used)
database.DDL.emuDB_itemsFilteredTmp = gsub("CREATE TABLE items", "CREATE TEMP TABLE itemsFilteredTmp", database.DDL.emuDB_items)

database.DDL.emuDB_labelsFilteredTmp = gsub("CREATE TABLE labels", "CREATE TEMP TABLE labelsFilteredTmp", database.DDL.emuDB_labels)
database.DDL.emuDB_labelsFilteredTmp_idx = "CREATE INDEX labelsFilteredTmp_idx ON labelsFilteredTmp(itemID,db_uuid,session,bundle,name)"

database.DDL.emuDB_linksExtFilteredTmp = gsub("CREATE TABLE linksExt", "CREATE TEMP TABLE linksExtFilteredTmp", database.DDL.emuDB_linksExt)
database.DDL.emuDB_linksExtFilteredTmp_idx = 'CREATE INDEX linksExtFilteredTmp_idx ON linksExtFilteredTmp(db_uuid,session,bundle,fromID,toID)'

# tabels that store subsets of filtered tables
database.DDL.emuDB_itemsFilteredSubsetTmp = gsub("CREATE TABLE items", "CREATE TEMP TABLE itemsFilteredSubsetTmp", database.DDL.emuDB_items) 
database.DDL.emuDB_labelsFilteredSubsetTmp = gsub("CREATE TABLE labels", "CREATE TEMP TABLE labelsFilteredSubsetTmp", database.DDL.emuDB_labels)
database.DDL.emuDB_labelsFilteredSubsetTmp_idx = "CREATE INDEX labelsFilteredSubsetTmp_idx ON labelsFilteredSubsetTmp(itemID,db_uuid,session,bundle,name)"
database.DDL.emuDB_linksExtFilteredSubsetTmp = gsub("CREATE TABLE linksExt", "CREATE TEMP TABLE linksExtFilteredSubsetTmp", database.DDL.emuDB_linksExt)
database.DDL.emuDB_linksExtFilteredSubsetTmp_idx = 'CREATE INDEX linksExtFilteredSubsetTmp_idx ON linksExtFilteredSubsetTmp(db_uuid,session,bundle,fromID,toID)'

# leftIntermResult
database.DDL.emuDB_leftIntermResultItemsTmp = 'CREATE TEMP TABLE leftIntermResultItemsTmp (
  db_uuid VARCHAR(36),
  session TEXT,
  bundle TEXT,
  seqStartId INTEGER,
  seqEndId INTEGER,
  seqLen INTEGER,
  level TEXT,
  PRIMARY KEY (db_uuid, session, bundle, seqStartId, seqEndId),
  FOREIGN KEY (db_uuid, session, bundle) REFERENCES bundle(db_uuid, session_name, name)
);'

database.DDL.emuDB_leftIntermResultItemsTmp_idx = "CREATE INDEX leftIntermResultItemsTmp_idx ON leftIntermResultItemsTmp(db_uuid,session,bundle,seqStartId,seqEndId,seqLen,level)"


database.DDL.emuDB_leftIntermResultLinksTmp = 'CREATE TEMP TABLE leftIntermResultLinksTmp (
  db_uuid VARCHAR(36),
  session TEXT,
  bundle TEXT,
  fromID INTEGER,
  toID INTEGER,
  label TEXT,
  PRIMARY KEY (db_uuid, session, bundle, fromID, toID),
  FOREIGN KEY (db_uuid, session, bundle) REFERENCES bundle(db_uuid, session_name, name)
);'

database.DDL.emuDB_leftIntermResultMetaInfosTmp = 'CREATE TEMP TABLE leftIntermResultMetaInfosTmp (
  resultLevel TEXT,
  projectionAttrLevel TEXT,
  queryStr TEXT
);'

database.DDL.emuDB_leftIntermResultProjectionItemsTmp = gsub("CREATE TEMP TABLE leftIntermResultItemsTmp", "CREATE TEMP TABLE leftIntermResultProjectionItemsTmp", database.DDL.emuDB_leftIntermResultItemsTmp)

# rightIntermResult
database.DDL.emuDB_rightIntermResultItemsTmp = gsub("CREATE TEMP TABLE leftIntermResultItemsTmp", "CREATE TEMP TABLE rightIntermResultItemsTmp", database.DDL.emuDB_leftIntermResultItemsTmp)
database.DDL.emuDB_rightIntermResultLinksTmp = gsub("CREATE TEMP TABLE leftIntermResultLinksTmp", "CREATE TEMP TABLE rightIntermResultLinksTmp", database.DDL.emuDB_leftIntermResultLinksTmp)
database.DDL.emuDB_rightIntermResultMetaInfosTmp = gsub("CREATE TEMP TABLE leftIntermResultMetaInfosTmp", "CREATE TEMP TABLE rightIntermResultMetaInfosTmp", database.DDL.emuDB_leftIntermResultMetaInfosTmp)
database.DDL.emuDB_rightIntermResultProjectionItemsTmp = gsub("CREATE TEMP TABLE leftIntermResultProjectionItemsTmp", "CREATE TEMP TABLE rightIntermResultProjectionItemsTmp", database.DDL.emuDB_leftIntermResultProjectionItemsTmp)

# bracketsIntermResultItemsTmp
database.DDL.emuDB_bracketsIntermResultItemsTmp = gsub("CREATE TEMP TABLE leftIntermResultItemsTmp", "CREATE TEMP TABLE bracketsIntermResultItemsTmp", database.DDL.emuDB_leftIntermResultItemsTmp)

#################################
#

EMPTY_RESULT_DF=data.frame(db_uuid=character(0),session=character(0),bundle=character(0),seqStartId=character(0),seqEndId=character(0),seqLen=integer(0),level=character(0),stringsAsFactors = FALSE)

setQueryTmpEmuDBs<-function(queryTmpEmuDBs){
  internalVars$queryTmpEmuDBs<-queryTmpEmuDBs
}

getQueryTmpEmuDBs<-function(){
  return(internalVars$queryTmpEmuDBs)
}

.create.condition.text<-function(opr,value){
  o=list(opr=opr,value=value)
  class(o)<-c('emuR.condition.text','emuR.condition')
  return(o)
}

create_conditionTextAlternatives<-function(opr,values){
  o=list(opr=opr,values=values)
  class(o)<-c('emuR.condition.text.alternatives','emuR.condition')
  return(o)
}

create.condition.level.label<-function(levelName,conditionText){
  o=list(levelName=levelName,conditionText=conditionText)
  class(o)<-c('emuR.condition.level.label','emuR.condition')
  return(o)
}

create.condition<-function(op1,opr,op2){
  o=list(op1=op1,opr=opr,op2=op2)
  class(o)<-'emuR.condition'
  return(o)
}


# create.subtree<-function(items, links, resultLevel = NULL, projectionItems=NULL){
#   o=list(items=items,links=links,resultLevel=resultLevel,projectionItems=projectionItems)
#   class(o)<-'emuDB.query.result'
#   return(o)
# }

emuR.regexprl<-function(pattern,x){
  m=regexpr(pattern,x)
  return((m==1) & (attr(m,'match.length')==nchar(x)))
}

write_table_forced<-function(dbUUID,name,value){
  if(dbExistsTable(get_emuDBcon(dbUUID),name)){
    dbRemoveTable(get_emuDBcon(dbUUID),name)
  }
  dbWriteTable(get_emuDBcon(dbUUID),name = name,value=value)
}

check_level_attribute_name<-function(dbConfig,name){
  aNms=get.all.attribute.names(dbConfig)
  if(! (name %in% aNms)){
    stop("Unknown level attribute name: '",name,"'. Database attribute names are: ",paste(aNms,collapse=','),"\n")
  }
}

## @param emuDBhandle
## @param intermResTablePrefix "left" or "right"
clear_intermResTabels <- function(emuDBhandle, intermResTablePrefix = "left"){
  
  dbGetQuery(emuDBhandle$connection, paste0("DELETE FROM ", paste0(intermResTablePrefix, "IntermResultItemsTmp")))
  dbGetQuery(emuDBhandle$connection, paste0("DELETE FROM ", paste0(intermResTablePrefix, "IntermResultLinksTmp")))
  dbGetQuery(emuDBhandle$connection, paste0("DELETE FROM ", paste0(intermResTablePrefix, "IntermResultMetaInfosTmp")))
  dbGetQuery(emuDBhandle$connection, paste0("DELETE FROM ", paste0(intermResTablePrefix, "IntermResultProjectionItemsTmp")))
}

## @param emuDBhandle
## @param levelName
## @param intermResTablePrefix "left" or "right" 
query_labels <- function(emuDBhandle, levelName, intermResTablePrefix = "left", conditionText, useSubsets){
  
  if(useSubsets){
    labelTableName = "labelsFilteredSubsetTmp"
  }else{
    labelTableName = "labelsFilteredTmp"
  }
  
  clear_intermResTabels(emuDBhandle, intermResTablePrefix)
  opr=conditionText[['opr']]
  values=conditionText[['values']]
  res=NULL
  if(opr=='==' | opr=='='){
    for(value in values){
      dbGetQuery(emuDBhandle$connection, paste0("INSERT INTO ", intermResTablePrefix, "IntermResultItemsTmp ",
                                                "SELECT db_uuid, session, bundle, itemID AS seqStartId, itemID AS seqEndId, 1 AS seqLen,'", 
                                                levelName, "' AS level FROM ", labelTableName, " ",
                                                "WHERE name = '", levelName, "' AND label = '", value, "'"))
    }
  }else if(opr=='!='){   
    sqlStr = paste0("INSERT INTO ", intermResTablePrefix, "IntermResultItemsTmp ",
                    "SELECT db_uuid, session, bundle, itemID AS seqStartId, itemID AS seqEndId, 1 AS seqLen,'", 
                    levelName, "' AS level FROM ", labelTableName, " ",
                    "WHERE name = '", levelName, "'")
    for(value in values){
      sqlStr = paste0(sqlStr, " AND label <> '", value, "'")
    }
    dbGetQuery(emuDBhandle$connection, sqlStr)
    
  }else if(opr=='=~'){
    for(value in values){
      ldf = dbGetQuery(emuDBhandle$connection, paste0("SELECT db_uuid, session, bundle, itemID AS seqStartId, itemID AS seqEndId, 1 AS seqLen,'", 
                                                      levelName, "' AS level, label FROM ", labelTableName, " ",
                                                      "WHERE name = '", levelName, "'"))
      ssl = emuR.regexprl(value,ldf[['label']])
      res = ldf[ssl,]
      dbWriteTable(emuDBhandle$connection, paste0(intermResTablePrefix, "IntermResultItemsTmp"), subset(res, select = -label), overwrite = F, append = T) # label column is ignored by dbWriteTable
    }
  }else if(opr=='!~'){
    for(value in values){
      ldf = dbGetQuery(emuDBhandle$connection, paste0("SELECT db_uuid, session, bundle, itemID AS seqStartId, itemID AS seqEndId, 1 AS seqLen,'", 
                                                      levelName, "' AS level, label FROM ", labelTableName, " ",
                                                      "WHERE name = '", levelName, "'"))
      ssl = !emuR.regexprl(value,ldf[['label']])
      res = ldf[ssl,]
      dbWriteTable(emuDBhandle$connection, paste0(intermResTablePrefix, "IntermResultItemsTmp"), subset(res, select = -label), overwrite = F, append = T) # label column is ignored by dbWriteTable
    }
  }else{
    stop("Syntax error: Unknown operator: '",opr,"'\n")
  }
  # insert resultLevel
  dbGetQuery(emuDBhandle$connection, paste0("INSERT INTO ", intermResTablePrefix, "IntermResultMetaInfosTmp (resultLevel) ",
                                            "VALUES ('", levelName, "')"))
}

# query_databaseLevelLabel<-function(emuDBhandle, levelName, conditionText){
#   #levelSubset=subset(ldf,name==levelName)
#   ldf = dbReadTable(emuDBhandle$connection, "labels") # SIC!
#   subsetSelector=ldf[['name']]==levelName
#   levelSubset=ldf[subsetSelector,]
#   itemsMatch=query_labels(levelSubset, conditionText)
#   itemsAsSeqs=NULL
#   if(nrow(itemsMatch)>0){
#     itemsIdxSql='CREATE INDEX itemsMatch_idx ON itemsMatch(itemID)'
#     
#     itemsAsSeqQStr=paste0("SELECT db_uuid,session,bundle,itemID AS seqStartId, itemID AS seqEndId,1 AS seqLen,'",levelName,"' AS level FROM itemsMatch")
#     itemsAsSeqs=sqldf(c(itemsIdxSql,itemsAsSeqQStr))
#   }else{
#     itemsAsSeqs=EMPTY_RESULT_DF
#   }
#   # add sequence index
#   #qStr=paste0("SELECT i.*,",rsIdx," AS rsIdx FROM itemsMatch i")
#   #itemsMatch=sqldf(qStr)
#   res=create.subtree(items=itemsAsSeqs,links=NULL,resultLevel=levelName)
#   return(res)
# }



query.database.eql.FUNKA<-function(dbConfig,q,items=NULL){
  # BNF: FUNKA = POSA | NUMA;
  qTrim=str_trim(q)
  if(is.null(items)){
    items=getQueryTmpEmuDBs()[['queryItems']]
  }
  allItems=getQueryTmpEmuDBs()[['queryItems']]
  
  # determine function name
  # TODO duplicate code
  prbOpen = get_string.Position(string=qTrim,searchStr='(',literalQuote="'")
  if(prbOpen!=-1){
    prbClose = get_stringPosition(string=qTrim,searchStr=')',literalQuote="'")
    if(prbClose==-1){
      stop("Syntax error: Missing closing round bracket ')' in '",q,"'\n")
    }else{
      if(prbOpen>prbClose){
        stop("Syntax error: Expected opening round bracket '(' before closing round bracket in '",q,"'\n")
      }
      if(prbOpen==1){
        stop("Syntax error: Expected function name in '",q,"'\n")
      }
      paramsVec=str_split(substr(qTrim,prbOpen+1,prbClose-1),',')
      params=paramsVec[[1]]
      paramsLen=length(params)
      # all functions require exactly two params
      if(paramsLen!=2){
        stop("Syntax error: All EQL functions require exactly two parameters in '",q,"'\n")
      }
      param1=str_trim(params[[1]])
      param2=str_trim(params[[2]])
      # check attribute names
      aNms=get.all.attribute.names(dbConfig)
      if(! (param1 %in% aNms)){
        msg=paste0("Unknown level attribute name: '",param1,"'.")
        if(length(aNms)>0){
          msg=paste0(msg," Database attribute names are: ",paste(aNms,collapse=','))
        }
        msg=paste0(msg,"\n")
        stop(msg)
      }
      if(! (param2 %in% aNms)){
        msg=paste0("Unknown level attribute name: '",param2,"'.")
        if(length(aNms)>0){
          msg=paste0(msg," Database attribute names are: ",paste(aNms,collapse=','))
        }
        msg=paste0(msg,"\n")
        stop(msg)
      }
      
      funcValueTerm=str_trim(substring(qTrim,prbClose+1))
      
      
      funcName=str_trim(substr(qTrim,1,prbOpen-1))
      # BNF: POSA = POSFKT,'(',EBENE,',',EBENE,')','=','0'| '1';
      links=getQueryTmpEmuDBs()[['queryLinksExt']]
      #links=database[['linksWithPositions']]
      itemsAsSeqs=NULL
      
      level1=get.level.name.for.attribute(dbConfig,param1)
      level2=get.level.name.for.attribute(dbConfig,param2)
      
      # BNF: VOP = '=' | '!=' | '>' | '<' | '<=' | '>=';
      
      if(funcName=='Start' | funcName=='End' | funcName=='Medial'){
        
        if(funcValueTerm!=''){
          # check equals operator == or =
          expEqualSign=substr(funcValueTerm,1,1)
          if(expEqualSign!='='){
            stop("Syntax error: Expected equal sign '==' for in function term: '",qTrim,"'\n")
          }
          op='='
          funcValuePos=2
          if(substr(funcValueTerm,2,2)=='='){
            funcValuePos=3
            op='=='
          }
          funcValue=str_trim(substring(text=funcValueTerm,first=funcValuePos))
        }else{
          stop("Syntax error: function ",funcName," requires function value in: '",qTrim,"'\n")
        }
      } 
      itemsIdxSql='CREATE INDEX items_idx ON items(db_uuid,session,bundle,level,itemID,seqIdx)'
      linksIdxSql='CREATE INDEX links_idx ON links(db_uuid,session,bundle,fromID,toID,toSeqIdx,toSeqLen)'
      if(funcName=='Start'){
        cond=NULL
        if(funcValue=='0'){
          cond='!='
        }else if(funcValue=='1'){
          cond='='
        }else{
          stop("Syntax error: Expected function value 0 or 1 after '",op,"' in function term: '",qTrim,"'\n")
        }
        sqlQStr=paste0("SELECT DISTINCT i.db_uuid,i.session,i.bundle,i.itemID AS seqStartId, i.itemID AS seqEndId,1 AS seqLen,'",param2,"' AS level \
                        FROM items i,allItems d \
                        WHERE i.db_uuid=d.db_uuid AND i.session=d.session AND i.bundle=d.bundle\
                         AND i.level='",level2,"' AND d.level='",level1,"' \
                         AND EXISTS \
                          (SELECT * FROM links k \
                           WHERE d.db_uuid=k.db_uuid AND d.session=k.session AND d.bundle=k.bundle \
                            AND i.db_uuid=k.db_uuid AND i.session=k.session AND i.bundle=k.bundle \
                            AND k.fromID=d.itemID AND k.toID=i.itemID AND k.toSeqIdx",cond,"0\
                           )") 
        
        itemsAsSeqs=sqldf(c(itemsIdxSql,linksIdxSql,sqlQStr))
        resultLevel=param2
      }else if(funcName=='Medial'){
        cond=NULL
        bOp=NULL
        if(funcValue=='0'){
          cond='='
          bOp='OR'
        }else if(funcValue=='1'){
          cond='!='
          bOp='AND'
        }else{
          stop("Syntax error: Expected function value 0 or 1 after '",op,"' in function term: '",qTrim,"'\n")
        }
        sqlQStr=paste0("SELECT DISTINCT i.db_uuid,i.session,i.bundle,i.itemID AS seqStartId, i.itemID AS seqEndId,1 AS seqLen,'",param2,"' AS level \
                       FROM items i,allItems d \
                       WHERE i.db_uuid=d.db_uuid AND i.session=d.session AND i.bundle=d.bundle \
                       AND i.level='",level2,"' AND d.level='",level1,"' \
                       AND EXISTS \
                       (SELECT * FROM links k \
                       WHERE d.db_uuid=k.db_uuid AND d.session=k.session AND d.bundle=k.bundle \
                        AND i.db_uuid=k.db_uuid AND i.session=k.session AND i.bundle=k.bundle \
                        AND k.fromID=d.itemID AND k.toID=i.itemID AND (k.toSeqIdx",cond,"0 ",bOp," k.toSeqIdx+1",cond,"k.toSeqLen)\
                       )") 
        itemsAsSeqs=sqldf(c(itemsIdxSql,linksIdxSql,sqlQStr))
        resultLevel=param2
      }else if(funcName=='End'){
        cond=NULL
        if(funcValue=='0'){
          cond='!='
        }else if(funcValue=='1'){
          cond='='
        }else{
          stop("Syntax error: Expected function value 0 or 1 after '",op,"' in function term: '",qTrim,"'\n")
        }
        sqlQStr=paste0("SELECT DISTINCT i.db_uuid,i.session,i.bundle,i.itemID AS seqStartId, i.itemID AS seqEndId,1 AS seqLen,'",param2,"' AS level FROM \
                       items i,allItems d \
                       WHERE i.db_uuid=d.db_uuid AND i.session=d.session AND i.bundle=d.bundle \
                       AND i.level='",level2,"' AND d.level='",level1,"' AND EXISTS \
                       (SELECT * FROM links k \
                        WHERE d.db_uuid=k.db_uuid AND d.session=k.session AND d.bundle=k.bundle \
                            AND i.db_uuid=k.db_uuid AND i.session=k.session AND i.bundle=k.bundle \
                            AND k.fromID=d.itemID AND k.toID=i.itemID AND k.toSeqIdx+1",cond,"k.toSeqLen\
                       )") 
        itemsAsSeqs=sqldf(c(itemsIdxSql,linksIdxSql,sqlQStr))
        resultLevel=param2
      }else if(funcName=='Num'){
        funcVal=NULL
        funcOpr=NULL
        for(opr in c('==','!=','<=','>=','=','>','<')){
          p=get_stringPosition(string=funcValueTerm,searchStr=opr)
          if(p==1){
            oprLen=nchar(opr)
            funcOpr=substr(funcValueTerm,1,oprLen)
            funcValStr=str_trim(substring(funcValueTerm,oprLen+1))
            funcVal=as.integer(funcValStr)
            if(is.na(funcVal)){
              stop("Syntax error: Could not parse Num function value as integer: '",funcValStr,"'\n")
            }
            break
          }
        }
        if(is.null(funcOpr) | is.null(funcVal)){
          stop("Syntax error: Unknown operator and/or value for Num  function: '",funcValueTerm,"'\n")
        }
        if(funcOpr=='=='){
          sqlFuncOpr='='
        }else{
          sqlFuncOpr=funcOpr
        }
        # BNF: NUMA = 'Num','(',EBENE,',',EBENE,')',VOP,INTPN;
        # Note return value level is param1 here
        # sqlQStr=paste0("SELECT d.id AS seqStartId, d.id AS seqEndId FROM allItems i,items d WHERE i.level='",param2,"' AND d.level='",param1,"' AND EXISTS (SELECT * FROM links k WHERE k.bundle=i.bundle AND k.bundle=d.bundle AND k.fromID=d.itemID AND k.toID=i.itemID AND k.toLevel=i.level AND k.toSeqLen=",funcValue,")")
        #numChilds=as.integer(funcValue)
        #if(is.na(numChilds)){
        # stop("Syntax error: Expected integer value after '=' in function term: '",qTrim,"'\n")
        #}
        #         sqlQStr=paste0("SELECT DISTINCT d.db_uuid,d.session,d.bundle,d.itemID AS seqStartId, d.itemID AS seqEndId,1 AS seqLen,'",param1,"' AS level \
        #                        FROM allItems i,items d \
        #                       WHERE i.db_uuid=d.db_uuid AND i.session=d.session AND i.bundle=d.bundle \
        #                        AND i.level='",level2,"' AND d.level='",level1,"' AND EXISTS \
        #                        (SELECT * FROM links k \
        #                        WHERE d.db_uuid=k.db_uuid AND d.session=k.session AND d.bundle=k.bundle \
        #                         AND i.db_uuid=k.db_uuid AND i.session=k.session AND i.bundle=k.bundle \
        #                         AND k.fromID=d.itemID AND k.toID=i.itemID AND k.toLevel=i.level AND k.toSeqLen",sqlFuncOpr,funcVal,"\
        #                        )") 
        
        sqlQStr=paste0("SELECT DISTINCT d.db_uuid,d.session,d.bundle,d.itemID AS seqStartId, d.itemID AS seqEndId,1 AS seqLen,'",param1,"' AS level \
                       FROM items d \
                      WHERE (SELECT count(*) FROM allItems i WHERE i.db_uuid=d.db_uuid AND i.session=d.session AND i.bundle=d.bundle \
                       AND i.level='",level2,"' AND d.level='",level1,"' AND EXISTS \
                       (SELECT * FROM links k \
                       WHERE d.db_uuid=k.db_uuid AND d.session=k.session AND d.bundle=k.bundle \
                        AND i.db_uuid=k.db_uuid AND i.session=k.session AND i.bundle=k.bundle \
                        AND ( \
                         ( k.fromID=d.itemID AND k.toID=i.itemID AND k.toLevel=i.level ) OR \
                         ( k.fromID=i.itemID AND k.toID=d.itemID AND k.toLevel=d.level ) \
                        )\
                       ))",sqlFuncOpr,funcVal)
        itemsAsSeqs=sqldf(c(itemsIdxSql,linksIdxSql,sqlQStr))
        resultLevel=param1
      }else{
        stop("Syntax error: Unknwon function: '",funcName,"'")
      }
      res=create.subtree(items=itemsAsSeqs,links=NULL,resultLevel=resultLevel)
    }
  }else{
    stop("Syntax error: Missing opening round bracket '(' in '",q,"'\n")
  }
}

query_databaseEqlSQ <- function(emuDBhandle, q, intermResTablePrefix, useSubsets){
  # BNF: EA = ETIKETTA | FUNKA;
  qTrim=str_trim(q)
  res=NULL
  # detect function calls by existence of round brackets
  prbOpen=get_stringPosition(string=qTrim,searchStr='(',literalQuote="'")
  if(prbOpen!=-1){
    prbClose=get_stringPosition(string=qTrim,searchStr=')',literalQuote="'")
    if(prbClose==-1){
      stop("Syntax error: Missing closing round bracket ')' in '",q,"'\n")
    }else{
      if(prbOpen>prbClose){
        stop("Syntax error: Expected opening round bracket '(' before closing round bracket in '",q,"'\n")
      }
      if(prbOpen==1){
        stop("Syntax error: Expected function name in '",q,"'\n")
      }
      res=query.database.eql.FUNKA(dbConfig,qTrim,items)
    }
  }else{
    # No round brackets, assuming a level query
    query_databaseEqlLABELQ(emuDBhandle, qTrim, useSubsets, intermResTablePrefix = intermResTablePrefix)
  }
  
  # return(res)
}

query_databaseEqlLABELQ <- function(emuDBhandle, q, useSubsets, intermResTablePrefix = "left"){
  # BNF: ETIKETTA = ['#'],EBENE,('=' | '!='),ETIKETTALTERNATIVEN;
  
  qTrim=str_trim(q)
  dbConfig = load_DBconfig(emuDBhandle)
  for(opr in c('==','!=','=~','!~','=')){
    p = get_stringPosition(string = q, searchStr = opr, literalQuote = "'")
    if(p!=-1){
      oprLen=nchar(opr)
      level=substr(q,1,p-1)
      projectionLevel=FALSE
      lvlTrim=str_trim(level)
      lvlName=lvlTrim
      if(grepl('^#',lvlTrim)){
        # projection marker
        # the BNF does not allow white space between '#' and level string
        # but the implementation of Emu does, so we allow it here too
        
        lvlName=str_trim(substring(lvlTrim,2))
        projectionLevel=TRUE
      }
      aNms = get_allAttributeNames(emuDBhandle)
      if(! (lvlName %in% aNms)){
        stop("Unknown level attribute name: '",lvlName,"'. Database attribute names are: ",paste(aNms,collapse=','),"\n")
      }
      labelStr=substring(q,p+oprLen)
      labelTrim=str_trim(labelStr)  
      
      
      # check label for key chars
      # TODO Labels should to be allowed to contain key chars if they are single quoted 
      deniedStrs=c('^','->','==','!=','=')
      for(deniedStr in deniedStrs){
        pt=get_stringPosition(string=labelTrim,searchStr=deniedStr,literalQuote="'")
        if(pt!=-1){
          stop("Syntax error label ",labelStr," contains '",deniedStr,"'. Quote label with ''.")
        }
      }
      
      # BNF: ETIKETTALTERNATIVEN = ETIKETT , {'|',ETIKETT};
      # parse alternatives
      labelAlts=c()
      lp=1
      lsp=0
      while(lsp!=-1){
        lsp=get_stringPosition(string=labelTrim,pos=lp,searchStr='|',literalQuote="'")
        if(lsp!=-1){
          if(lsp==1){
            stop("Syntax error: label alternatives cannot start with '|' character in '",labelTrim,"'")
          }
          labelAltTerm=substr(labelTrim,lp,lsp-1)
          labelAlt=str_trim(labelAltTerm)
          labelAlts=c(labelAlts,labelAlt)
          lp=lsp+1
        }
      }
      # add last term
      labelAltTerm=substring(labelTrim,lp)
      labelAlt=str_trim(labelAltTerm)
      labelAlts=c(labelAlts,labelAlt)
      
      labelAltsUq=c()
      # unquote labels
      # BNF: ETIKETT = ETIKETTIERUNG | ("'",ETIKETTIERUNG,"'");
      # Suggestion for improvement:
      # labelGroups (legacy EMU 'legal' directive) MUST NOT be quoted, to distinguish labelGroups from ordinary label or label pattern:
      # BNF__:ETIKETT = LABEL_GROUP_NAME | ETIKETTIERUNG | ("'",ETIKETTIERUNG,"'");
      #        ETIKETTIERUNG = {ALPHA|DIGIT}
      
      for(labelAlt in labelAlts){
        label=NULL
        if(substr(labelAlt,1,1)=="'"){
          lblTrimLen=nchar(labelAlt)
          if(substring(labelAlt,lblTrimLen)!="'"){
            stop("Syntax error: expected closing single quote at end of label '",labelAlt,"'\n")
          }
          label=substr(labelAlt,2,lblTrimLen-1)
          labelAltsUq=c(labelAltsUq,label)
        }else{
          # check for labelGroup on level
          lvlDefs=dbConfig[['levelDefinitions']]
          isLabelGroup=FALSE
          for(lvlDef in lvlDefs){
            for(attrDef in lvlDef[['attributeDefinitions']]){
              if(lvlName==attrDef[['name']]){
                lblGrps=attrDef[['labelGroups']]
                for(lblGrp in lblGrps){
                  if(labelAlt==lblGrp[['name']]){
                    # is label group, expand
                    for(lblGrpVal in lblGrp[['values']]){
                      labelAltsUq=c(labelAltsUq,lblGrpVal)
                    }
                    isLabelGroup=TRUE
                    break
                  }
                }
              }
            }
          }
          if(!isLabelGroup){
            # check for database labelGroup
            dbLblGrps=dbConfig$labelGroups
            for(dbLblGrp in dbLblGrps){
              if(labelAlt==dbLblGrp[['name']]){
                # is label group, expand
                for(dbLblGrpVal in dbLblGrp[['values']]){
                  labelAltsUq=c(labelAltsUq,dbLblGrpVal)
                }
                isLabelGroup=TRUE
                break
              }
            }
          }
          if(!isLabelGroup){
            # ordinary label
            label=labelAlt
            labelAltsUq=c(labelAltsUq,label)
          }
        }
        
      }
      cond=NULL
      #if(length(labelAltsUq)==1){
      #  cond=create.condition.text(opr,label)
      #}else{
      cond = create_conditionTextAlternatives(opr, labelAltsUq)
      #}
      if(is.null(labels)){
        labels=getQueryTmpEmuDBs()[['queryLabels']]
      }
      query_labels(emuDBhandle, levelName = lvlName, intermResTablePrefix = intermResTablePrefix, cond, useSubsets)
      #res[['projectionItems']]=NULL
      if(projectionLevel){
        # rIts=res[['items']]
        # projection of the result items themself
        # lvlNms=rep(lvlName,nrow(rIts))
        # res[['projectionItems']]=data.frame(db_uuid=rIts[,'db_uuid'],session=rIts[,'session'],bundle=rIts[,'bundle'],seqStartId=rIts[,'seqStartId'],seqEndId=rIts[,'seqEndId'],seqLen=rIts[,'seqLen'],pSeqStartId=rIts[,'seqStartId'],pSeqEndId=rIts[,'seqEndId'],pSeqLen=rIts[,'seqLen'],pLevel=lvlNms,stringsAsFactors = FALSE)
        # res[['projectionAttrLevel']]=lvlName
        dbGetQuery(emuDBhandle$connection, paste0("INSERT INTO ", intermResTablePrefix, "IntermResultProjectionItemsTmp ", "SELECT * FROM ", intermResTablePrefix, "IntermResultItemsTmp"))
        dbGetQuery(emuDBhandle$connection, paste0("UPDATE ", intermResTablePrefix, "IntermResultMetaInfosTmp SET projectionAttrLevel = '", lvlName, "'"))
      }
      return()
    }
  }
  stop("Syntax error: No operator found.")
}

query_databaseEqlCONJQ<-function(emuDBhandle, q, intermResTablePrefix){
  # BNF: KONJA = EA,{'&',EA};
  qTrim=str_trim(q)
  conditions=list()
  # initialize with empty result
  # res=create.subtree(items=EMPTY_RESULT_DF,links=NULL,resultLevel=NULL,projectionItems=NULL)
  startPos=1
  p=0
  # items=getQueryTmpEmuDBs()[['queryItems']]
  # labels=getQueryTmpEmuDBs()[['queryLabels']]
  resultLevel=NULL
  projection=FALSE
  useSubsets = FALSE
  # parse through all terms of and (&) operation
  while(p>=0){
    # find ampersand '&' char
    p = get_stringPosition(string=qTrim,searchStr='&',pos=startPos,literalQuote="'")
    if(p==-1){
      # get single term
      condStr=str_trim(substring(qTrim,startPos))
    }else{
      # get leading term
      condStr=str_trim(substr(qTrim,startPos,p-1))
      # advance to next
      startPos=p+1
    }
    # execute query on term      
    query_databaseEqlSQ(emuDBhandle, condStr, intermResTablePrefix, useSubsets = useSubsets)
    # set resultLevel of first term
    if(is.null(resultLevel)){
      # termResLevel=res[['resultLevel']]
      termResLevel = dbGetQuery(emuDBhandle$connection, paste0("SELECT * FROM ", intermResTablePrefix, "IntermResultMetaInfosTmp"))$resultLevel
      if(!is.null(termResLevel)){
        resultLevel=termResLevel
      }
    }
    
    # seqIts=res[['items']]
    nRes = dbGetQuery(emuDBhandle$connection, paste0("SELECT COUNT(*) AS n FROM ", intermResTablePrefix, "IntermResultItemsTmp"))$n
    if(nRes==0){
      # empty result stop here and return
      return()
    }else{
      # remove all entries from subsets
      dbGetQuery(emuDBhandle$connection, "DELETE FROM itemsFilteredSubsetTmp")
      dbGetQuery(emuDBhandle$connection, "DELETE FROM labelsFilteredSubsetTmp")
      
      # Proceed with items matching current condition by placeing them into subset tabels
      dbGetQuery(emuDBhandle$connection, paste0("INSERT INTO itemsFilteredSubsetTmp ",
                                                "SELECT DISTINCT i.* FROM itemsFilteredTmp i, ", intermResTablePrefix, "IntermResultItemsTmp imr ",
                                                "WHERE i.db_uuid=imr.db_uuid AND i.session=imr.session AND i.bundle=imr.bundle AND i.itemID=imr.seqStartId"))
      
      dbGetQuery(emuDBhandle$connection, paste0("INSERT INTO labelsFilteredSubsetTmp ",
                                                "SELECT DISTINCT l.* FROM labelsFilteredTmp l, ", intermResTablePrefix, "IntermResultItemsTmp imr ",
                                                "WHERE l.db_uuid=imr.db_uuid AND l.session=imr.session AND l.bundle=imr.bundle AND l.itemID=imr.seqStartId"))
      useSubsets = TRUE
      
      # Proceed with items matching current condition
      # items=sqldf("SELECT i.* FROM items i,seqIts s WHERE i.db_uuid=s.db_uuid AND i.session=s.session AND i.bundle=s.bundle AND i.itemID=s.seqStartId")
      # labels=sqldf("SELECT l.* FROM labels l,seqIts s WHERE l.db_uuid=s.db_uuid AND l.session=s.session AND l.bundle=s.bundle AND l.itemID=s.seqStartId")
      nProjItems = dbGetQuery(emuDBhandle$connection, paste0("SELECT COUNT(*) AS n FROM ", intermResTablePrefix, "IntermResultProjectionItemsTmp"))$n
      if(!nProjItems == 0){
        # only one term can be marked with hashtag
        if(projection){
          stop("Only one hashtag allowed in linear query term: ",qTrim)
        }
        # if one of the boolean terms is marked with the hashtag the whole term is marked 
        projection=TRUE
        # projectionAttrLevel=res[['projectionAttrLevel']]
        projectionAttrLevel = dbGetQuery(emuDBhandle$connection, paste0("SELECT projectionAttrLevel FROM ", intermResTablePrefix, "IntermResultMetaInfosTmp"))$projectionAttrLevel
      }
    }
  }
  #res[['items']][,'level']=resultLevel
  dbGetQuery(emuDBhandle$connection, paste0("UPDATE ", intermResTablePrefix, "IntermResultItemsTmp ", "SET level ='", resultLevel, "'"))
  # items=res[['items']]
  # SIC... why is the items df set again?
  if(projection){
    # SIC no idea why this has to be set again? Commenting out for now because it is set by
    # qStr=paste0('SELECT i.db_uuid,i.session,i.bundle,i.seqStartId,i.seqEndId,i.seqStartId AS pSeqStartId ,i.seqEndId AS pSeqEndId,i.seqLen AS pSeqLen,"',projectionAttrLevel,'" AS pLevel FROM items i')
    # res[['projectionItems']]=sqldf(qStr)
  }
  # res[['resultLevel']]=resultLevel
  dbGetQuery(emuDBhandle$connection, paste0("UPDATE ", intermResTablePrefix, "IntermResultMetaInfosTmp ", "SET resultLevel = '", resultLevel, "'"))
  # return(res)
}

query_databaseEqlInBracket<-function(emuDBhandle, q, intermResTablePrefix = "left"){
  parseRes=list()
  qTrim=str_trim(q)
  # parse SEQA or DOMA
  seqPos = get_stringPositionOutsideBrackets(qTrim,'->',literalQuote="'",bracket=c('[',']'))
  domPos = get_stringPositionOutsideBrackets(qTrim,'^',literalQuote="'",bracket=c('[',']'))
  if(seqPos!=-1 || domPos!=-1){
    # items=getQueryTmpEmuDBs()[['queryItems']]
    # links=getQueryTmpEmuDBs()[['queryLinksExt']]
    # parse DOMA or SEQA
    lExpRes=NULL
    prjIts=NULL
    if(domPos!=-1){
      left=str_trim(substr(qTrim,1,domPos-1))
      right=str_trim(substring(qTrim,domPos+1))
    }else if(seqPos!=-1){
      left=str_trim(substr(qTrim,1,seqPos-1))
      right=str_trim(substring(qTrim,seqPos+2))
    }
    
    
    # check that left side is not a SQ -> ensure depth first traversal of parse tree of query
    brOpenPos = get_charPosition(left, '[', literalQuote="'")
    if(brOpenPos != -1){
      # if so query left side before querying right side
      query_databaseWithEql(emuDBhandle, left, intermResTablePrefix = "left")
      query_databaseWithEql(emuDBhandle, right, intermResTablePrefix = "right")
    }else{
      # else query right side first
      query_databaseWithEql(emuDBhandle, right, intermResTablePrefix = "right")
      query_databaseWithEql(emuDBhandle, left, intermResTablePrefix = "left")
    }
    
    #lResPIts=lRes[['projectionItems']]
    #rResPIts=rRes[['projectionItems']]
    nLeftProjItems = dbGetQuery(emuDBhandle$connection, paste0("SELECT COUNT(*) AS n FROM leftIntermResultProjectionItemsTmp"))$n
    nRightProjItems = dbGetQuery(emuDBhandle$connection, paste0("SELECT COUNT(*) AS n FROM rightIntermResultProjectionItemsTmp"))$n
    # lIsProj=!is.null(lResPIts)
    # rIsProj=!is.null(rResPIts)
    #cat("left: ",lIsProj," right: ",rIsProj,"\n")
    if(nLeftProjItems != 0 & nRightProjItems != 0){
      stop("Multiple hash tags '#' not allowed in EQL2 query!")
    }
    
    # get items on dominance compare levels
    # lResIts=lRes[['items']]
    # lResAttrName=lRes[['resultLevel']]
    lResAttrName = dbGetQuery(emuDBhandle$connection, "SELECT resultLevel FROM leftIntermResultMetaInfosTmp")$resultLevel
    lResLvl = get_levelNameForAttributeName(emuDBhandle, lResAttrName)
    
    
    # rResAttrName=rRes[['resultLevel']]
    rResAttrName = dbGetQuery(emuDBhandle$connection, "SELECT resultLevel FROM rightIntermResultMetaInfosTmp")$resultLevel
    rResLvl = get_levelNameForAttributeName(emuDBhandle, rResAttrName)
    if(domPos!=-1 & lResLvl==rResLvl){
      stop("Dominance query on same levels impossible.\nLeft level: ",lResLvl," (attr:",lResAttrName,") equals right level: ",lResLvl," (attr:",rResAttrName,")\n")
    }
    # check equal levels for sequence query
    # (Do this already at this point, fixes issue: Sequence query should always throw an error if arguments not on same level. #39 )
    if(seqPos!=-1 & lResAttrName!=rResAttrName){
      stop("Levels of sequence query '",qTrim,"' do not match. (",lResAttrName," not equal ",rResAttrName,")")
    }
    
    lLvlItems=NULL
    # sqldf cannot handle empty data frames 
    # lResItsNrows=nrow(lResIts)
    nLeftResIts = dbGetQuery(emuDBhandle$connection, paste0("SELECT COUNT(*) AS n FROM leftIntermResultItemsTmp"))$n
    if(nLeftResIts == 0){
      clear_intermResTabels(emuDBhandle, "left")
      clear_intermResTabels(emuDBhandle, "right")
      return()
    }
    #else{
    #  lqStr=paste0("SELECT i.* FROM lResIts ls,items i WHERE i.id=ls.seqStartId AND level='",lResAttrName,"'")
    #  #lqStr=paste0("SELECT i.* FROM lResIts ls,items i WHERE i.id=ls.seqStartId")
    #  lLvlItems=sqldf(lqStr)
    #}
    
    # rResIts=rRes[['items']]
    
    
    
    rLvlItems=NULL
    # sqldf cannot handle empty data frames 
    # rResItsNrows=nrow(rResIts)
    nRightResIts = dbGetQuery(emuDBhandle$connection, paste0("SELECT COUNT(*) AS n FROM rightIntermResultItemsTmp"))$n
    if(nRightResIts == 0){
      clear_intermResTabels(emuDBhandle, "left")
      clear_intermResTabels(emuDBhandle, "right")
      return()
    }
    
    if(domPos!=-1){
      # parse DOMA
      # query the result level of left term
      
      linksNrows=nrow(links)
      if(linksNrows==0){
        res=create.subtree(items=EMPTY_RESULT_DF,links=NULL,resultLevel=lResAttrName,projectionItems=NULL)
        return(res)
      }
      
      # right seq items
      #rSeqIts=list.seq.items(rResIts)
      itemsIdxSql='CREATE INDEX items_idx ON items(db_uuid,session,bundle,itemID)'
      #       rSeqIts=sqldf(c(itemsIdxSql,"SELECT i.* FROM items s,items i,items e,rResIts r WHERE \
      #                       s.db_uuid=i.db_uuid AND s.session=i.session AND s.bundle=i.bundle AND \
      #                       s.db_uuid=e.db_uuid AND s.session=e.session AND s.bundle=e.bundle AND \
      #                       s.db_uuid=r.db_uuid AND s.session=r.session AND s.bundle=r.bundle AND \
      #                       s.itemID=r.seqStartId AND e.itemID=r.seqEndId AND i.level=s.level AND i.seqIdx>=s.seqIdx AND i.seqIdx<=e.seqIdx"))
      lSeqRes=EMPTY_RESULT_DF
      
      # build dominance SQL query string 
      itemsSameBundleCond1="ils.db_uuid=irs.db_uuid AND ils.session=irs.session AND ils.bundle=irs.bundle AND "
      itemsSameBundleCond2="ils.db_uuid=irs.db_uuid AND ils.session=irs.session AND ils.bundle=irs.bundle AND "
      itemsSameBundleCond3="ils.db_uuid=ire.db_uuid AND ils.session=ire.session AND ils.bundle=ire.bundle AND "
      itemsSameBundleCond4="ils.db_uuid=lid.db_uuid AND ils.session=lid.session AND ils.bundle=lid.bundle AND "
      itemsSameBundleCond5="ils.db_uuid=rid.db_uuid AND ils.session=rid.session AND ils.bundle=rid.bundle"
      linkSameBundleCond1="k.db_uuid=ils.db_uuid AND k.db_uuid=irs.db_uuid AND k.session=ils.session AND k.session=irs.session AND k.bundle=ils.bundle AND k.bundle=irs.bundle"
      linkSameBundleCond2="m.db_uuid=ils.db_uuid AND m.db_uuid=irs.db_uuid AND m.session=ils.session AND m.session=irs.session AND m.bundle=ils.bundle AND m.bundle=irs.bundle"
      lDomQuerySelectStr="lid.db_uuid,lid.session,lid.bundle,lid.seqStartId,lid.seqEndId,lid.seqLen,lid.level"
      rDomQuerySelectStr="rid.seqStartId AS rsId,rid.seqEndId AS reId,rid.seqLen AS rL,rid.level AS rLev"
      domQueryFromStr="lResIts lid, rResIts rid,items ils, items irs,items ile,items ire"
      domQueryStrCond0=paste0(itemsSameBundleCond1,itemsSameBundleCond2,itemsSameBundleCond3,"ils.itemID=lid.seqStartId AND ile.itemID=lid.seqEndId AND ",itemsSameBundleCond4,"irs.itemID=rid.seqStartId AND ire.itemID=rid.seqEndId AND ",itemsSameBundleCond5)
      # The query has now the corners of the dominance "trapeze" in ils,ile,irs,ire
      # Check sequence start item of left result on existence of a link to the start item of the right sequence 
      domQueryStrCond1=paste0("EXISTS (SELECT * FROM links k WHERE ",linkSameBundleCond1," AND ((k.fromID=ils.itemID AND k.toID=irs.itemID) OR (k.toID=ils.itemID AND k.fromID=irs.itemID)))")
      # ... and sequence end item of left result on existence of a link to the end item of the right sequence 
      domQueryStrCond2=paste0("EXISTS (SELECT * FROM links m WHERE ",linkSameBundleCond2," AND ((m.fromID=ile.itemID AND m.toID=ire.itemID) OR (m.toID=ile.itemID AND m.fromID=ire.itemID)))")
      
      # concatenate the dominance query string
      domQueryStrTail=paste0(" FROM ",domQueryFromStr," WHERE ", domQueryStrCond0, " AND ", domQueryStrCond1," AND ",domQueryStrCond2)
      lrDomQueryStr=paste0("SELECT DISTINCT ",lDomQuerySelectStr,",",rDomQuerySelectStr,domQueryStrTail)
      
      # Indices for left and right result items and for links
      
      lResIdxSql='CREATE INDEX lResIts_idx ON lResIts(db_uuid,session,bundle,seqStartId,seqEndId)'
      rResIdxSql='CREATE INDEX rResIts_idx ON rResIts(db_uuid,session,bundle,seqStartId,seqEndId)'
      linksIdxSql='CREATE INDEX links_idx ON links(db_uuid,session,bundle,fromID,toID)'
      
      # execute index creation and then dominance query 
      lrExpRes=sqldf(c(lResIdxSql,rResIdxSql,itemsIdxSql,linksIdxSql,lrDomQueryStr))
      
      #lExpRes=data.frame(seqStartId=lrExpRes[,'seqStartId'],seqEndId=lrExpRes[,'seqEndId'],seqLen=lrExpRes[,'seqLen'],level=lrExpRes[,'level'],stringsAsFactors = FALSE)
      # lrExpRes might have double items, use a distinct select to create the data.frame for left term
      # for example in the query "[ Syllable=S ^ Phonetic=s ]" on ae there exists one Syllable S which dominates two Phonetic s items 
      # Fix for issue #12
      # Note: indices do not improve performance here 
      lExpRes=sqldf("SELECT DISTINCT db_uuid,session,bundle,seqStartId,seqEndId,seqLen,level FROM lrExpRes")
      lPrjIts=NULL
      rPrjIts=NULL
      if(nrow(lrExpRes)>0){
        if(!is.null(lResPIts)){
          # TODO
          #lPrjIts=reduce.projection.items(lExpRes,lResPIts)
          qStr="SELECT i.db_uuid,i.session,i.bundle,i.seqStartId,i.seqEndId,i.seqLen,pi.pSeqStartId,pi.pSeqEndId,pi.pSeqLen,pi.pLevel FROM lrExpRes i,lResPIts pi WHERE i.db_uuid=pi.db_uuid AND i.session=pi.session AND i.bundle=pi.bundle AND i.seqStartId=pi.seqStartId AND i.seqEndId=pi.seqEndId"
          #rQStr="SELECT rpi.* FROM rightProjectionItems rpi WHERE EXISTS (SELECT i.seqStartId FROM resultItems i WHERE i.seqStartId=rpi.seqStartId && i.seqEndId=rpi.seqEndId)"
          
          #qStr=paste0(lQStr," UNION ",rQStr)
          #lPrjIts=sqldf(c(lrExpResIdxSql,qStr))
          lPrjIts=sqldf(qStr)
        }
        
        if(!is.null(rResPIts) && nrow(rResPIts)>0){
          qStr="SELECT i.db_uuid,i.session,i.bundle,i.seqStartId,i.seqEndId,i.seqLen,pi.pSeqStartId,pi.pSeqEndId,pi.pSeqLen,pi.pLevel FROM lrExpRes i,rResPIts pi WHERE i.db_uuid=pi.db_uuid AND i.session=pi.session AND i.bundle=pi.bundle AND i.rsId=pi.seqStartId AND i.reId=pi.seqEndId"
          #rQStr="SELECT rpi.* FROM rightProjectionItems rpi WHERE EXISTS (SELECT i.seqStartId FROM resultItems i WHERE i.seqStartId=rpi.seqStartId && i.seqEndId=rpi.seqEndId)"
          
          #qStr=paste0(lQStr," UNION ",rQStr)
          #rPrjIts=sqldf(c(lrExpResIdxSql,qStr))
          rPrjIts=sqldf(qStr)
          
        }
      }
      prjIts=NULL
      if(!is.null(lPrjIts)){
        if(is.null(prjIts)){
          prjIts=lPrjIts
        }
      }
      if(!is.null(rPrjIts)){
        if(is.null(prjIts)){
          prjIts=rPrjIts
        }else{
          # union
          prjIts=rbind(prjIts,rPrjIts)
        }
      }
      
      
      
      
      
    }
    if(seqPos!=-1){

      # query the result level of left term (removed lid.seqEndId AS leId,rid.seqStartId AS rsId,)
      lrSeqQueryStr = paste0("SELECT lid.db_uuid,lid.session,lid.bundle,lid.seqStartId, rid.seqEndId,lid.seqLen+rid.seqLen AS seqLen,lid.level FROM leftIntermResultItemsTmp lid, rightIntermResultItemsTmp rid, itemsFilteredTmp il, itemsFilteredTmp ir WHERE \
                          il.db_uuid=ir.db_uuid AND il.session=ir.session AND il.bundle=ir.bundle AND \
                           il.db_uuid=lid.db_uuid AND il.session=lid.session AND il.bundle=lid.bundle AND \
                          il.db_uuid=rid.db_uuid AND il.session=rid.session AND il.bundle=rid.bundle AND \
                           il.itemID=lid.seqEndId AND ir.itemID=rid.seqStartId AND il.level=ir.level AND ir.seqIdx=il.seqIdx+1")
      
      lrExpRes = dbGetQuery(emuDBhandle$connection, lrSeqQueryStr)
      
      dbWriteTable(emuDBhandle$connection, paste0(intermResTablePrefix, "IntermResultItemsTmp"), lrExpRes, overwrite = T)
      # check if no sequences where found -> clear & return
      nSeq = dbGetQuery(emuDBhandle$connection, paste0("SELECT COUNT(*) AS n FROM ", intermResTablePrefix, "IntermResultItemsTmp"))$n
      if(nSeq == 0){
        clear_intermResTabels(emuDBhandle, "left")
        clear_intermResTabels(emuDBhandle, "right")
        return()
      }
      
      
      # dbGetQuery(emuDBhandle$connection, paste0("INSERT INTO ", intermResTablePrefix, "IntermResultMetaInfosTmp (resultLevel)", "VALUES ('", lResAttrName, "')"))
      
      # select final result columns
      # lExpRes=data.frame(db_uuid=lrExpRes[['db_uuid']],session=lrExpRes[['session']],bundle=lrExpRes[['bundle']],seqStartId=lrExpRes[,'seqStartId'],seqEndId=lrExpRes[,'seqEndId'],seqLen=lrExpRes[,'seqLen'],level=lrExpRes[,'level'],stringsAsFactors = FALSE)
      
      # lPrjIts=NULL
      if(nLeftProjItems != 0){
        # TODO
        #lPrjIts=reduce.projection.items(lExpRes,lResPIts)
        # qStr = paste0("SELECT i.db_uuid,i.session,i.bundle,i.seqStartId,i.seqEndId,pi.pSeqStartId,pi.pSeqEndId,pi.pSeqLen,pi.pLevel FROM ", paste0(intermResTablePrefix, "IntermResultItemsTmp"), " i, leftIntermResultProjectionItemsTmp pi WHERE \
        # i.db_uuid=pi.db_uuid AND i.session=pi.session AND i.bundle=pi.bundle AND i.seqStartId=pi.seqStartId AND i.leId=pi.seqEndId")
        #rQStr="SELECT rpi.* FROM rightProjectionItems rpi WHERE EXISTS (SELECT i.seqStartId FROM resultItems i WHERE i.seqStartId=rpi.seqStartId && i.seqEndId=rpi.seqEndId)"
        
        # ensure projection items are in correct tables
        allLeftPI = dbGetQuery(emuDBhandle$connection, "SELECT * FROM leftIntermResultProjectionItemsTmp")
        dbWriteTable(emuDBhandle$connection, paste0(intermResTablePrefix, "IntermResultProjectionItemsTmp"), allLeftPI, overwrite = T)
        # ensure meta infos are in correct tables
        allMeta = dbGetQuery(emuDBhandle$connection, "SELECT * FROM leftIntermResultMetaInfosTmp")
        dbWriteTable(emuDBhandle$connection, paste0(intermResTablePrefix, "IntermResultMetaInfowTmp"), allMeta, overwrite = T)
      }
      # rPrjIts=NULL
      if(nRightProjItems != 0){

        # qStr = paste0("SELECT i.db_uuid,i.session,i.bundle,i.seqStartId,i.seqEndId,pi.seqStartId,pi.seqEndId,pi.seqLen,pi.level FROM ", paste0(intermResTablePrefix, "IntermResultItemsTmp"), " i, rightIntermResultProjectionItemsTmp pi WHERE \
        # i.db_uuid=pi.db_uuid AND i.session=pi.session AND i.bundle=pi.bundle AND i.seqStartId=pi.seqStartId AND i.seqEndId=pi.seqEndId")
        # rPrjIts=sqldf(qStr)
        # dbGetQuery(emuDBhandle$connection, qStr)
        
        # ensure projection items are in correct tables
        allRightPI = dbGetQuery(emuDBhandle$connection, "SELECT * FROM rightIntermResultProjectionItemsTmp")
        dbWriteTable(emuDBhandle$connection, paste0(intermResTablePrefix, "IntermResultProjectionItemsTmp"), allRightPI, overwrite = T)
        # ensure meta infos are in correct tables
        allMeta = dbGetQuery(emuDBhandle$connection, "SELECT * FROM rightIntermResultMetaInfosTmp")
        dbWriteTable(emuDBhandle$connection, paste0(intermResTablePrefix, "IntermResultMetaInfowTmp"), allMeta, overwrite = T)
        
      }
      # no idea what this if for?      
      #       prjIts=NULL
      #       if(!is.null(lPrjIts)){
      #         if(is.null(prjIts)){
      #           prjIts=lPrjIts
      #         }
      #       }
      #       if(!is.null(rPrjIts)){
      #         if(is.null(prjIts)){
      #           prjIts=rPrjIts
      #         }else{
      #           # union
      #           prjIts=rbind(prjIts,rPrjIts)
      #         }
      #       }
      
    }
    
    # links of result tree ? No.
    
    # res=create.subtree(items=lExpRes,links=NULL,resultLevel=lResAttrName,projectionItems=prjIts)
    return()
  }else{
    query_databaseWithEql(emuDBhandle, qTrim, intermResTablePrefix)
  }
  #stop("Syntax error: Expected sequence '->' or domination '^' operator.")
}

## Query EMU database
## 
## @param database object of class emuDB
## @param query EQL query string
## @return EMU seglist
## @author Klaus Jaensch
## @import sqldf stringr
## @export
## @keywords emuDB database query Emu EQL 
## 
query.database.with.eql.seglist<-function(dbConfig,query){
  
  rs=query_databaseWithEql(dbConfig,query)
  segList=convert_queryResultToEmusegs(dbConfig,rs)
  return(segList)
  
}

query_databaseWithEqlEmuRsegs<-function(emuDBhandle, query, timeRefSegmentLevel){
  
  query_databaseWithEql(emuDBhandle, query)
  segList = convert_queryResultToEmuRsegs(emuDBhandle, timeRefSegmentLevel)
  return(segList)
  
}




## Query EMU database
## 
## @param database object of class emuDB
## @param query EQL query string
## @return result set object
## @author Klaus Jaensch
## @import sqldf stringr
## @export
## @keywords emuDB database query Emu EQL 
## 
query_databaseWithEql<-function(emuDBhandle, query, intermResTablePrefix = "left"){
  parseRes=list()
  qTrim=str_trim(query)
  brOpenPos=get_charPosition(qTrim,'[',literalQuote="'")
  if(brOpenPos==-1){
    query_databaseEqlCONJQ(emuDBhandle, qTrim, intermResTablePrefix = intermResTablePrefix)
    # res[['queryStr']]=query
    dbGetQuery(emuDBhandle$connection, paste0("UPDATE ", intermResTablePrefix, "IntermResultMetaInfosTmp SET queryStr = '", query, "'"))
    return()
  }else{
    
    brClosePos=get_lastCharPosition(qTrim,']',literalQuote="'")
    if(brClosePos==-1){
      stop("Syntax error: missing close bracket ']' for open bracket at pos ",brOpenPos,"\n")
    }
    
    if(brOpenPos!=1){
      stop("Syntax error: Expected open bracket '[' at the beginning\n")
    }
    
    if(brClosePos!=nchar(qTrim)){
      stop("Syntax error: Expected close bracket ']' at the end\n")
    }
    
    #parse string in bracket
    inBr=substr(qTrim,brOpenPos+1,brClosePos-1)
    query_databaseEqlInBracket(emuDBhandle, inBr, intermResTablePrefix)
    
    # inBrRes[['queryStr']]=query
    return()
    
  }
  stop("Unknown syntax error.")
}

##' Query emuDB
##' @description Function to query an emuDB
##' @details Evaluates a query string of query language queryLang on an emuDB referenced by dbName and returns a segment list of desired type resultType.  
##' For details of the query language please refer to the EQL vignette (type: \code{vignette('EQL')} ).
##' Returns a list of segments which meet the conditions given by the query string. A segment can consist of one (e.g. 's') or more (e.g. 's->t') items from the specified emuDB level. 
##' Segment objects (type 'SEGMENT') contain the label string and the start and end time information of the segment (in msec). \link{emuRsegs} objects additionally contain sample position of start and end item. 
##' Time information of symbolic elements (type 'ITEM') are derived from linked SEGMENT levels if available. If multiple linked SEGMENT levels exist, you can specify the level by argument \code{timeRefSegmentLevel}. If time and sample values cannot be derived they will be set to \code{\link{NA}}.
##' \link{emuRsegs} result lists will be ordered by the hidden columns UUID, session, bundle and sample start position. Legacy \link{emusegs} lists are ordered by the columns utts and start.
##' The query may be limited to session and/or bundle names specified by regular expression pattern strings (see \link{regex}) in parameters \code{sessionPattern} respectively \code{bundlePattern}.
##' @param emuDBhandle emuDB handle object (see \link{load_emuDB})
##' @param query string (see vignette \code{EQL})
##' @param sessionPattern A regular expression pattern matching session names to be searched from the database
##' @param bundlePattern A regular expression pattern matching bundle names to be searched from the database
##' @param queryLang query language used for evaluating the query string 
##' @param timeRefSegmentLevel set time derivation segment level
##' @param resultType type (class name) of result
##' @return result set object of class resultType (default: \link{emuRsegs}, compatible to legacy type \link{emusegs})
##' @author Klaus Jaensch
##' @import sqldf stringr
##' @export
##' @seealso \code{\link{load_emuDB}}
##' @keywords emuDB database query Emu EQL 
##' @examples
##' \dontrun{
##' ## Query database 'ae' using EQL query [Phonetic=t -> Phonetic=s]:
##' ## 'Find all sequences /ts/ in level Phonetics'.
##' ## and store result seglist in variable segListTs
##' 
##' seglistTs=query('ae','[Phonetic==t -> Phonetic==s]')
##' 
##' ## Query database 'ae' using EQL query [Syllable=S ^ Phoneme=t]:
##' ## 'Find all items 't' in level Phoneme that are dominated by items 'S' in level Syllable.'
##' ## Return legacy Emu result type 'emusegs'
##' 
##' query('ae','[Syllable==S ^ Phoneme==t]',resultType='emusegs')
##' 
##' ## Query 'p' items of level Phoneme from bundles whose bundle names start with 'msajc07' 
##' ## and whose session names start with '00'
##' ## (Note that here the query uses the operator '=' (meaning '==') which is kept for 
##' ##  backwards compatibilty to EQL1.)  
##' 
##' query('ae','Phoneme=p',bundlePattern='msajc05.*',sessionPattern ='00.*')
##' 
##' 
##' }
##' 

query <- function(emuDBhandle, query, sessionPattern = '.*', bundlePattern = '.*', queryLang = 'EQL2', timeRefSegmentLevel = NULL, resultType = NULL){
  
  if(queryLang=='EQL2'){
    # create temp tables 
    drop_tmpQueryTablesDBI(emuDBhandle)
    create_tmpQueryTablesDBI(emuDBhandle)
    
    queryItems <- dbGetQuery(emuDBhandle$connection, paste0("SELECT * FROM items WHERE db_uuid='", emuDBhandle$UUID, "'"))
    queryLabels <- dbGetQuery(emuDBhandle$connection, paste0("SELECT * FROM labels WHERE db_uuid='", emuDBhandle$UUID, "'"))
    queryLinksExt <- dbGetQuery(emuDBhandle$connection, paste0("SELECT * FROM linksExt WHERE db_uuid='", emuDBhandle$UUID,"'"))
    
    # if set get logical vectors that match sessionPattern and bundlePattern
    if(!is.null(sessionPattern) && sessionPattern!='.*'){
      sesSelIts = emuR.regexprl(sessionPattern, queryItems$session)
      sesSelLbls = emuR.regexprl(sessionPattern, queryLabels$session)
      sesSelLks = emuR.regexprl(sessionPattern, queryLinksExt$session)
    }else{
      sesSelIts = rep(TRUE, nrow(queryItems))
      sesSelLbls = rep(TRUE, nrow(queryLabels))
      sesSelLks = rep(TRUE, nrow(queryLinksExt))
    }
    if(!is.null(bundlePattern) && bundlePattern!='.*'){
      bndlSelIts = emuR.regexprl(bundlePattern, queryItems$bundle)
      bndlSelLbls = emuR.regexprl(bundlePattern, queryLabels$bundle)
      bndlSelLks = emuR.regexprl(bundlePattern, queryLinksExt$bundle)
    }else{
      bndlSelIts = rep(TRUE, nrow(queryItems))
      bndlSelLbls = rep(TRUE, nrow(queryLabels))
      bndlSelLks = rep(TRUE, nrow(queryLinksExt))
    }
    # write to tmp tables
    dbWriteTable(emuDBhandle$connection, "itemsFilteredTmp", queryItems[sesSelIts && bndlSelIts, ], overwrite = TRUE)
    dbWriteTable(emuDBhandle$connection, "labelsFilteredTmp", queryLabels[sesSelLbls && bndlSelLbls, ], overwrite = TRUE)
    dbWriteTable(emuDBhandle$connection, "linksExtFilteredTmp", queryLinksExt[sesSelLks && bndlSelLks, ], overwrite = TRUE)
    
    
    if(is.null(resultType)){
      emuRsegs = query_databaseWithEqlEmuRsegs(emuDBhandle,query,timeRefSegmentLevel)
      drop_tmpQueryTablesDBI(emuDBhandle)
      return(emuRsegs)
    }else{
      if(resultType=='emuRsegs'){
        emuRsegs = query_databaseWithEqlEmuRsegs(emuDBhandle,query,timeRefSegmentLevel)
        drop_tmpQueryTablesDBI(emuDBhandle)
        return(emuRsegs)
      }else if(resultType=='emusegs'){
        if(!is.null(timeRefSegmentLevel)){
          # TODO 
          stop("Parameter timeRefSegmentLevel not yet supported for resultType 'emusegs'. Please use resultType 'emuRsegs' (default).")
        }
        return(query.database.with.eql.seglist(emuDBhandle,query))
      }else{
        stop("Unknown result type: '",resultType,"'. Supported result types: 'emuRsegs', emusegs'")
      }
    }
    # free temp tables
    setQueryTmpEmuDBs(NULL)
    
  }else{
    stop("Unknown query language '",queryLang,"'.")
  }
}

create_tmpQueryTablesDBI <- function(emuDBhandle){
  
  dbGetQuery(emuDBhandle$connection, database.DDL.emuDB_itemsFilteredTmp)
  dbGetQuery(emuDBhandle$connection, database.DDL.emuDB_labelsFilteredTmp)
  dbGetQuery(emuDBhandle$connection, database.DDL.emuDB_labelsFilteredTmp_idx)
  dbGetQuery(emuDBhandle$connection, database.DDL.emuDB_linksExtFilteredTmp)
  dbGetQuery(emuDBhandle$connection, database.DDL.emuDB_linksExtFilteredTmp_idx)
  
  dbGetQuery(emuDBhandle$connection, database.DDL.emuDB_itemsFilteredSubsetTmp)
  dbGetQuery(emuDBhandle$connection, database.DDL.emuDB_labelsFilteredSubsetTmp)
  dbGetQuery(emuDBhandle$connection, database.DDL.emuDB_labelsFilteredSubsetTmp_idx)
  dbGetQuery(emuDBhandle$connection, database.DDL.emuDB_linksExtFilteredSubsetTmp)
  dbGetQuery(emuDBhandle$connection, database.DDL.emuDB_linksExtFilteredSubsetTmp_idx)
  
  dbGetQuery(ae$connection, database.DDL.emuDB_leftIntermResultItemsTmp)
  dbGetQuery(ae$connection, database.DDL.emuDB_leftIntermResultItemsTmp_idx)
  dbGetQuery(ae$connection, database.DDL.emuDB_leftIntermResultLinksTmp)
  dbGetQuery(ae$connection, database.DDL.emuDB_leftIntermResultMetaInfosTmp)
  dbGetQuery(ae$connection, database.DDL.emuDB_leftIntermResultProjectionItemsTmp)
  
  dbGetQuery(ae$connection, database.DDL.emuDB_rightIntermResultItemsTmp)
  dbGetQuery(ae$connection, database.DDL.emuDB_rightIntermResultLinksTmp)
  dbGetQuery(ae$connection, database.DDL.emuDB_rightIntermResultMetaInfosTmp)
  dbGetQuery(ae$connection, database.DDL.emuDB_rightIntermResultProjectionItemsTmp)
  
  dbGetQuery(ae$connection, database.DDL.emuDB_bracketsIntermResultItemsTmp)
  
}

drop_tmpQueryTablesDBI <- function(emuDBhandle){
  tableNames = dbListTables(emuDBhandle$connection)
  if("itemsFilteredTmp" %in% tableNames) dbGetQuery(emuDBhandle$connection, "DROP TABLE itemsFilteredTmp")
  if("labelsFilteredTmp" %in% tableNames) dbGetQuery(emuDBhandle$connection, "DROP TABLE labelsFilteredTmp")
  if("linksExtFilteredTmp" %in% tableNames) dbGetQuery(emuDBhandle$connection, "DROP TABLE linksExtFilteredTmp")
  
  if("itemsFilteredSubsetTmp" %in% tableNames) dbGetQuery(emuDBhandle$connection, "DROP TABLE itemsFilteredSubsetTmp")
  if("itemsFilteredSubsetTmp" %in% tableNames) dbGetQuery(emuDBhandle$connection, "DROP TABLE labelsFilteredSubsetTmp")
  if("itemsFilteredSubsetTmp" %in% tableNames) dbGetQuery(emuDBhandle$connection, "DROP TABLE linksExtFilteredSubsetTmp")
  
  if("leftIntermResultItemsTmp" %in% tableNames) dbGetQuery(emuDBhandle$connection, "DROP TABLE leftIntermResultItemsTmp")
  if("leftIntermResultLinksTmp" %in% tableNames) dbGetQuery(emuDBhandle$connection, "DROP TABLE leftIntermResultLinksTmp")
  if("leftIntermResultMetaInfosTmp" %in% tableNames) dbGetQuery(emuDBhandle$connection, "DROP TABLE leftIntermResultMetaInfosTmp")
  if("leftIntermResultProjectionItemsTmp" %in% tableNames) dbGetQuery(emuDBhandle$connection, "DROP TABLE leftIntermResultProjectionItemsTmp")
  
  if("rightIntermResultItemsTmp" %in% tableNames) dbGetQuery(emuDBhandle$connection, "DROP TABLE rightIntermResultItemsTmp")
  if("rightIntermResultLinksTmp" %in% tableNames) dbGetQuery(emuDBhandle$connection, "DROP TABLE rightIntermResultLinksTmp")
  if("rightIntermResultMetaInfosTmp" %in% tableNames) dbGetQuery(emuDBhandle$connection, "DROP TABLE rightIntermResultMetaInfosTmp")
  if("rightIntermResultProjectionItemsTmp" %in% tableNames) dbGetQuery(emuDBhandle$connection, "DROP TABLE rightIntermResultProjectionItemsTmp")
  
  if("bracketsIntermResultItemsTmp" %in% tableNames) dbGetQuery(emuDBhandle$connection, "DROP TABLE bracketsIntermResultItemsTmp")
  
}

print.emuDB.query.result<-function(queryResult){
  cat("Item sequences:\n")
  print(queryResult[['items']])
  cat("\nResult level: ",queryResult[['resultLevel']],"\n")
  cat("Query was: ",queryResult[['queryStr']],"\n")
  
}
