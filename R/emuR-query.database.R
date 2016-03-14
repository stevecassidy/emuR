requireNamespace("stringr", quietly = T)

###########################################################################
# create table / index definitions for DBI that are used at query time

create_tmpFilteredQueryTablesDBI <- function(emuDBhandle){
  
  # tabels that store "filtered" items, labels and linksExt (when session/bundlePatterns are used)
  database.DDL.emuDB_itemsFilteredTmp = gsub("CREATE TABLE items", "CREATE TEMP TABLE itemsFilteredTmp", database.DDL.emuDB_items)
  database.DDL.emuDB_itemsFilteredTmp_idx = "CREATE INDEX itemsFilteredTmp_idx ON itemsFilteredTmp(db_uuid,session,bundle,itemID)"
  
  database.DDL.emuDB_labelsFilteredTmp = gsub("CREATE TABLE labels", "CREATE TEMP TABLE labelsFilteredTmp", database.DDL.emuDB_labels)
  database.DDL.emuDB_labelsFilteredTmp_idx = "CREATE INDEX labelsFilteredTmp_idx ON labelsFilteredTmp(itemID,db_uuid,session,bundle,name)"

  database.DDL.emuDB_linksFilteredTmp = gsub("CREATE TABLE links", "CREATE TEMP TABLE linksFilteredTmp", database.DDL.emuDB_links)
  database.DDL.emuDB_linksFilteredTmp_idx = 'CREATE INDEX linksFilteredTmp_idx ON linksFilteredTmp(db_uuid,session,bundle,fromID,toID)'
    
  database.DDL.emuDB_linksExtFilteredTmp = gsub("CREATE TABLE linksExt", "CREATE TEMP TABLE linksExtFilteredTmp", database.DDL.emuDB_linksExt)
  database.DDL.emuDB_linksExtFilteredTmp_idx = 'CREATE INDEX linksExtFilteredTmp_idx ON linksExtFilteredTmp(db_uuid,session,bundle,fromID,toID)'
  
  DBI::dbGetQuery(emuDBhandle$connection, database.DDL.emuDB_itemsFilteredTmp)
  DBI::dbGetQuery(emuDBhandle$connection, database.DDL.emuDB_itemsFilteredTmp_idx)
  DBI::dbGetQuery(emuDBhandle$connection, database.DDL.emuDB_labelsFilteredTmp)
  DBI::dbGetQuery(emuDBhandle$connection, database.DDL.emuDB_labelsFilteredTmp_idx)
  DBI::dbGetQuery(emuDBhandle$connection, database.DDL.emuDB_linksFilteredTmp)
  DBI::dbGetQuery(emuDBhandle$connection, database.DDL.emuDB_linksFilteredTmp_idx)
  DBI::dbGetQuery(emuDBhandle$connection, database.DDL.emuDB_linksExtFilteredTmp)
  DBI::dbGetQuery(emuDBhandle$connection, database.DDL.emuDB_linksExtFilteredTmp_idx)
  
  # tabels that store subsets of filtered tables
  database.DDL.emuDB_itemsFilteredSubsetTmp = gsub("CREATE TABLE items", "CREATE TEMP TABLE itemsFilteredSubsetTmp", database.DDL.emuDB_items) 
  database.DDL.emuDB_itemsFilteredSubsetTmp_idx = "CREATE INDEX itemsFilteredSubsetTmp_idx ON itemsFilteredSubsetTmp(db_uuid,session,bundle,itemID)"
  database.DDL.emuDB_labelsFilteredSubsetTmp = gsub("CREATE TABLE labels", "CREATE TEMP TABLE labelsFilteredSubsetTmp", database.DDL.emuDB_labels)
  database.DDL.emuDB_labelsFilteredSubsetTmp_idx = "CREATE INDEX labelsFilteredSubsetTmp_idx ON labelsFilteredSubsetTmp(itemID,db_uuid,session,bundle,name)"
  database.DDL.emuDB_linksExtFilteredSubsetTmp = gsub("CREATE TABLE linksExt", "CREATE TEMP TABLE linksExtFilteredSubsetTmp", database.DDL.emuDB_linksExt)
  database.DDL.emuDB_linksExtFilteredSubsetTmp_idx = 'CREATE INDEX linksExtFilteredSubsetTmp_idx ON linksExtFilteredSubsetTmp(db_uuid,session,bundle,fromID,toID)'
  
  DBI::dbGetQuery(emuDBhandle$connection, database.DDL.emuDB_itemsFilteredSubsetTmp)
  DBI::dbGetQuery(emuDBhandle$connection, database.DDL.emuDB_itemsFilteredSubsetTmp_idx)
  DBI::dbGetQuery(emuDBhandle$connection, database.DDL.emuDB_labelsFilteredSubsetTmp)
  DBI::dbGetQuery(emuDBhandle$connection, database.DDL.emuDB_labelsFilteredSubsetTmp_idx)
  
  database.DDL.emuDB_lrExpResTmp = paste0("CREATE TEMP TABLE lrExpResTmp (",
                                          "db_uuid VARCHAR(36),",
                                          "session TEXT,",
                                          "bundle TEXT,",
                                          "lSeqStartId INTEGER,",
                                          "lSeqEndId INTEGER,",
                                          "lSeqLen INTEGER,",
                                          "lLevel TEXT,",
                                          "rSeqStartId INTEGER,",
                                          "rSeqEndId INTEGER,",
                                          "rSeqLen INTERGER,",
                                          "rLevel TEXT,",
                                          "FOREIGN KEY (db_uuid, session, bundle) REFERENCES bundle(db_uuid, session_name, name)",
                                          ");")
  
  database.DDL.emuDB_lrExpResTmp_idx = 'CREATE INDEX lrExpResTmp_idx ON lrExpResTmp(db_uuid,session,bundle)'
  
  DBI::dbGetQuery(emuDBhandle$connection, database.DDL.emuDB_lrExpResTmp)
  DBI::dbGetQuery(emuDBhandle$connection, database.DDL.emuDB_lrExpResTmp_idx)
  
}

#####################################
create_intermResTmpQueryTablesDBI <- function(emuDBhandle, suffix = "root"){
  
  database.DDL.emuDB_intermRes_itemsTmp = paste0("CREATE TEMP TABLE intermRes_itemsTmp_", suffix, " (",
                                                 "db_uuid VARCHAR(36),",
                                                 "session TEXT,",
                                                 "bundle TEXT,",
                                                 "seqStartId INTEGER,",
                                                 "seqEndId INTEGER,",
                                                 "seqLen INTEGER,",
                                                 "level TEXT,",
                                                 "PRIMARY KEY (db_uuid, session, bundle, seqStartId, seqEndId),",
                                                 "FOREIGN KEY (db_uuid, session, bundle) REFERENCES bundle(db_uuid, session_name, name)",
                                                 ");")
  
  database.DDL.emuDB_intermRes_itemsTmp_idx = paste0("CREATE INDEX intermRes_itemsTmp_", suffix, "_idx ON intermRes_itemsTmp_", suffix, "(db_uuid,session,bundle,seqStartId,seqEndId,seqLen,level)")
  
  database.DDL.emuDB_intermRes_metaInfosTmp = paste0("CREATE TEMP TABLE intermRes_metaInfosTmp_", suffix, " (",
                                                     "resultLevel TEXT,",
                                                     "projectionAttrLevel TEXT,",
                                                     "queryStr TEXT",
                                                     ");")
  
  database.DDL.emuDB_intermRes_metaInfosTmp_idx = paste0("CREATE INDEX intermRes_metaInfosTmp_", suffix, "_idx ON intermRes_metaInfosTmp_", suffix, "(resultLevel,projectionAttrLevel,queryStr)")
  
  database.DDL.emuDB_intermRes_projItemsTmp = paste0("CREATE TEMP TABLE intermRes_projItemsTmp_", suffix, " (",
                                                     "db_uuid VARCHAR(36),",
                                                     "session TEXT,",
                                                     "bundle TEXT,",
                                                     "seqStartId INTEGER,",
                                                     "seqEndId INTEGER,",
                                                     "pSeqStartId INTEGER,",
                                                     "pSeqEndId INTEGER,",
                                                     "pSeqLen INTEGER,",
                                                     "pLevel TEXT,",
                                                     "FOREIGN KEY (db_uuid, session, bundle) REFERENCES bundle(db_uuid, session_name, name)",
                                                     ");")
  
  database.DDL.emuDB_intermRes_projItemsTmp_idx = paste0("CREATE INDEX intermRes_projItemsTmp_", suffix, "_idx ON intermRes_projItemsTmp_", suffix, "(db_uuid,session,bundle,seqStartId,seqEndId)")
  
  DBI::dbGetQuery(emuDBhandle$connection, database.DDL.emuDB_intermRes_itemsTmp)
  DBI::dbGetQuery(emuDBhandle$connection, database.DDL.emuDB_intermRes_itemsTmp_idx)
  DBI::dbGetQuery(emuDBhandle$connection, database.DDL.emuDB_intermRes_metaInfosTmp)
  DBI::dbGetQuery(emuDBhandle$connection, database.DDL.emuDB_intermRes_metaInfosTmp_idx)
  DBI::dbGetQuery(emuDBhandle$connection, database.DDL.emuDB_intermRes_projItemsTmp)
  DBI::dbGetQuery(emuDBhandle$connection, database.DDL.emuDB_intermRes_projItemsTmp_idx)
}

drop_tmpFilteredQueryTablesDBI <- function(emuDBhandle){
  tableNames = DBI::dbListTables(emuDBhandle$connection)
  if("itemsFilteredTmp" %in% tableNames) DBI::dbGetQuery(emuDBhandle$connection, "DROP TABLE itemsFilteredTmp")
  if("labelsFilteredTmp" %in% tableNames) DBI::dbGetQuery(emuDBhandle$connection, "DROP TABLE labelsFilteredTmp")
  if("linksFilteredTmp" %in% tableNames) DBI::dbGetQuery(emuDBhandle$connection, "DROP TABLE linksFilteredTmp")
  if("linksExtFilteredTmp" %in% tableNames) DBI::dbGetQuery(emuDBhandle$connection, "DROP TABLE linksExtFilteredTmp")
  
  if("itemsFilteredSubsetTmp" %in% tableNames) DBI::dbGetQuery(emuDBhandle$connection, "DROP TABLE itemsFilteredSubsetTmp")
  if("labelsFilteredSubsetTmp" %in% tableNames) DBI::dbGetQuery(emuDBhandle$connection, "DROP TABLE labelsFilteredSubsetTmp")
}

drop_allTmpTablesDBI <- function(emuDBhandle){
  allTables = DBI::dbListTables(emuDBhandle$connection)
  allTmpTables = allTables[grepl(".*Tmp.*", allTables)]
  for(tmpTable in allTmpTables){
    DBI::dbGetQuery(emuDBhandle$connection, paste0("DROP TABLE ", tmpTable))
  }
}

## @param emuDBhandle
## @param intermResTableSuffix
clear_intermResTabels <- function(emuDBhandle, intermResTableSuffix, clearProjectionItems = TRUE){
  DBI::dbGetQuery(emuDBhandle$connection, paste0("DELETE FROM intermRes_itemsTmp_", intermResTableSuffix))
  DBI::dbGetQuery(emuDBhandle$connection, paste0("DELETE FROM intermRes_metaInfosTmp_", intermResTableSuffix))
  if(clearProjectionItems) DBI::dbGetQuery(emuDBhandle$connection, paste0("DELETE FROM intermRes_projItemsTmp_", intermResTableSuffix))
}
###################################################################
################## Functions implementing EQL #####################
###################################################################

#################################
# helper functions for query

create_conditionTextAlternatives<-function(opr,values){
  o=list(opr=opr,values=values)
  return(o)
}

emuR_regexprl<-function(pattern,x){
  m=regexpr(pattern,x)
  return((m==1) & (attr(m,'match.length')==nchar(x)))
}

check_levelAttributeName<-function(emuDBhandle,name){
  aNms=get_allAttributeNames(emuDBhandle)
  if(! (name %in% aNms)){
    stop("Unknown level attribute name: '",name,"'. Database attribute names are: ",paste(aNms,collapse=','),"\n")
  }
}

#################################
# actual query functions

## @param emuDBhandle
## @param levelName
## @param intermResTablePrefix "left" or "right" 
query_labels <- function(emuDBhandle, levelName, intermResTableSuffix, conditionText, useSubsets){
  
  if(useSubsets){
    labelTableName = "labelsFilteredSubsetTmp"
  }else{
    labelTableName = "labelsFilteredTmp"
  }
  # clear tables but keep projectionItems so they don't get lost in queries like : [Text == the -> #Text =~ .* & Accent == S]  (right side of ->)
  clear_intermResTabels(emuDBhandle, intermResTableSuffix, clearProjectionItems = FALSE)
  opr=conditionText[['opr']]
  values=conditionText[['values']]
  res=NULL
  if(opr=='==' | opr=='='){
    for(value in values){
      DBI::dbGetQuery(emuDBhandle$connection, paste0("INSERT INTO intermRes_itemsTmp_", intermResTableSuffix, " ",
                                                "SELECT db_uuid, session, bundle, itemID AS seqStartId, itemID AS seqEndId, 1 AS seqLen,'", 
                                                levelName, "' AS level FROM ", labelTableName, " ",
                                                "WHERE name = '", levelName, "' AND label = '", value, "'"))
    }
  }else if(opr=='!='){   
    sqlStr = paste0("INSERT INTO intermRes_itemsTmp_", intermResTableSuffix, " ", 
                    "SELECT db_uuid, session, bundle, itemID AS seqStartId, itemID AS seqEndId, 1 AS seqLen,'", 
                    levelName, "' AS level FROM ", labelTableName, " ",
                    "WHERE name = '", levelName, "'")
    for(value in values){
      sqlStr = paste0(sqlStr, " AND label <> '", value, "'")
    }
    DBI::dbGetQuery(emuDBhandle$connection, sqlStr)
    
  }else if(opr=='=~'){
    for(value in values){
      ldf = DBI::dbGetQuery(emuDBhandle$connection, paste0("SELECT db_uuid, session, bundle, itemID AS seqStartId, itemID AS seqEndId, 1 AS seqLen,'", 
                                                      levelName, "' AS level, label FROM ", labelTableName, " ",
                                                      "WHERE name = '", levelName, "'"))
      ssl = emuR_regexprl(value,ldf[['label']])
      res = ldf[ssl,]
      DBI::dbWriteTable(emuDBhandle$connection, paste0("intermRes_itemsTmp_", intermResTableSuffix), subset(res, select = -label), append = T) # label column is ignored by DBI::dbWriteTable
    }
  }else if(opr=='!~'){
    for(value in values){
      ldf = DBI::dbGetQuery(emuDBhandle$connection, paste0("SELECT db_uuid, session, bundle, itemID AS seqStartId, itemID AS seqEndId, 1 AS seqLen,'", 
                                                      levelName, "' AS level, label FROM ", labelTableName, " ",
                                                      "WHERE name = '", levelName, "'"))
      ssl = !emuR_regexprl(value,ldf[['label']])
      res = ldf[ssl,]
      DBI::dbWriteTable(emuDBhandle$connection, paste0("intermRes_itemsTmp_", intermResTableSuffix), subset(res, select = -label), append = T) # label column is ignored by DBI::dbWriteTable
    }
  }else{
    stop("Syntax error: Unknown operator: '",opr,"'\n")
  }
  # clear insert resultLevel
  DBI::dbGetQuery(emuDBhandle$connection, paste0("DELETE FROM intermRes_metaInfosTmp_", intermResTableSuffix))
  DBI::dbGetQuery(emuDBhandle$connection, paste0("INSERT INTO intermRes_metaInfosTmp_", intermResTableSuffix, " (resultLevel) ",
                                            "VALUES ('", levelName, "')"))
}


query_databaseEqlFUNCQ<-function(emuDBhandle, q, intermResTableSuffix, useSubsets){
  # BNF: FUNCQ = POSQ | NUMQ;
  qTrim=stringr::str_trim(q)
  if(useSubsets){
    itemsTableName = "itemsFilteredSubsetTmp"
  }else{
    itemsTableName = "itemsFilteredTmp"
  }
  
  
  # determine function name
  # TODO duplicate code
  prbOpen = get_stringPosition(string=qTrim,searchStr='(',literalQuote="'")
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
      param1=stringr::str_trim(params[[1]])
      param2=stringr::str_trim(params[[2]])
      # check attribute names
      aNms=get_allAttributeNames(emuDBhandle)
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
      
      funcValueTerm=stringr::str_trim(substring(qTrim,prbClose+1))
      
      
      funcName=stringr::str_trim(substr(qTrim,1,prbOpen-1))
      # BNF: POSA = POSFKT,'(',EBENE,',',EBENE,')','=','0'| '1';
      itemsAsSeqs=NULL
      
      level1=get_levelNameForAttributeName(emuDBhandle, param1)
      level2=get_levelNameForAttributeName(emuDBhandle, param2)
      
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
          funcValue=stringr::str_trim(substring(text=funcValueTerm,first=funcValuePos))
        }else{
          stop("Syntax error: function ",funcName," requires function value in: '",qTrim,"'\n")
        }
      } 
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
                        FROM ", itemsTableName, " i, itemsFilteredTmp d \
                        WHERE i.db_uuid=d.db_uuid AND i.session=d.session AND i.bundle=d.bundle\
                         AND i.level='",level2,"' AND d.level='",level1,"' \
                         AND EXISTS \
                          (SELECT * FROM linksExtFilteredTmp k \
                           WHERE d.db_uuid=k.db_uuid AND d.session=k.session AND d.bundle=k.bundle \
                            AND i.db_uuid=k.db_uuid AND i.session=k.session AND i.bundle=k.bundle \
                            AND k.fromID=d.itemID AND k.toID=i.itemID AND k.toSeqIdx",cond,"0\
                           )") 
        
        itemsAsSeqs = DBI::dbGetQuery(emuDBhandle$connection, sqlQStr)
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
                       FROM ", itemsTableName, " i,itemsFilteredTmp d \
                       WHERE i.db_uuid=d.db_uuid AND i.session=d.session AND i.bundle=d.bundle \
                       AND i.level='",level2,"' AND d.level='",level1,"' \
                       AND EXISTS \
                       (SELECT * FROM linksExtFilteredTmp k \
                       WHERE d.db_uuid=k.db_uuid AND d.session=k.session AND d.bundle=k.bundle \
                        AND i.db_uuid=k.db_uuid AND i.session=k.session AND i.bundle=k.bundle \
                        AND k.fromID=d.itemID AND k.toID=i.itemID AND (k.toSeqIdx",cond,"0 ",bOp," k.toSeqIdx+1",cond,"k.toSeqLen)\
                       )") 
        itemsAsSeqs = DBI::dbGetQuery(emuDBhandle$connection, sqlQStr)
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
                       ", itemsTableName, " i, itemsFilteredTmp d \
                       WHERE i.db_uuid=d.db_uuid AND i.session=d.session AND i.bundle=d.bundle \
                       AND i.level='",level2,"' AND d.level='",level1,"' AND EXISTS \
                       (SELECT * FROM linksExtFilteredTmp k \
                        WHERE d.db_uuid=k.db_uuid AND d.session=k.session AND d.bundle=k.bundle \
                            AND i.db_uuid=k.db_uuid AND i.session=k.session AND i.bundle=k.bundle \
                            AND k.fromID=d.itemID AND k.toID=i.itemID AND k.toSeqIdx+1",cond,"k.toSeqLen\
                       )")
        itemsAsSeqs = DBI::dbGetQuery(emuDBhandle$connection, sqlQStr)
        resultLevel=param2
      }else if(funcName=='Num'){
        funcVal=NULL
        funcOpr=NULL
        for(opr in c('==','!=','<=','>=','=','>','<')){
          p=get_stringPosition(string=funcValueTerm,searchStr=opr)
          if(p==1){
            oprLen=nchar(opr)
            funcOpr=substr(funcValueTerm,1,oprLen)
            funcValStr=stringr::str_trim(substring(funcValueTerm,oprLen+1))
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
        sqlQStr=paste0("SELECT DISTINCT d.db_uuid,d.session,d.bundle,d.itemID AS seqStartId, d.itemID AS seqEndId,1 AS seqLen,'",param1,"' AS level \
                       FROM ", itemsTableName, " d \
                      WHERE (SELECT count(*) FROM itemsFilteredTmp  i WHERE i.db_uuid=d.db_uuid AND i.session=d.session AND i.bundle=d.bundle \
                       AND i.level='",level2,"' AND d.level='",level1,"' AND EXISTS \
                       (SELECT * FROM linksExtFilteredTmp k \
                       WHERE d.db_uuid=k.db_uuid AND d.session=k.session AND d.bundle=k.bundle \
                        AND i.db_uuid=k.db_uuid AND i.session=k.session AND i.bundle=k.bundle \
                        AND ( \
                         ( k.fromID=d.itemID AND k.toID=i.itemID AND k.toLevel=i.level ) OR \
                         ( k.fromID=i.itemID AND k.toID=d.itemID AND k.toLevel=d.level ) \
                        )\
                       ))",sqlFuncOpr,funcVal)
        itemsAsSeqs = DBI::dbGetQuery(emuDBhandle$connection, sqlQStr)
        resultLevel=param1
      }else{
        stop("Syntax error: Unknwon function: '",funcName,"'")
      }
      DBI::dbGetQuery(emuDBhandle$connection, paste0("DELETE FROM intermRes_itemsTmp_", intermResTableSuffix))
      DBI::dbWriteTable(emuDBhandle$connection, paste0("intermRes_itemsTmp_", intermResTableSuffix), itemsAsSeqs, append = T)
      # move meta infos to correct table
      DBI::dbGetQuery(emuDBhandle$connection, paste0("DELETE FROM intermRes_metaInfosTmp_", intermResTableSuffix))
      DBI::dbGetQuery(emuDBhandle$connection, paste0("INSERT INTO intermRes_metaInfosTmp_", intermResTableSuffix, " VALUES ('", resultLevel, "', NULL, '", qTrim, "')"))
      
    }
  }else{
    stop("Syntax error: Missing opening round bracket '(' in '",q,"'\n")
  }
}

query_databaseEqlSQ <- function(emuDBhandle, q, intermResTableSuffix, useSubsets){
  # BNF: SQ = LABELQ | FUNCQ;
  qTrim=stringr::str_trim(q)
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
      res=query_databaseEqlFUNCQ(emuDBhandle, qTrim, intermResTableSuffix, useSubsets)
    }
  }else{
    # No round brackets, assuming a level query
    query_databaseEqlLABELQ(emuDBhandle, qTrim, useSubsets, intermResTableSuffix = intermResTableSuffix)
  }
}

query_databaseEqlLABELQ <- function(emuDBhandle, q, useSubsets, intermResTableSuffix){
  # BNF: ETIKETTA = ['#'],EBENE,('=' | '!='),ETIKETTALTERNATIVEN;
  
  qTrim=stringr::str_trim(q)
  dbConfig = load_DBconfig(emuDBhandle)
  for(opr in c('==','!=','=~','!~','=')){
    p = get_stringPosition(string = q, searchStr = opr, literalQuote = "'")
    if(p!=-1){
      oprLen=nchar(opr)
      level=substr(q,1,p-1)
      projectionLevel=FALSE
      lvlTrim=stringr::str_trim(level)
      lvlName=lvlTrim
      if(grepl('^#',lvlTrim)){
        # projection marker
        # the BNF does not allow white space between '#' and level string
        # but the implementation of Emu does, so we allow it here too
        
        lvlName=stringr::str_trim(substring(lvlTrim,2))
        projectionLevel=TRUE
      }
      aNms = get_allAttributeNames(emuDBhandle)
      if(! (lvlName %in% aNms)){
        stop("Unknown level attribute name: '",lvlName,"'. Database attribute names are: ",paste(aNms,collapse=','),"\n")
      }
      labelStr=substring(q,p+oprLen)
      labelTrim=stringr::str_trim(labelStr)  
      
      
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
          labelAlt=stringr::str_trim(labelAltTerm)
          labelAlts=c(labelAlts,labelAlt)
          lp=lsp+1
        }
      }
      # add last term
      labelAltTerm=substring(labelTrim,lp)
      labelAlt=stringr::str_trim(labelAltTerm)
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
      cond = create_conditionTextAlternatives(opr, labelAltsUq)
      query_labels(emuDBhandle, levelName = lvlName, intermResTableSuffix = intermResTableSuffix, cond, useSubsets)
      if(projectionLevel){
        DBI::dbGetQuery(emuDBhandle$connection, paste0("INSERT INTO intermRes_projItemsTmp_", intermResTableSuffix, " SELECT db_uuid, session, bundle, seqStartId, seqEndId, seqStartId AS pSeqStartId, seqEndId AS pSeqEndId, seqLen AS pSeqLen, level AS pLevel FROM intermRes_ItemsTmp_", intermResTableSuffix))
        DBI::dbGetQuery(emuDBhandle$connection, paste0("UPDATE intermRes_metaInfosTmp_", intermResTableSuffix, " SET projectionAttrLevel = '", lvlName, "'"))
      }
      return()
    }
  }
  stop("Syntax error: No operator found.")
}

query_databaseEqlCONJQ<-function(emuDBhandle, q, intermResTableSuffix){
  # BNF: CONJQ = SQ,{'&',SQ};
  qTrim=stringr::str_trim(q)
  conditions=list()
  # initialize with empty result
  startPos=1
  p=0
  resultLevel=NULL
  projection=FALSE
  useSubsets = FALSE
  # parse through all terms of and (&) operation
  while(p>=0){
    # find ampersand '&' char
    p = get_stringPosition(string=qTrim,searchStr='&',pos=startPos,literalQuote="'")
    if(p==-1){
      # get single term
      condStr=stringr::str_trim(substring(qTrim,startPos))
    }else{
      # get leading term
      condStr=stringr::str_trim(substr(qTrim,startPos,p-1))
      # advance to next
      startPos=p+1
    }
    # find projection marker (#) in condStr
    pHash = get_stringPosition(string=condStr,searchStr='#',literalQuote="'")
    if(pHash!=-1){
      if(projection){
        stop("Only one hashtag allowed in linear query term: ",qTrim)
      }else{
        projection = TRUE
      }
    }
    # execute query on term
    query_databaseEqlSQ(emuDBhandle, condStr, intermResTableSuffix, useSubsets = useSubsets)
    # set resultLevel of first term
    if(is.null(resultLevel)){
      termResLevel = DBI::dbGetQuery(emuDBhandle$connection, paste0("SELECT * FROM intermRes_metaInfosTmp_", intermResTableSuffix))$resultLevel
      if(!is.null(termResLevel)){
        resultLevel=termResLevel
      }
    }
    
    nRes = DBI::dbGetQuery(emuDBhandle$connection, paste0("SELECT COUNT(*) AS n FROM intermRes_itemsTmp_", intermResTableSuffix))$n
    if(nRes==0){
      # empty result stop here and return
      return()
    }else{
      # remove all entries from subsets
      DBI::dbGetQuery(emuDBhandle$connection, "DELETE FROM itemsFilteredSubsetTmp")
      DBI::dbGetQuery(emuDBhandle$connection, "DELETE FROM labelsFilteredSubsetTmp")
      
      # Proceed with items matching current condition by placeing them into subset tabels
      DBI::dbGetQuery(emuDBhandle$connection, paste0("INSERT INTO itemsFilteredSubsetTmp ",
                                                "SELECT DISTINCT i.* FROM itemsFilteredTmp i, intermRes_itemsTmp_", intermResTableSuffix, " imr ",
                                                "WHERE i.db_uuid=imr.db_uuid AND i.session=imr.session AND i.bundle=imr.bundle AND i.itemID=imr.seqStartId"))
      
      DBI::dbGetQuery(emuDBhandle$connection, paste0("INSERT INTO labelsFilteredSubsetTmp ",
                                                "SELECT DISTINCT l.* FROM labelsFilteredTmp l, intermRes_itemsTmp_", intermResTableSuffix, " imr ",
                                                "WHERE l.db_uuid=imr.db_uuid AND l.session=imr.session AND l.bundle=imr.bundle AND l.itemID=imr.seqStartId"))
      useSubsets = TRUE
      
    }
  }
  
  DBI::dbGetQuery(emuDBhandle$connection, paste0("UPDATE intermRes_itemsTmp_", intermResTableSuffix, " SET level ='", resultLevel, "'"))
  DBI::dbGetQuery(emuDBhandle$connection, paste0("UPDATE intermRes_metaInfosTmp_", intermResTableSuffix, " SET resultLevel = '", resultLevel, "'"))
}

##################################
query_databaseEqlInBracket<-function(emuDBhandle, q, intermResTableSuffix, leftRightTableNrCounter = 0){
  parseRes=list()
  qTrim=stringr::str_trim(q)
  # parse SEQQ or DOMQ
  seqPos = get_stringPositionOutsideBrackets(qTrim,'->',literalQuote="'",bracket=c('[',']'))
  domPos = get_stringPositionOutsideBrackets(qTrim,'^',literalQuote="'",bracket=c('[',']'))
  if(seqPos!=-1 || domPos!=-1){
    # parse DOMQ or SEQQ
    lExpRes=NULL
    prjIts=NULL
    if(domPos!=-1){
      left=stringr::str_trim(substr(qTrim,1,domPos-1))
      right=stringr::str_trim(substring(qTrim,domPos+1))
    }else if(seqPos!=-1){
      left=stringr::str_trim(substr(qTrim,1,seqPos-1))
      right=stringr::str_trim(substring(qTrim,seqPos+2))
    }
    # create left & right temp table
    leftTableSuffix = paste0("left_", leftRightTableNrCounter)
    rightTableSuffix = paste0("right_", leftRightTableNrCounter)
    leftRightTableNrCounter = leftRightTableNrCounter + 1
    
    create_intermResTmpQueryTablesDBI(emuDBhandle, suffix = leftTableSuffix)
    create_intermResTmpQueryTablesDBI(emuDBhandle, suffix = rightTableSuffix)

    query_databaseWithEql(emuDBhandle, left, intermResTableSuffix = leftTableSuffix, leftRightTableNrCounter)
    query_databaseWithEql(emuDBhandle, right, intermResTableSuffix = rightTableSuffix, leftRightTableNrCounter + 1)
    
    # check if left or right side results are empty -> clear tabels and return
    nLeftResIts = DBI::dbGetQuery(emuDBhandle$connection, paste0("SELECT COUNT(*) AS n FROM intermRes_itemsTmp_", leftTableSuffix))$n
    if(nLeftResIts == 0){
      clear_intermResTabels(emuDBhandle, leftTableSuffix)
      clear_intermResTabels(emuDBhandle, rightTableSuffix)
      return()
    }
    
    
    nRightResIts = DBI::dbGetQuery(emuDBhandle$connection, paste0("SELECT COUNT(*) AS n FROM intermRes_itemsTmp_", rightTableSuffix))$n
    if(nRightResIts == 0){
      clear_intermResTabels(emuDBhandle, leftTableSuffix)
      clear_intermResTabels(emuDBhandle, rightTableSuffix)
      return()
    }
    
    nLeftProjItems = DBI::dbGetQuery(emuDBhandle$connection, paste0("SELECT COUNT(*) AS n FROM intermRes_projItemsTmp_", leftTableSuffix))$n
    nRightProjItems = DBI::dbGetQuery(emuDBhandle$connection, paste0("SELECT COUNT(*) AS n FROM intermRes_projItemsTmp_", rightTableSuffix))$n
    
    if(nLeftProjItems != 0 & nRightProjItems != 0){
      stop("Multiple hash tags '#' not allowed in EQL2 query!")
    }
    # get items on dominance compare levels
    lResAttrName = DBI::dbGetQuery(emuDBhandle$connection, paste0("SELECT resultLevel FROM intermRes_MetaInfosTmp_", leftTableSuffix))$resultLevel
    lResLvl = get_levelNameForAttributeName(emuDBhandle, lResAttrName)
    
    rResAttrName = DBI::dbGetQuery(emuDBhandle$connection, paste0("SELECT resultLevel FROM intermRes_MetaInfosTmp_", rightTableSuffix))$resultLevel
    rResLvl = get_levelNameForAttributeName(emuDBhandle, rResAttrName)
    if(domPos!=-1 & lResLvl==rResLvl){
      stop("Dominance query on same levels impossible.\nLeft level: ",lResLvl," (attr:",lResAttrName,") equals right level: ",lResLvl," (attr:",rResAttrName,")\n")
    }
    # check equal levels for sequence query
    # (Do this already at this point, fixes issue: Sequence query should always throw an error if arguments not on same level. #39 )
    if(seqPos!=-1 & lResAttrName!=rResAttrName){
      stop("Levels of sequence query '",qTrim,"' do not match. (",lResAttrName," not equal ",rResAttrName,")")
    }
    
    
    if(domPos!=-1){
      # parse DOMQ
      # query the result level of left term
      nLinksExt = DBI::dbGetQuery(emuDBhandle$connection, paste0("SELECT COUNT(*) AS n FROM linksExtFilteredTmp"))$n
      if(nLinksExt==0){
        clear_intermResTabels(emuDBhandle, leftTableSuffix)
        clear_intermResTabels(emuDBhandle, rightTableSuffix)
        return()
      }
      # right seq items
      
      # build dominance SQL query string 
      itemsSameBundleCond1="ils.db_uuid=irs.db_uuid AND ils.session=irs.session AND ils.bundle=irs.bundle AND "
      itemsSameBundleCond2="ils.db_uuid=irs.db_uuid AND ils.session=irs.session AND ils.bundle=irs.bundle AND "
      itemsSameBundleCond3="ils.db_uuid=ire.db_uuid AND ils.session=ire.session AND ils.bundle=ire.bundle AND "
      itemsSameBundleCond4="ils.db_uuid=lid.db_uuid AND ils.session=lid.session AND ils.bundle=lid.bundle AND "
      itemsSameBundleCond5="ils.db_uuid=rid.db_uuid AND ils.session=rid.session AND ils.bundle=rid.bundle"
      linkSameBundleCond1="k.db_uuid=ils.db_uuid AND k.db_uuid=irs.db_uuid AND k.session=ils.session AND k.session=irs.session AND k.bundle=ils.bundle AND k.bundle=irs.bundle"
      linkSameBundleCond2="m.db_uuid=ils.db_uuid AND m.db_uuid=irs.db_uuid AND m.session=ils.session AND m.session=irs.session AND m.bundle=ils.bundle AND m.bundle=irs.bundle"
      lDomQuerySelectStr="lid.db_uuid,lid.session,lid.bundle,lid.seqStartId AS lSeqStartId, lid.seqEndId AS lSeqEndId, lid.seqLen AS lSeqLen, lid.level AS lLevel"
      rDomQuerySelectStr="rid.seqStartId AS rSeqStartId, rid.seqEndId AS rSeqEndId, rid.seqLen AS rSeqLen, rid.level AS rLevel"
      domQueryFromStr=paste0("intermRes_itemsTmp_", leftTableSuffix, " lid, intermRes_ItemsTmp_", rightTableSuffix," rid, itemsFilteredTmp ils, itemsFilteredTmp irs, itemsFilteredTmp ile, itemsFilteredTmp ire")
      domQueryStrCond0=paste0(itemsSameBundleCond1,itemsSameBundleCond2,itemsSameBundleCond3,"ils.itemID=lid.seqStartId AND ile.itemID=lid.seqEndId AND ",itemsSameBundleCond4,"irs.itemID=rid.seqStartId AND ire.itemID=rid.seqEndId AND ",itemsSameBundleCond5)
      # The query has now the corners of the dominance "trapeze" in ils,ile,irs,ire
      # Check sequence start item of left result on existence of a link to the start item of the right sequence 
      domQueryStrCond1=paste0("EXISTS (SELECT * FROM linksExtFilteredTmp k WHERE ",linkSameBundleCond1," AND ((k.fromID=ils.itemID AND k.toID=irs.itemID) OR (k.toID=ils.itemID AND k.fromID=irs.itemID)))")
      # ... and sequence end item of left result on existence of a link to the end item of the right sequence 
      domQueryStrCond2=paste0("EXISTS (SELECT * FROM linksExtFilteredTmp m WHERE ",linkSameBundleCond2," AND ((m.fromID=ile.itemID AND m.toID=ire.itemID) OR (m.toID=ile.itemID AND m.fromID=ire.itemID)))")
      
      # concatenate the dominance query string
      domQueryStrTail=paste0(" FROM ",domQueryFromStr," WHERE ", domQueryStrCond0, " AND ", domQueryStrCond1," AND ",domQueryStrCond2)
      lrDomQueryStr=paste0("SELECT DISTINCT ",lDomQuerySelectStr,",",rDomQuerySelectStr,domQueryStrTail)
      
      # perform query an place result into lrExpResTmp table
      DBI::dbGetQuery(emuDBhandle$connection, paste0("DELETE FROM lrExpResTmp"))
      insertQueryStr = paste0("INSERT INTO lrExpResTmp ", lrDomQueryStr)
      DBI::dbGetQuery(emuDBhandle$connection, insertQueryStr)
      
      # DBI::dbWriteTable(emuDBhandle$connection, "lrExpResTmp", lrExpRes, append = T)
      nLrExpRes = DBI::dbGetQuery(emuDBhandle$connection, "SELECT COUNT(*) AS n FROM lrExpResTmp")$n
      if(nLrExpRes>0){
        if(nLeftProjItems != 0){
          # reduce projection items to DOMQ result items and store in correct table
          qStr = paste0("SELECT i.db_uuid, i.session, i.bundle, i.lSeqStartId AS seqStartId, i.lSeqEndId AS seqEndId, pi.pSeqStartId, pi.pSeqEndId, pi.pSeqLen, pi.pLevel ",
                        "FROM lrExpResTmp i, intermRes_ProjItemsTmp_", leftTableSuffix, " pi ",
                        "WHERE i.db_uuid=pi.db_uuid AND i.session=pi.session AND i.bundle=pi.bundle AND i.lSeqStartId=pi.seqStartId AND i.lSeqEndId=pi.seqEndId")
          reducedPI = DBI::dbGetQuery(emuDBhandle$connection, qStr)
          DBI::dbGetQuery(emuDBhandle$connection, paste0("DELETE FROM intermRes_ProjItemsTmp_", intermResTableSuffix))
          DBI::dbWriteTable(emuDBhandle$connection, paste0("intermRes_ProjItemsTmp_", intermResTableSuffix), reducedPI, append = T)

          # move meta infos to correct table
          # allMeta = DBI::dbGetQuery(emuDBhandle$connection, paste0("SELECT * FROM intermRes_MetaInfosTmp_", leftTableSuffix))
          # DBI::dbGetQuery(emuDBhandle$connection, paste0("DELETE FROM intermRes_MetaInfosTmp_", intermResTableSuffix))
          # DBI::dbWriteTable(emuDBhandle$connection, paste0("intermRes_MetaInfosTmp_", intermResTableSuffix), allMeta, append = T)
          
        }
        
        if(nRightProjItems != 0){
          # reduce projection items to DOMQ result items and store in correct table
          qStr = paste0("SELECT i.db_uuid, i.session, i.bundle, i.lSeqStartId AS seqStartId, i.lSeqEndId AS seqEndId, pi.pSeqStartId, pi.pSeqEndId, pi.pSeqLen, pi.pLevel ",
                        "FROM lrExpResTmp i, intermRes_ProjItemsTmp_", rightTableSuffix, " pi ",
                        "WHERE i.db_uuid=pi.db_uuid AND i.session=pi.session AND i.bundle=pi.bundle AND i.rSeqStartId=pi.seqStartId AND i.rSeqEndId=pi.seqEndId")
          reducedPI = DBI::dbGetQuery(emuDBhandle$connection, qStr)
          DBI::dbGetQuery(emuDBhandle$connection, paste0("DELETE FROM intermRes_ProjItemsTmp_", intermResTableSuffix))
          DBI::dbWriteTable(emuDBhandle$connection, paste0("intermRes_ProjItemsTmp_", intermResTableSuffix), reducedPI, append = T)

          # move meta infos to correct table
          # allMeta = DBI::dbGetQuery(emuDBhandle$connection, paste0("SELECT * FROM intermRes_MetaInfosTmp_", rightTableSuffix))
          # DBI::dbGetQuery(emuDBhandle$connection, paste0("DELETE FROM intermRes_MetaInfosTmp_", intermResTableSuffix))
          # DBI::dbWriteTable(emuDBhandle$connection, paste0("intermRes_MetaInfosTmp_", intermResTableSuffix), allMeta, append = T)
          
        }
      }
      
      # if(nRightProjItems == 0 & nRightProjItems == 0){
        # if no projItems -> place left Meta infos in result table
        allMeta = DBI::dbGetQuery(emuDBhandle$connection, paste0("SELECT * FROM intermRes_MetaInfosTmp_", leftTableSuffix))
        DBI::dbGetQuery(emuDBhandle$connection, paste0("DELETE FROM intermRes_MetaInfosTmp_", intermResTableSuffix))
        DBI::dbWriteTable(emuDBhandle$connection, paste0("intermRes_MetaInfosTmp_", intermResTableSuffix), allMeta, append = T)
      # }
      
      # place result in correct table
      resItems = DBI::dbGetQuery(emuDBhandle$connection, "SELECT DISTINCT db_uuid, session, bundle, lSeqStartId AS seqStartId, lSeqEndId AS seqEndId, lSeqLen AS seqLen, lLevel AS level FROM lrExpResTmp")
      DBI::dbGetQuery(emuDBhandle$connection, paste0("DELETE FROM intermRes_ItemsTmp_", intermResTableSuffix))
      DBI::dbWriteTable(emuDBhandle$connection, paste0("intermRes_ItemsTmp_", intermResTableSuffix), resItems, append = T)
      
      
    }
    if(seqPos!=-1){
      # query the result level of left term (removed lid.seqEndId AS leId,rid.seqStartId AS rsId,)
      lrSeqQueryStr=paste0("SELECT lid.db_uuid, lid.session, lid.bundle, lid.seqStartId AS lSeqStartId, lid.seqEndId AS lSeqEndId, lid.seqLen AS lSeqLen, lid.level AS lLevel, rid.seqStartId AS rSeqStartId, rid.seqEndId AS rSeqEndId, rid.seqLen AS rSeqLen, lid.level AS rLevel ",
                           "FROM intermRes_itemsTmp_", leftTableSuffix, " lid, intermRes_itemsTmp_", rightTableSuffix, " rid, itemsFilteredTmp il, itemsFilteredTmp ir ",
                           "WHERE il.db_uuid=ir.db_uuid AND il.session=ir.session AND il.bundle=ir.bundle ",
                           "AND il.db_uuid=lid.db_uuid AND il.session=lid.session AND il.bundle=lid.bundle ",
                           "AND il.db_uuid=rid.db_uuid AND il.session=rid.session AND il.bundle=rid.bundle ",
                           "AND il.itemID=lid.seqEndId AND ir.itemID=rid.seqStartId AND il.level=ir.level AND ir.seqIdx=il.seqIdx+1")
      
      
      # perform query an place result into lrExpResTmp table
      DBI::dbGetQuery(emuDBhandle$connection, paste0("DELETE FROM lrExpResTmp"))
      insertQueryStr = paste0("INSERT INTO lrExpResTmp ", lrSeqQueryStr)
      DBI::dbGetQuery(emuDBhandle$connection, insertQueryStr)

      # check if no sequences where found -> clear & return
      nSeq = DBI::dbGetQuery(emuDBhandle$connection, paste0("SELECT COUNT(*) AS n FROM lrExpResTmp"))$n
      if(nSeq == 0){
        clear_intermResTabels(emuDBhandle, leftTableSuffix)
        clear_intermResTabels(emuDBhandle, rightTableSuffix)
        return()
      }
      
      
      if(nLeftProjItems != 0){
        # reduce to projection items
        # check if SEQQ result items and store in correct table
        qStr=paste0("SELECT i.db_uuid, i.session, i.bundle, i.lSeqStartId, i.rSeqEndId, pi.pSeqStartId, pi.pSeqEndId, pi.pSeqLen, pi.pLevel ",
                    "FROM lrExpResTmp i, intermRes_ProjItemsTmp_", leftTableSuffix, " pi ",
                    "WHERE i.db_uuid=pi.db_uuid AND i.session=pi.session AND i.bundle=pi.bundle AND i.lSeqStartId=pi.seqStartId AND i.lSeqEndId=pi.seqEndId")
        
        reducedPI = DBI::dbGetQuery(emuDBhandle$connection, qStr)
        DBI::dbGetQuery(emuDBhandle$connection, paste0("DELETE FROM intermRes_ProjItemsTmp_", intermResTableSuffix))
        DBI::dbWriteTable(emuDBhandle$connection, paste0("intermRes_ProjItemsTmp_", intermResTableSuffix), reducedPI, append = T)
        # move meta infos to correct table
        allMeta = DBI::dbGetQuery(emuDBhandle$connection, paste0("SELECT * FROM intermRes_MetaInfosTmp_", leftTableSuffix))
        DBI::dbGetQuery(emuDBhandle$connection, paste0("DELETE FROM intermRes_MetaInfosTmp_", intermResTableSuffix))
        DBI::dbWriteTable(emuDBhandle$connection, paste0("intermRes_MetaInfosTmp_", intermResTableSuffix), allMeta, append = T)
        
      }
      
      if(nRightProjItems != 0){
        # reduce to projection items
        qStr=paste0("SELECT i.db_uuid, i.session, i.bundle, i.lSeqStartId, i.rSeqEndId, pi.pSeqStartId, pi.pSeqEndId, pi.pSeqLen, pi.pLevel ",
                    "FROM lrExpResTmp i,intermRes_ProjItemsTmp_", rightTableSuffix, " pi ",
                    "WHERE i.db_uuid=pi.db_uuid AND i.session=pi.session AND i.bundle=pi.bundle AND i.rSeqStartId=pi.seqStartId AND i.rSeqEndId=pi.seqEndId")
        
        reducedPI = DBI::dbGetQuery(emuDBhandle$connection, qStr)
        DBI::dbGetQuery(emuDBhandle$connection, paste0("DELETE FROM intermRes_ProjItemsTmp_", intermResTableSuffix))
        DBI::dbWriteTable(emuDBhandle$connection, paste0("intermRes_ProjItemsTmp_", intermResTableSuffix), reducedPI, append = T)
        # move meta infos to correct table
        allMeta = DBI::dbGetQuery(emuDBhandle$connection, paste0("SELECT * FROM intermRes_MetaInfosTmp_", rightTableSuffix))
        DBI::dbGetQuery(emuDBhandle$connection, paste0("DELETE FROM intermRes_MetaInfosTmp_", intermResTableSuffix))
        DBI::dbWriteTable(emuDBhandle$connection, paste0("intermRes_MetaInfosTmp_", intermResTableSuffix), allMeta, append = T)
        
      }
      
      if(nLeftProjItems == 0 & nRightProjItems == 0){
        # if no projItems -> place left Meta infos in result table
        allMeta = DBI::dbGetQuery(emuDBhandle$connection, paste0("SELECT * FROM intermRes_MetaInfosTmp_", leftTableSuffix))
        DBI::dbGetQuery(emuDBhandle$connection, paste0("DELETE FROM intermRes_MetaInfosTmp_", intermResTableSuffix))
        DBI::dbWriteTable(emuDBhandle$connection, paste0("intermRes_MetaInfosTmp_", intermResTableSuffix), allMeta, append = T)
      }
      
      # place result in correct table
      resItems = DBI::dbGetQuery(emuDBhandle$connection, "SELECT DISTINCT db_uuid, session, bundle, lSeqStartId AS seqStartId, rSeqEndId AS seqEndId, lSeqLen+rSeqLen AS seqLen, lLevel AS level FROM lrExpResTmp")
      DBI::dbGetQuery(emuDBhandle$connection, paste0("DELETE FROM intermRes_itemsTmp_", intermResTableSuffix))
      DBI::dbWriteTable(emuDBhandle$connection, paste0("intermRes_ItemsTmp_", intermResTableSuffix), resItems, append = T)
      
    }
    return()
  }else{
    query_databaseWithEql(emuDBhandle, qTrim, intermResTableSuffix, leftRightTableNrCounter)
  }
}

## Query EMU database
## 
## @param database object of class emuDB
## @param query EQL query string
## @return EMU seglist
## @export
## @keywords emuDB database query Emu EQL 
## 
query_databaseWithEqlEmusegs<-function(emuDBhandle, query){

  # create "root" intermediate result tables
  create_intermResTmpQueryTablesDBI(emuDBhandle, suffix = "root")
  # query emuDB
  query_databaseWithEql(emuDBhandle, query, intermResTableSuffix = "root", leftRightTableNrCounter = 0)
  # escape singel quotes
  query = gsub("'", "''", query)
  DBI::dbGetQuery(emuDBhandle$connection, paste0("UPDATE intermRes_MetaInfosTmp_root SET queryStr = '", query, "'"))
  emusegs=convert_queryResultToEmusegs(emuDBhandle)
  return(emusegs)
  
}

####################
query_databaseWithEqlEmuRsegs<-function(emuDBhandle, query, timeRefSegmentLevel){
  
  # create "root" intermediate result tables
  create_intermResTmpQueryTablesDBI(emuDBhandle, suffix = "root")
  # query emuDB
  query_databaseWithEql(emuDBhandle, query, intermResTableSuffix = "root", leftRightTableNrCounter = 0)
  # escape singel quotes
  query = gsub("'", "''", query)
  DBI::dbGetQuery(emuDBhandle$connection, paste0("UPDATE intermRes_metaInfosTmp_root SET queryStr = '", query, "'"))
  emuRsegs = convert_queryResultToEmuRsegs(emuDBhandle, timeRefSegmentLevel)
  return(emuRsegs)
  
}




## Query EMU database
## 
## @param database object of class emuDB
## @param query EQL query string
## @return result set object
## @export
## @keywords emuDB database query Emu EQL 
## 
query_databaseWithEql<-function(emuDBhandle, query, intermResTableSuffix, leftRightTableNrCounter){
  parseRes=list()
  qTrim=stringr::str_trim(query)
  brOpenPos=get_charPosition(qTrim,'[',literalQuote="'")
  if(brOpenPos==-1){
    query_databaseEqlCONJQ(emuDBhandle, qTrim, intermResTableSuffix = intermResTableSuffix)
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
    query_databaseEqlInBracket(emuDBhandle, inBr, intermResTableSuffix, leftRightTableNrCounter)
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
##' @export
##' @seealso \code{\link{load_emuDB}}
##' @keywords emuDB database query Emu EQL 
##' @examples
##' \dontrun{
##' 
##' ##################################
##' # prerequisite: loaded ae emuDB 
##' # (see ?load_emuDB for more information)
##' 
##' ## Query database ae with EQL query "[Phonetic=t -> Phonetic=s]":
##' ## 'Find all sequences /ts/ in level Phonetics'.
##' ## and store result seglist in variable segListTs
##' 
##' seglistTs=query(ae, "[Phonetic == t -> Phonetic == s]")
##' 
##' ## Query database ae with EQL query "[Syllable == S ^ Phoneme == t]":
##' ## 'Find all items 't' in level Phoneme that are dominated by items 'S' in level Syllable.'
##' ## Return legacy Emu result type 'emusegs'
##' 
##' query(ae, "[Syllable == S ^ Phoneme == t]", resultType="emusegs")
##' 
##' ## Query 'p' items of level Phoneme from bundles whose bundle names start with 'msajc07' 
##' ## and whose session names start with '00'
##' ## (Note that here the query uses the operator '=' (meaning '==') which is kept for 
##' ##  backwards compatibilty to EQL1.)  
##' 
##' query(ae, "Phoneme = p", bundlePattern = "msajc05.*", sessionPattern = "00.*")
##' 
##' }
##' 

query <- function(emuDBhandle, query, sessionPattern = '.*', bundlePattern = '.*', queryLang = 'EQL2', timeRefSegmentLevel = NULL, resultType = NULL){
  
  if(queryLang=='EQL2'){
    # create temp tables 
    drop_allTmpTablesDBI(emuDBhandle)
    create_tmpFilteredQueryTablesDBI(emuDBhandle)
    
    # extract all items for session/bundlePattern regEx matching (should check if REGEXP is available and is so use that instead)
    queryItems <- DBI::dbGetQuery(emuDBhandle$connection, paste0("SELECT * FROM items WHERE db_uuid='", emuDBhandle$UUID, "'"))
    queryLabels <- DBI::dbGetQuery(emuDBhandle$connection, paste0("SELECT * FROM labels WHERE db_uuid='", emuDBhandle$UUID, "'"))
    queryLinks <- DBI::dbGetQuery(emuDBhandle$connection, paste0("SELECT * FROM links WHERE db_uuid='", emuDBhandle$UUID,"'"))
    queryLinksExt <- DBI::dbGetQuery(emuDBhandle$connection, paste0("SELECT * FROM linksExt WHERE db_uuid='", emuDBhandle$UUID,"'"))
    
    # if set get logical vectors that match sessionPattern and bundlePattern
    if(!is.null(sessionPattern) && sessionPattern!='.*'){
      sesSelIts = emuR_regexprl(sessionPattern, queryItems$session)
      sesSelLbls = emuR_regexprl(sessionPattern, queryLabels$session)
      sesSelLks = emuR_regexprl(sessionPattern, queryLinks$session)
      sesSelLksExt = emuR_regexprl(sessionPattern, queryLinksExt$session)
    }else{
      sesSelIts = rep(TRUE, nrow(queryItems))
      sesSelLbls = rep(TRUE, nrow(queryLabels))
      sesSelLks = rep(TRUE, nrow(queryLinks))
      sesSelLksExt = rep(TRUE, nrow(queryLinksExt))
    }
    if(!is.null(bundlePattern) && bundlePattern!='.*'){
      bndlSelIts = emuR_regexprl(bundlePattern, queryItems$bundle)
      bndlSelLbls = emuR_regexprl(bundlePattern, queryLabels$bundle)
      bndlSelLks = emuR_regexprl(bundlePattern, queryLinks$bundle)
      bndlSelLksExt = emuR_regexprl(bundlePattern, queryLinksExt$bundle)
    }else{
      bndlSelIts = rep(TRUE, nrow(queryItems))
      bndlSelLbls = rep(TRUE, nrow(queryLabels))
      bndlSelLks = rep(TRUE, nrow(queryLinks))
      bndlSelLksExt = rep(TRUE, nrow(queryLinksExt))
    }
    # write to tmp tables
    DBI::dbWriteTable(emuDBhandle$connection, "itemsFilteredTmp", queryItems[sesSelIts & bndlSelIts, ], append = TRUE)
    DBI::dbWriteTable(emuDBhandle$connection, "labelsFilteredTmp", queryLabels[sesSelLbls & bndlSelLbls, ], append = TRUE)
    DBI::dbWriteTable(emuDBhandle$connection, "linksFilteredTmp", queryLinks[sesSelLks & bndlSelLks, ], append = TRUE)
    DBI::dbWriteTable(emuDBhandle$connection, "linksExtFilteredTmp", queryLinksExt[sesSelLksExt & bndlSelLksExt, ], append = TRUE)
    
    if(is.null(resultType)){
      emuRsegs = query_databaseWithEqlEmuRsegs(emuDBhandle,query,timeRefSegmentLevel)
      drop_allTmpTablesDBI(emuDBhandle)
      return(emuRsegs)
    }else{
      if(resultType=='emuRsegs'){
        emuRsegs = query_databaseWithEqlEmuRsegs(emuDBhandle,query,timeRefSegmentLevel)
        drop_allTmpTablesDBI(emuDBhandle)
        return(emuRsegs)
      }else if(resultType=='emusegs'){
        if(!is.null(timeRefSegmentLevel)){
          # TODO 
          stop("Parameter timeRefSegmentLevel not yet supported for resultType 'emusegs'. Please use resultType 'emuRsegs' (default).")
        }
        return(query_databaseWithEqlEmusegs(emuDBhandle,query))
      }else{
        stop("Unknown result type: '",resultType,"'. Supported result types: 'emuRsegs', emusegs'")
      }
    }
    
  }else{
    stop("Unknown query language '",queryLang,"'.")
  }
}
