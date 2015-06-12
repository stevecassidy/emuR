require(sqldf)
require(stringr)

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

.create.condition.text.alternatives<-function(opr,values){
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


create.subtree<-function(items,links,resultLevel=NULL,projectionItems=NULL){
  o=list(items=items,links=links,resultLevel=resultLevel,projectionItems=projectionItems)
  class(o)<-'emuDB.query.result'
  return(o)
}

emuR.regexprl<-function(pattern,x){
  m=regexpr(pattern,x)
  return((m==1) & (attr(m,'match.length')==nchar(x)))
}

write_table_forced<-function(name,value){
if(dbExistsTable(getEmuDBcon(),name)){
  dbRemoveTable(getEmuDBcon(),name)
}
dbWriteTable(getEmuDBcon(),name = name,value=value)
}

query.labels<-function(ldf,conditionText){
  opr=conditionText[['opr']]
  values=conditionText[['values']]
  res=NULL
  if(opr=='==' | opr=='='){
    for(value in values){
      if(is.null(res)){
        #res=subset(ldf,label==value)
        ssl=ldf[['label']]==value
        res=ldf[ssl,]
      }else{
        #r=subset(ldf,label==value)
        ssl=ldf[['label']]==value
        r=ldf[ssl,]
        res=rbind(res,r)
      }
    }
  }else if(opr=='!='){   
    for(value in values){
      if(is.null(res)){
        #res=subset(ldf,label!=value)
        ssl=ldf[['label']]!=value
        res=ldf[ssl,]
      }else{
        #res=subset(res,label!=value)
        ssl=res[['label']]!=value
        res=res[ssl,]
      }
    }
  }else if(opr=='=~'){
    for(value in values){
      if(is.null(res)){
        #res=subset(ldf,grepl(value,label))
        ssl=emuR.regexprl(value,ldf[['label']])
        res=ldf[ssl,]
      }else{
        #r=subset(ldf,grepl(value,label))
        ssl=emuR.regexprl(value,ldf[['label']])
        r=ldf[ssl,]
        res=rbind(res,r)
      }
    }
  }else if(opr=='!~'){
    for(value in values){
      if(is.null(res)){
        #res=subset(ldf,!grepl(value,label))
        ssl=!emuR.regexprl(value,ldf[['label']])
        res=ldf[ssl,]
      }else{
        #res=subset(res,!grepl(value,label))
        ssl=!emuR.regexprl(value,res[['label']])
        res=res[ssl,]
      }
    }
  }else{
    stop("Syntax error: Unknown operator: '",opr,"'\n")
  }
  return(res)
}

query.database.level.label<-function(ldf,levelName,conditionText){
  #levelSubset=subset(ldf,name==levelName)
  subsetSelector=ldf[['name']]==levelName
  levelSubset=ldf[subsetSelector,]
  itemsMatch=query.labels(levelSubset,conditionText)
  itemsAsSeqs=NULL
  if(nrow(itemsMatch)>0){
    itemsIdxSql='CREATE INDEX itemsMatch_idx ON itemsMatch(itemID)'
   
    itemsAsSeqQStr=paste0("SELECT db_uuid,session,bundle,itemID AS seqStartId, itemID AS seqEndId,1 AS seqLen,'",levelName,"' AS level FROM itemsMatch")
    itemsAsSeqs=sqldf(c(itemsIdxSql,itemsAsSeqQStr))
  }else{
    itemsAsSeqs=EMPTY_RESULT_DF
  }
  # add sequence index
  #qStr=paste0("SELECT i.*,",rsIdx," AS rsIdx FROM itemsMatch i")
  #itemsMatch=sqldf(qStr)
  res=create.subtree(items=itemsAsSeqs,links=NULL,resultLevel=levelName)
  return(res)
}



query.database.eql.FUNKA<-function(dbConfig,q,items=NULL){
  # BNF: FUNKA = POSA | NUMA;
  qTrim=str_trim(q)
  if(is.null(items)){
    items=getQueryTmpEmuDBs()[['queryItems']]
  }
  allItems=getQueryTmpEmuDBs()[['queryItems']]
  
  # determine function name
  # TODO duplicate code
  prbOpen=get.string.position(string=qTrim,searchStr='(',literalQuote="'")
  if(prbOpen!=-1){
    prbClose=get.string.position(string=qTrim,searchStr=')',literalQuote="'")
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
          expEqualSign=substr(funcValueTerm,1,1)
          if(expEqualSign!='='){
            stop("Syntax error: Expected equal sign '=' for in function term: '",qTrim,"'\n")
          }
          funcValue=str_trim(substring(funcValueTerm,2,))
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
          stop("Syntax error: Expected function value 0 or 1 after '=' in function term: '",qTrim,"'\n")
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
          stop("Syntax error: Expected function value 0 or 1 after '=' in function term: '",qTrim,"'\n")
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
          stop("Syntax error: Expected function value 0 or 1 after '=' in function term: '",qTrim,"'\n")
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
          p=get.string.position(string=funcValueTerm,searchStr=opr)
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
          sqlRuncOpr='='
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
        sqlQStr=paste0("SELECT DISTINCT d.db_uuid,d.session,d.bundle,d.itemID AS seqStartId, d.itemID AS seqEndId,1 AS seqLen,'",param1,"' AS level \
                       FROM allItems i,items d \
                      WHERE i.db_uuid=d.db_uuid AND i.session=d.session AND i.bundle=d.bundle \
                       AND i.level='",level2,"' AND d.level='",level1,"' AND EXISTS \
                       (SELECT * FROM links k \
                       WHERE d.db_uuid=k.db_uuid AND d.session=k.session AND d.bundle=k.bundle \
                        AND i.db_uuid=k.db_uuid AND i.session=k.session AND i.bundle=k.bundle \
                        AND k.fromID=d.itemID AND k.toID=i.itemID AND k.toLevel=i.level AND k.toSeqLen",sqlFuncOpr,funcVal,"\
                       )") 
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

query.database.eql.EA<-function(dbConfig,q,items=NULL,labels=NULL){
  # BNF: EA = ETIKETTA | FUNKA;
  qTrim=str_trim(q)
  res=NULL
  # detect function calls by existence of round brackets
  prbOpen=get.string.position(string=qTrim,searchStr='(',literalQuote="'")
  if(prbOpen!=-1){
    prbClose=get.string.position(string=qTrim,searchStr=')',literalQuote="'")
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
    res=query.database.eql.ETTIKETTA(dbConfig,qTrim,labels)
  }
  
  return(res)
}

query.database.eql.ETTIKETTA<-function(dbConfig,q,labels=NULL){
  # BNF: ETIKETTA = ['#'],EBENE,('=' | '!='),ETIKETTALTERNATIVEN;
  
  qTrim=str_trim(q)
  for(opr in c('==','!=','=~','!~','=')){
    p=get.string.position(string=q,searchStr=opr,literalQuote="'")
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
      # TODO check if level exists
      #cat("Level: '",lvlName,"'\n",sep='')
      aNms=get.all.attribute.names(dbConfig)
      if(! (lvlName %in% aNms)){
        
        stop("Unknown level attribute name: '",lvlName,"'. Database attribute names are: ",paste(aNms,collapse=','),"\n")
      }
      labelStr=substring(q,p+oprLen)
      labelTrim=str_trim(labelStr)  
      
      
      # check label for key chars
      # TODO Labels should to be allowed to contain key chars if they are single quoted 
      deniedStrs=c('^','->','==','!=','=')
      for(deniedStr in deniedStrs){
        pt=get.string.position(string=labelTrim,searchStr=deniedStr,literalQuote="'")
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
        lsp=get.string.position(string=labelTrim,pos=lp,searchStr='|',literalQuote="'")
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
          # check for labelGroup
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
                  }
                }
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
      cond=.create.condition.text.alternatives(opr,labelAltsUq)
      #}
      if(is.null(labels)){
        labels=getQueryTmpEmuDBs()[['queryLabels']]
      }
      res=query.database.level.label(ldf=labels,levelName=lvlName,cond)
      res[['projectionItems']]=NULL
      if(projectionLevel){
        rIts=res[['items']]
        # projection of the result items themself
        lvlNms=rep(lvlName,nrow(rIts))
        res[['projectionItems']]=data.frame(db_uuid=rIts[,'db_uuid'],session=rIts[,'session'],bundle=rIts[,'bundle'],seqStartId=rIts[,'seqStartId'],seqEndId=rIts[,'seqEndId'],seqLen=rIts[,'seqLen'],pSeqStartId=rIts[,'seqStartId'],pSeqEndId=rIts[,'seqEndId'],pSeqLen=rIts[,'seqLen'],pLevel=lvlNms,stringsAsFactors = FALSE)
        res[['projectionAttrLevel']]=lvlName
      }
      return(res)
    }
  }
  stop("Syntax error: No operator found.")
}

query.database.eql.KONJA<-function(dbConfig,q){
   # BNF: KONJA = EA,{'&',EA};
    qTrim=str_trim(q)
    conditions=list()
    # initialize with empty result
    res=create.subtree(items=EMPTY_RESULT_DF,links=NULL,resultLevel=NULL,projectionItems=NULL)
    startPos=1
    p=0
    items=getQueryTmpEmuDBs()[['queryItems']]
    labels=getQueryTmpEmuDBs()[['queryLabels']]
    resultLevel=NULL
    projection=FALSE
    
    # parse through all terms of and (&) operation
    while(p>=0){
      # find ampersand '&' char
      p=get.string.position(string=qTrim,searchStr='&',pos=startPos,literalQuote="'")
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
      res=query.database.eql.EA(dbConfig,condStr,items=items,labels=labels)
      # set resultLevel of first term
      if(is.null(resultLevel)){
        termResLevel=res[['resultLevel']]
        if(!is.null(termResLevel)){
          resultLevel=termResLevel
        }
      }
      
      seqIts=res[['items']]
      nRes=nrow(seqIts)
      if(nRes==0){
        # empty result stop here and return
        return(res)
      }else{
        # Proceed with items matching current condition
        items=sqldf("SELECT i.* FROM items i,seqIts s WHERE i.db_uuid=s.db_uuid AND i.session=s.session AND i.bundle=s.bundle AND i.itemID=s.seqStartId")
        labels=sqldf("SELECT l.* FROM labels l,seqIts s WHERE l.db_uuid=s.db_uuid AND l.session=s.session AND l.bundle=s.bundle AND l.itemID=s.seqStartId")
        if(!is.null(res[['projectionItems']])){
          # only one term can be marked with hashtag
          if(projection){
            stop("Only one hashtag allowed in linear query term: ",qTrim)
          }
          # if one of the boolean terms is marked with the hashtag the whole term is marked 
          projection=TRUE
          projectionAttrLevel=res[['projectionAttrLevel']]
        }
      }
    }
    res[['items']][,'level']=resultLevel
    items=res[['items']]
    if(projection){      
      qStr=paste0('SELECT i.db_uuid,i.session,i.bundle,i.seqStartId,i.seqEndId,i.seqStartId AS pSeqStartId ,i.seqEndId AS pSeqEndId,i.seqLen AS pSeqLen,"',projectionAttrLevel,'" AS pLevel FROM items i')
      res[['projectionItems']]=sqldf(qStr)
    }
    res[['resultLevel']]=resultLevel
    return(res)
}

query.database.eql.in.bracket<-function(dbConfig,q){
  parseRes=list()
  qTrim=str_trim(q)
  # parse SEQA or DOMA
  seqPos=get.string.position.outside.brackets(qTrim,'->',literalQuote="'",bracket=c('[',']'))
  domPos=get.string.position.outside.brackets(qTrim,'^',literalQuote="'",bracket=c('[',']'))
  if(seqPos!=-1 || domPos!=-1){
    items=getQueryTmpEmuDBs()[['queryItems']]
    links=getQueryTmpEmuDBs()[['queryLinksExt']]
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
    lRes=query.database.with.eql(dbConfig,left)
    rRes=query.database.with.eql(dbConfig,right)
    
    lResPIts=lRes[['projectionItems']]
    rResPIts=rRes[['projectionItems']]
    
    lIsProj=!is.null(lResPIts)
    rIsProj=!is.null(rResPIts)
    #cat("left: ",lIsProj," right: ",rIsProj,"\n")
    if(lIsProj & rIsProj){
      stop("Multiple hash tags '#' not allowed in EQL2 query!")
    }
    
    # get items on dominance compare levels
    lResIts=lRes[['items']]
    lResAttrName=lRes[['resultLevel']]
    lResLvl=get.level.name.by.attribute.name(dbConfig,lResAttrName)
   
    
    rResAttrName=rRes[['resultLevel']]
    rResLvl=get.level.name.by.attribute.name(dbConfig,rResAttrName)
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
    lResItsNrows=nrow(lResIts)
    if(lResItsNrows==0){
      res=create.subtree(items=lResIts,links=NULL,resultLevel=lResAttrName,projectionItems=lResPIts)
      return(res)
    }
    #else{
    #  lqStr=paste0("SELECT i.* FROM lResIts ls,items i WHERE i.id=ls.seqStartId AND level='",lResAttrName,"'")
    #  #lqStr=paste0("SELECT i.* FROM lResIts ls,items i WHERE i.id=ls.seqStartId")
    #  lLvlItems=sqldf(lqStr)
    #}
    rResIts=rRes[['items']]
    
   
   
    rLvlItems=NULL
    # sqldf cannot handle empty data frames 
    rResItsNrows=nrow(rResIts)
    if(rResItsNrows==0){
      res=create.subtree(items=rResIts,links=NULL,resultLevel=lResAttrName,projectionItems=rResPIts)
      return(res)
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
      # parse SEQA
      # query the result level of left term
      lrSeqQueryStr=paste0("SELECT lid.db_uuid,lid.session,lid.bundle,lid.seqStartId,lid.seqEndId AS leId,rid.seqStartId AS rsId,rid.seqEndId,lid.seqLen+rid.seqLen AS seqLen,lid.level FROM lResIts lid, rResIts rid,items il, items ir WHERE \
                          il.db_uuid=ir.db_uuid AND il.session=ir.session AND il.bundle=ir.bundle AND \
                           il.db_uuid=lid.db_uuid AND il.session=lid.session AND il.bundle=lid.bundle AND \
                          il.db_uuid=rid.db_uuid AND il.session=rid.session AND il.bundle=rid.bundle AND \
                           il.itemID=lid.seqEndId AND ir.itemID=rid.seqStartId AND il.level=ir.level AND ir.seqIdx=il.seqIdx+1")
      lrExpRes=sqldf(lrSeqQueryStr)
      
      # select final result columns
      lExpRes=data.frame(db_uuid=lrExpRes[['db_uuid']],session=lrExpRes[['session']],bundle=lrExpRes[['bundle']],seqStartId=lrExpRes[,'seqStartId'],seqEndId=lrExpRes[,'seqEndId'],seqLen=lrExpRes[,'seqLen'],level=lrExpRes[,'level'],stringsAsFactors = FALSE)
      
      lPrjIts=NULL
      if(!is.null(lResPIts)){
        # TODO
        #lPrjIts=reduce.projection.items(lExpRes,lResPIts)
        qStr="SELECT i.db_uuid,i.session,i.bundle,i.seqStartId,i.seqEndId,pi.pSeqStartId,pi.pSeqEndId,pi.pSeqLen,pi.pLevel FROM lrExpRes i,lResPIts pi WHERE \
        i.db_uuid=pi.db_uuid AND i.session=pi.session AND i.bundle=pi.bundle AND i.seqStartId=pi.seqStartId AND i.leId=pi.seqEndId"
        #rQStr="SELECT rpi.* FROM rightProjectionItems rpi WHERE EXISTS (SELECT i.seqStartId FROM resultItems i WHERE i.seqStartId=rpi.seqStartId && i.seqEndId=rpi.seqEndId)"
        
        #qStr=paste0(lQStr," UNION ",rQStr)
        lPrjIts=sqldf(qStr)
      }
      rPrjIts=NULL
      if(!is.null(rResPIts)){
        
        qStr="SELECT i.db_uuid,i.session,i.bundle,i.seqStartId,i.seqEndId,pi.pSeqStartId,pi.pSeqEndId,pi.pSeqLen,pi.pLevel FROM lrExpRes i,rResPIts pi WHERE \
        i.db_uuid=pi.db_uuid AND i.session=pi.session AND i.bundle=pi.bundle AND i.rsId=pi.seqStartId AND i.seqEndId=pi.seqEndId"
        rPrjIts=sqldf(qStr)
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
    
    # links of result tree ? No.
    
    res=create.subtree(items=lExpRes,links=NULL,resultLevel=lResAttrName,projectionItems=prjIts)
    return(res)
  }else{
    query.database.with.eql(dbConfig,qTrim)
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

  rs=query.database.with.eql(dbConfig,query)
  segList=convert.query.result.to.seglist(dbConfig,rs)
  return(segList)
  
}

query.database.with.eql.segmentlist<-function(dbConfig,query,timeRefSegmentLevel){
  
  rs=query.database.with.eql(dbConfig,query)
  segList=convert.query.result.to.segmentlist(dbConfig,rs,timeRefSegmentLevel)
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
query.database.with.eql<-function(dbConfig,query){
  parseRes=list()
  qTrim=str_trim(query)
  
  brOpenPos=get.char.position(qTrim,'[',literalQuote="'")
  if(brOpenPos==-1){
    res=query.database.eql.KONJA(dbConfig,qTrim)
    res[['queryStr']]=query
    return(res)
  }else{
    
    brClosePos=get.last.char.position(qTrim,']',literalQuote="'")
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
    inBrRes=query.database.eql.in.bracket(dbConfig,inBr)
    inBrRes[['queryStr']]=query
    return(inBrRes)
    
  }
  stop("Unknown syntax error.")
}

##' Query EMU database
##' @description Query an EMU database
##' @details Evaluates EQL2 query string on emuDB and returns a segment list of desired type.  
##' For details of the query language please refer to EQL vignette (type: \code{vignette('EQL')} ). 
##' Time information of symbolic elements (type 'ITEM') are derived from linked segment levels if available. If not available time and sample values may be set to \code{\link{NA}}.
##' @param dbName name of EMU database
##' @param query EQL2 query string
##' @param sessionPattern A regular expression pattern matching session names to be searched from the database
##' @param bundlePattern A regular expression pattern matching bundle names to be searched from the database
##' @param queryLang query language
##' @param timeRefSegmentLevel set time derivation segment level
##' @param resultType type (class name) of result
##' @param dbUUID optional UUID of emuDB
##' @return result set object of class resultType (default: \link{emuRsegs}, compatible to legacy type \link{emusegs})
##' @author Klaus Jaensch
##' @import sqldf stringr
##' @export
##' @seealso \code{\link{load_emuDB}}
##' @keywords emuDB database query Emu EQL 
##' @examples
##' \dontrun{
##' ## Query database 'ae' using EQL query [Phonetic=t -> Phonetic=s] 
##' ## and store result seglist in variable segListTs
##' 
##' seglistTs=query('ae','[Phonetic=t -> Phonetic=s]')
##' 
##' ## Query seglist from database ae using EQL query [Syllable=S ^ Phoneme=t]
##' ## Request legacy result type 'emusegs'
##' 
##' query('ae','[Syllable=S ^ Phoneme=t]',resultType='emusegs')
##' 
##' 
##' ## Query 'p' phonemes from bundles whose names start with 'msajc07' 
##' 
##' query('ae','Phoneme=p',bundlePattern='msajc05.*')
##' 
##' 
##' }
##' 

query<-function(dbName=NULL,query,sessionPattern=NULL,bundlePattern=NULL,queryLang='EQL2',timeRefSegmentLevel=NULL,resultType=NULL,dbUUID=NULL){
  
    if(queryLang=='EQL2'){
      .initialize.DBI.database(createTables=FALSE)
      dbUUID=get_emuDB_UUID(dbName,dbUUID)
      db=.load.emuDB.DBI(uuid = dbUUID)
      dbConfig=db[['DBconfig']]
      # create 
      emuDBs.query.tmp<-list()
      emuDBs.query.tmp[['queryItems']]<-dbGetQuery(getEmuDBcon(),paste0("SELECT * FROM items WHERE db_uuid='",dbUUID,"'"))
      emuDBs.query.tmp[['queryLabels']]<-dbGetQuery(getEmuDBcon(),paste0("SELECT * FROM labels WHERE db_uuid='",dbUUID,"'"))
      emuDBs.query.tmp[['queryLinksExt']]<-dbGetQuery(getEmuDBcon(),paste0("SELECT * FROM linksExt WHERE db_uuid='",dbUUID,"'"))
      setQueryTmpEmuDBs(emuDBs.query.tmp)
      if(!is.null(sessionPattern)){
        newTmpDBs=list()
       
        sessSelIts=emuR.regexprl(sessionPattern,getQueryTmpEmuDBs()[['queryItems']][['session']])
        newTmpDBs[['queryItems']]<-getQueryTmpEmuDBs()[['queryItems']][sessSelIts,]
        
        sessSelLks=emuR.regexprl(sessionPattern,getQueryTmpEmuDBs()[['queryLinksExt']][['session']])
        newTmpDBs[['queryLinksExt']]<-getQueryTmpEmuDBs()[['queryLinksExt']][sessSelLks,]
        
        sessSelLbls=emuR.regexprl(sessionPattern,getQueryTmpEmuDBs()[['queryLabels']][['session']])
        newTmpDBs[['queryLabels']]<-getQueryTmpEmuDBs()[['queryLabels']][sessSelLbls,]
        setQueryTmpEmuDBs(newTmpDBs)
      }
      if(!is.null(bundlePattern)){
        
        
        newTmpDBs=getQueryTmpEmuDBs()
        if(is.null(newTmpDBs)){
          newTmpDBs=list()
        }
        bndlSelIts=emuR.regexprl(bundlePattern,getQueryTmpEmuDBs()[['queryItems']][['bundle']])
        newTmpDBs[['queryItems']]<-getQueryTmpEmuDBs()[['queryItems']][bndlSelIts,]
        
        bndlSelLks=emuR.regexprl(bundlePattern,getQueryTmpEmuDBs()[['queryLinksExt']][['bundle']])
        newTmpDBs[['queryLinksExt']]<-getQueryTmpEmuDBs()[['queryLinksExt']][bndlSelLks,]
        
        bndlSelLbls=emuR.regexprl(bundlePattern,getQueryTmpEmuDBs()[['queryLabels']][['bundle']])
        newTmpDBs[['queryLabels']]<-getQueryTmpEmuDBs()[['queryLabels']][bndlSelLbls,]
        setQueryTmpEmuDBs(newTmpDBs)
      }
      
      if(is.null(resultType)){
        return(query.database.with.eql.segmentlist(dbConfig,query,timeRefSegmentLevel))
        #return(query.database.with.eql(dbConfig,query))
      }else{
        if(resultType=='emusegs'){
          if(!is.null(timeRefSegmentLevel)){
            # TODO 
              stop("Parameter timeRefSegmentLevel not yet supported for resultType 'emusegs'. Please use resultType 'emuRsegs' (default).")
          }
          return(query.database.with.eql.seglist(dbConfig,query))
        }else{
          stop("Unknown result type: '",resultType,"'. Supported result types: 'emusegs', NULL")
        }
      }
      # free temp tables
      setQueryTmpEmuDBs(NULL)
      
    }else{
      stop("Unknown query language '",queryLang,"'.")
    }
}


print.emuDB.query.result<-function(queryResult){
  cat("Item sequences:\n")
  print(queryResult[['items']])
  cat("\nResult level: ",queryResult[['resultLevel']],"\n")
  cat("Query was: ",queryResult[['queryStr']],"\n")
  
}

##' Query emuR result context
##' @description Requery an EMU database
##' @param seglist segment list
##' @param offset offset in sequence
##' @param offsetRef referenec of offset: 'START' start of segments, 'END': end of segments
##' @param seqLength item length of new segment list
##' @param resultType type (class name) of result
##' @param dbUUID optional UUID odf emuDB
##' @return result set object of class resultType (default: 'emuRsegs')
##' @author Klaus Jaensch
##' @import sqldf stringr
##' @seealso \code{\link{load_emuDB}}
##' @keywords emuDB database requery Emu EQL2 
##' @examples
##' \dontrun{
##' 
##' ## Requery previous element of 'p' on phonetic level
##' sl1=query('ae','Phonetic=p')
##' contextRequery(sl1,offset=-1)
##' 
##' ## Requery context (previuos and following) of 'p' on phonetic level
##'
##' contextRequery(sl1,offset=-1,seqLength=3)
##' 
##' 
##' 
##' }
contextRequery<-function(seglist, offset=0,offsetRef='START',seqLength=1,resultType=NULL,dbUUID=NULL){
  if(!inherits(seglist,"emuRsegs")){
    stop("Segment list 'seglist' must be of type 'emuRsegs'. (Do not set a value for 'resultType' parameter for the query, the default resultType wiil be used)")
  }
 
  #   
  distinctEmuDbs=sqldf("SELECT DISTINCT db_uuid FROM seglist")
  distinctEmuDbsCnt=nrow(distinctEmuDbs)
  
  
  if(distinctEmuDbsCnt==0){
    # empty seglist, return the empty list
    return(seglist)
  }else if (distinctEmuDbsCnt>1){
    stop("Context query over multiple emuDbs (in this case: ",distinctEmuDbsCnt,") not (yet) supported.")
  }else{
    # all rows of seglist are in same emuDB
    dbUUID=distinctEmuDbs[1,'db_uuid']
    
    # load emuDB object
    db=.load.emuDB.DBI(uuid = dbUUID)
    # load config
    dbConfig=db[['DBconfig']]
    
    # create temporary items
    emuDBs.query.tmp=list()
    emuDBs.query.tmp[['queryItems']]<-dbGetQuery(getEmuDBcon(),paste0("SELECT * FROM items WHERE db_uuid='",dbUUID,"'"))
    emuDBs.query.tmp[['queryLabels']]<-dbGetQuery(getEmuDBcon(),paste0("SELECT * FROM labels WHERE db_uuid='",dbUUID,"'"))
    emuDBs.query.tmp[['queryLinksExt']]<-dbGetQuery(getEmuDBcon(),paste0("SELECT * FROM linksExt WHERE db_uuid='",dbUUID,"'"))
    setQueryTmpEmuDBs(emuDBs.query.tmp)
    
    items=dbReadTable(getEmuDBcon(),'items')
    itemsIdxSql='CREATE INDEX items_idx ON items(itemID,db_uuid,session,bundle,level,itemID,seqIdx,type,sampleRate,sampleStart,sampleDur,samplePoint)'
    resIdxSql='CREATE INDEX its_idx ON its(db_uuid,session,bundle,seqStartId,seqEndId,seqLen,level)'
    #   
    labelsIdxSql='CREATE INDEX labels_idx ON lblsDf(itemID,name)'
    labelsIdxSql='CREATE INDEX labels_idx ON lblsDf(itemID,db_uuid,session,bundle,name)'
    
    
    heQueryStr=paste0("SELECT il.db_uuid,il.session,il.bundle,il.itemID AS seqStartId,ir.itemID AS seqEndID,",seqLength," AS seqLen,il.level FROM seglist sl,items sll,items slr,items il, items ir \
                        WHERE \
                         il.db_uuid=ir.db_uuid AND il.session=ir.session AND il.bundle=ir.bundle AND \
                         il.db_uuid=sl.db_uuid AND il.session=sl.session AND il.bundle=sl.bundle AND \
                         sll.db_uuid=sl.db_uuid AND sll.session=sl.session AND sll.bundle=sl.bundle AND sl.startItemID=sll.itemID AND \
                         slr.db_uuid=sl.db_uuid AND slr.session=sl.session AND slr.bundle=sl.bundle AND sl.endItemID=slr.itemID AND ")
    if(offsetRef=='START'){
      heQueryStr=paste0(heQueryStr,"il.level=sll.level AND il.seqIdx=sll.seqIdx+",offset," AND \
                          ir.level=sll.level AND ir.seqIdx=sll.seqIdx+",offset+seqLength)
     }else if(offsetRef=='END'){
       heQueryStr=paste0(heQueryStr,"il.level=slr.level AND il.seqIdx=slr.seqIdx+",offset," AND \
                          ir.level=slr.level AND ir.seqIdx=slr.seqIdx+",offset+seqLength)
     }else{
       stop("Parameter offsetRef must be one of 'START' or 'END'\n")
     }
    he=sqldf(heQueryStr)
    result=list(items=he)
    trSl=convert.query.result.to.segmentlist(dbConfig = dbConfig,result=result)
    return(trSl)
  }
}

