require(sqldf)
require(stringr)

EMPTY_RESULT_DF=data.frame(seqStartId=character(0),seqEndId=character(0),seqLen=integer(0),level=character(0),stringsAsFactors = FALSE)

# .get.item.by.id <- function(bundle,id){
#   
#   for(lvl in bundle$levels){
#     for(item in lvl$items){
#       if(item$id==id){
#         return(item)
#       }
#     }
#   }
# }

# is.level.name<-function(schema,str){
#   
# }
# 
# list.related.items <-function(bundle,item,direction='to',recursive=TRUE){
#   relItems=list()
#   itemId=item$id
#   for(l in bundle$links){
#     if(l$fromID==itemId){
#       item=.get.item.by.id(bundle,l$toID)
#       relItems[[length(relItems)+1]]=item
#     }
#   }
#   
#   resItems=relItems
#   if(recursive){
#     
#     for(rItem in relItems){
#       relSubItems=list.related.items(bundle,rItem,direction=direction)
#       resItems=c(resItems,relSubItems)
#     }
#   }
#   return(resItems)
#   
# }


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
   
    itemsAsSeqQStr=paste0("SELECT itemID AS seqStartId, itemID AS seqEndId,1 AS seqLen,'",levelName,"' AS level FROM itemsMatch")
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


#list.seq.items<-function(resSeq){
#  sqIts=sqldf("SELECT i.* FROM items i,items s,items e,resSeq r WHERE s.id=r.seqStartId AND e.id=r.seqEndId AND i.session=s.session AND i.bundle=s.bundle AND i.level=s.level AND i.seqIdx>=s.seqIdx AND i.seqIdx<=e.seqIdx")
#  return(sqIts)
#}


#list.seq.item.ids<-function(resSeq){
#  cat("Seqs:",nrow(resSeq),"\n")
#  sqIts=sqldf("SELECT i.id FROM items i,items s,items e,resSeq r WHERE s.id=r.seqStartId AND e.id=r.seqEndId AND i.session=s.session AND i.bundle=s.bundle AND i.level=s.level AND i.seqIdx>=s.seqIdx AND i.seqIdx<=e.seqIdx")
#  return(sqIts)
#}

equal.emusegs<-function(seglist1,seglist2,compareAttributes=TRUE,tolerance=0.0,uttsPrefix2=''){
  if(!inherits(seglist1,"emusegs")){
    stop("seglist1 is not of class emusegs")
  }
  if(!inherits(seglist2,"emusegs")){
    stop("seglist2 is not of class emusegs")
  }
  if(tolerance<0){
    stop("tolerance must be greater or equal 0")
  }
  
  sl1RowCnt=nrow(seglist1)
  sl2RowCnt=nrow(seglist2)
  if(sl1RowCnt!=sl2RowCnt){
    return(FALSE)
  }
  if(compareAttributes){
    attrEq=((attr(seglist1,'query')==attr(seglist2,'query'))&attr(seglist1,'database')==attr(seglist2,'database'))
    if(!attrEq){
      return(FALSE)
    }
  }
  # seglist have no implicit order
  # to compare each segment or segment sequence we have to sort them
  #sl1bo=order('[[.data.frame'(seglist1,'bundle'))
  #sl1obb=`[.data.frame`(seglist1,sl1bo)
  #sl1so=order('[[.data.frame'(sl1obb,'start'))
  #sl1obbs=`[.data.frame`(sl1obb,sl1bo)
  
  sl1Order=order('[[.data.frame'(seglist1,'utts'),'[[.data.frame'(seglist1,'start'),'[[.data.frame'(seglist1,'end'),'[[.data.frame'(seglist1,'labels'))
  sl1=`[.data.frame`(seglist1,sl1Order,)
  sl2Order=order('[[.data.frame'(seglist2,'utts'),'[[.data.frame'(seglist2,'start'),'[[.data.frame'(seglist2,'end'),'[[.data.frame'(seglist2,'labels'))
  sl2=`[.data.frame`(seglist2,sl2Order,)
  equal=TRUE
  rs=1:sl1RowCnt
  for(i in rs){
    l1='[[.data.frame'(sl1,i,'labels')
    l2='[[.data.frame'(sl2,i,'labels')
    s1='[[.data.frame'(sl1,i,'start')
    s2='[[.data.frame'(sl2,i,'start')
    e1='[[.data.frame'(sl1,i,'end')
    e2='[[.data.frame'(sl2,i,'end')
    u1='[[.data.frame'(sl1,i,'utts')
    u2='[[.data.frame'(sl2,i,'utts')
    u2Sess=paste0(uttsPrefix2,u2)
    if(l1!=l2 | u1!=u2Sess){
      return(FALSE)
    }
    sdAbs=abs(s2-s1)
    if(sdAbs>tolerance){
      equal=FALSE
      #cat("Start differs ",s1,s2,sdAbs,"\n")
    }
    edAbs=abs(e2-e1)
    if(edAbs>tolerance){
      equal=FALSE
      #cat("End differs ",e1,e2,edAbs,"\n")
    }
    
  }
  return(equal)
  
}


convert.query.result.to.seglist<-function(database,result){
  its=NULL
  items=database[['items']]
  bundles=c()
  labels=c()
  start=c()
  end=c()
  slType=NULL
  projectionItems=result[['projectionItems']]
  if(!is.null(projectionItems)){ 
    its=data.frame(seqStartId=projectionItems[,'pSeqStartId'],seqEndId=projectionItems[,'pSeqEndId'],seqLen=projectionItems[,'pSeqLen'],level=projectionItems[,'pLevel'],stringsAsFactors = FALSE)
  }else{
    its=result[['items']]
  }
  itCount=nrow(its)
  if(itCount>0){
    links=database[['linksExt']]
    lblsDf=database[['labels']]   
    
    # get max length
    #itemsIdxSql='CREATE INDEX items_idx ON items(seqLen)'
    maxSeqLenDf=sqldf("SELECT max(seqLen) AS maxSeqLen FROM its")
    maxSeqLen=maxSeqLenDf[1,'maxSeqLen']
    #cat("Max seq len: ",maxSeqLen,"\n")
    
    # build query
    # TODO assume equal seq len for each sequence for now!!
    # this is not the case for queries like emu.query("andosl","*","[#[Phonetic=t -> Phonetic=S] -> #Phonetic=I]")
    # which are not allowed by BNF but in fact they are working with Emu 2.3
    # result would be a mix with t->S and I items (sequence lengths 2 and 1)
    
    # build dynamic query to build label string for variable result sequence length
    selectStr='SELECT s.id || e.id AS id, '
    fromStr='FROM items s,items e,its q,'
    whereStr='WHERE s.id=q.seqStartId AND e.id=q.seqEndId AND '
    for(seqIdx in 1:maxSeqLen){
      selectStr=paste0(selectStr,'(SELECT l.label FROM lblsDf l WHERE l.itemID=i',seqIdx,'.id AND name=q.level) AS lbl',seqIdx)
      #selectLblStr=paste0(selectLblStr,)
      fromStr=paste0(fromStr,'items i',seqIdx)
      offset=seqIdx-1
      whereStr=paste0(whereStr,'i',seqIdx,'.session=s.session AND i',seqIdx,'.bundle=s.bundle AND i',seqIdx,'.level=s.level AND i',seqIdx,'.seqIdx=s.seqIdx+',offset)
      if(seqIdx<maxSeqLen){
        selectStr=paste0(selectStr,',')
        fromStr=paste0(fromStr,',')
        whereStr=paste0(whereStr,' AND ')
      }
    }
    
    seqLblQStr=paste0(selectStr,' ',fromStr,' ',whereStr,' ORDER BY id')
    #cat(seqLblQStr,"\n")
    seqLbls=sqldf(seqLblQStr)
    
    # query seglist data except labels
    # for this data the information in start end item of the sequence is sufficient
    # the CASE WHEN THEN ELSE END terms are necessary to get the start end end samples of sequences which are not segment levels and therefore have no time information  
    hasLinks=(nrow(links)>0)
    
    q="SELECT s.id || e.id AS id,s.session,s.bundle,s.itemID AS startitemID ,e.itemID AS enditemID,s.type, \
                CASE s.type \
                     WHEN 'SEGMENT' THEN s.sampleStart \
                     WHEN 'EVENT' THEN s.samplePoint ";
    if(hasLinks){
      q=paste0(q," ELSE (SELECT i.sampleStart FROM items i WHERE i.session=s.session AND i.bundle=s.bundle AND i.type='SEGMENT' AND EXISTS (SELECT * FROM links l WHERE s.itemID=l.fromID AND i.itemID=l.toID AND i.session=l.session AND i.bundle=l.bundle AND l.toSeqIdx=0)) ")
    }
    q=paste0(q," END AS sampleStart, \
                CASE s.type \
                    WHEN 'SEGMENT' THEN (e.sampleStart+e.sampleDur) \
                    WHEN 'EVENT' THEN 0 ")  
    if(hasLinks){
      q=paste0(q," ELSE (SELECT i.sampleStart+i.sampleDur FROM items i WHERE i.session=s.session AND i.bundle=s.bundle AND i.type='SEGMENT'  AND EXISTS (SELECT * FROM links l WHERE s.itemID=l.fromID AND i.itemID=l.toID AND i.session=l.session AND i.bundle=l.bundle AND l.toSeqIdx+1=l.toSeqLen)) ")
    }
    q=paste0(q,"END AS sampleEnd, \
                CASE s.type \
                    WHEN 'SEGMENT' THEN s.sampleRate \
                    WHEN 'EVENT' THEN s.sampleRate ")
    if(hasLinks){
      q=paste0(q," ELSE (SELECT i.sampleRate FROM items i WHERE i.session=s.session AND i.bundle=s.bundle AND i.type='SEGMENT' AND EXISTS (SELECT * FROM links l WHERE s.itemID=l.fromID AND i.itemID=l.toID AND i.session=l.session AND i.bundle=l.bundle AND l.toSeqIdx=0)) ")
    }
    q=paste0(q," END AS sampleRate \
                FROM items s,items e,its r \
                WHERE s.id=r.seqStartId AND e.id=r.seqEndId AND e.session=s.session AND e.bundle=s.bundle AND e.level=s.level \
                ORDER BY id")
    
    itemsIdxSql='CREATE INDEX items_idx ON items(id,session,bundle,level,itemID,seqIdx)'
    resIdxSql='CREATE INDEX its_idx ON its(seqStartId,seqEndId,seqLen,level)'
    linksIdxSql='CREATE INDEX links_idx ON links(session,bundle,fromID,toID)'
    segListData=sqldf(c(itemsIdxSql,resIdxSql,linksIdxSql,q))
    # Note: CASE s.type WHEN 'SEGMENT' OR 'EVENT' did not work.
    
    if(itCount>0){
      for(itIdx in 1:itCount){
        
        id=segListData[itIdx,'id']
        idSeqLbls=seqLbls[itIdx,'id']
        if(id!=idSeqLbls){
          stop("Internal error: Mismatch of sequence label and seglist table")
        }
        label=seqLbls[itIdx,'lbl1']
        if(maxSeqLen>1){
          for(itSeqIdx in 2:maxSeqLen){
            lblCol=paste0('lbl',itSeqIdx)
            label=paste0(label,'->',seqLbls[itIdx,lblCol])
          }
        }
        
        labels=c(labels,label)
       
        bundle=segListData[itIdx,'bundle']
        
        #if(database[['bundleNamesUnique']]){
        #  bundleIDStr=bundle
        #}else{
          session=segListData[itIdx,'session']
          bundleIDStr=paste(session,bundle,sep = ':')
        #}
        bundles=c(bundles,bundleIDStr)
        
        # calculate start end times in milliseconds     
        type=segListData[itIdx,'type']
        
        seqEnd=0
        if(type=='EVENT'){
          seqStart=(segListData[itIdx,'sampleStart']/segListData[itIdx,'sampleRate'])*1000
          slType='event'
        }else{
          # type SEGMENT or ITEM
          seqStart=((segListData[itIdx,'sampleStart']+0.5)/segListData[itIdx,'sampleRate'])*1000
          seqEnd=((segListData[itIdx,'sampleEnd']+1.5)/segListData[itIdx,'sampleRate'])*1000
          slType='segment'
        }
        start=c(start,seqStart)
        end=c(end,seqEnd)
        
      }
    }
  }
  segList=make.seglist(labels=labels, start=start, end=end, utts=bundles, query=result[['queryStr']], type=slType, database=database[['DBconfig']][['name']])
  return(segList)
}

query.database.eql.FUNKA<-function(database,q,items=NULL){
  # BNF: FUNKA = POSA | NUMA;
  qTrim=str_trim(q)
  if(is.null(items)){
    items=database[['items']]
  }
  allItems=database[['items']]
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
        stop("Syntax error: Function '",funcName,"' requires exactly two parameters in '",q,"'\n")
      }
      param1=str_trim(params[[1]])
      param2=str_trim(params[[2]])
      # check attribute names
      aNms=get.attribute.names(database[['DBconfig']])
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
      links=database[['linksExt']]
      #links=database[['linksWithPositions']]
      itemsAsSeqs=NULL
      
      level1=get.level.name.for.attribute(database,param1)
      level2=get.level.name.for.attribute(database,param2)
      
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
      # TODO parse funcValueTerm for Num function
      
      if(funcName=='Start'){
        # TODO assume 1 as function value for now
        cond=NULL
        if(funcValue=='0'){
          cond='!='
        }else if(funcValue=='1'){
          cond='='
        }else{
          stop("Syntax error: Expected function value 0 or 1 after '=' in function term: '",qTrim,"'\n")
        }
        sqlQStr=paste0("SELECT DISTINCT i.id AS seqStartId, i.id AS seqEndId,1 AS seqLen,'",param2,"' AS level FROM items i,allItems d WHERE i.level='",level2,"' AND d.level='",level1,"' AND EXISTS (SELECT * FROM links k WHERE k.session=i.session AND k.bundle=i.bundle AND k.session=d.session AND k.bundle=d.bundle AND k.fromID=d.itemID AND k.toID=i.itemID AND k.toSeqIdx",cond,"0)") 
        itemsAsSeqs=sqldf(sqlQStr)
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
        sqlQStr=paste0("SELECT DISTINCT i.id AS seqStartId, i.id AS seqEndId,1 AS seqLen,'",param2,"' AS level FROM items i,allItems d WHERE i.level='",level2,"' AND d.level='",level1,"' AND EXISTS (SELECT * FROM links k WHERE k.session=i.session AND k.bundle=i.bundle AND k.session=d.session AND k.bundle=d.bundle AND k.fromID=d.itemID AND k.toID=i.itemID AND (k.toSeqIdx",cond,"0 ",bOp," k.toSeqIdx+1",cond,"k.toSeqLen))") 
        itemsAsSeqs=sqldf(sqlQStr)
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
        sqlQStr=paste0("SELECT DISTINCT i.id AS seqStartId, i.id AS seqEndId,1 AS seqLen,'",param2,"' AS level FROM items i,allItems d WHERE i.level='",level2,"' AND d.level='",level1,"' AND EXISTS (SELECT * FROM links k WHERE k.session=i.session AND k.bundle=i.bundle AND k.session=d.session AND k.bundle=d.bundle AND k.fromID=d.itemID AND k.toID=i.itemID AND k.toSeqIdx+1",cond,"k.toSeqLen)") 
        itemsAsSeqs=sqldf(sqlQStr)
        resultLevel=param2
      }else if(funcName=='Num'){
        # BNF: NUMA = 'Num','(',EBENE,',',EBENE,')',VOP,INTPN;
        # Note return value level is param1 here
       # sqlQStr=paste0("SELECT d.id AS seqStartId, d.id AS seqEndId FROM allItems i,items d WHERE i.level='",param2,"' AND d.level='",param1,"' AND EXISTS (SELECT * FROM links k WHERE k.bundle=i.bundle AND k.bundle=d.bundle AND k.fromID=d.itemID AND k.toID=i.itemID AND k.toLevel=i.level AND k.toSeqLen=",funcValue,")")
        #numChilds=as.integer(funcValue)
        #if(is.na(numChilds)){
         # stop("Syntax error: Expected integer value after '=' in function term: '",qTrim,"'\n")
        #}
        sqlQStr=paste0("SELECT DISTINCT d.id AS seqStartId, d.id AS seqEndId,1 AS seqLen,'",param1,"' AS level FROM allItems i,items d WHERE i.level='",level2,"' AND d.level='",level1,"' AND EXISTS (SELECT * FROM links k WHERE  k.session=i.session AND k.session=d.session AND k.bundle=i.bundle AND k.bundle=d.bundle AND k.fromID=d.itemID AND k.toID=i.itemID AND k.toLevel=i.level AND k.toSeqLen",funcValueTerm,")") 
        itemsAsSeqs=sqldf(sqlQStr)
        resultLevel=param1
      }else{
        stop("Unknwon function: '",funcName,"'")
      }
      res=create.subtree(items=itemsAsSeqs,links=NULL,resultLevel=resultLevel)
    }
  }else{
    stop("Syntax error: Missing opening round bracket '(' in '",q,"'\n")
  }
}

query.database.eql.EA<-function(database,q,items=NULL,labels=NULL){
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
      res=query.database.eql.FUNKA(database,qTrim,items)
    }
  }else{
    # No round brackets, assuming a level query
    res=query.database.eql.ETTIKETTA(database,qTrim,labels)
  }
  
  return(res)
}

query.database.eql.ETTIKETTA<-function(database,q,labels=NULL){
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
        
        #stop("Hash tag of EQL currently not supported")
        
        
        lvlName=str_trim(substring(lvlTrim,2))
        projectionLevel=TRUE
      }
      # TODO check if level exists
      #cat("Level: '",lvlName,"'\n",sep='')
      aNms=get.attribute.names(database[['DBconfig']])
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
      # BNF: ETIKETT = ETIKETTIERUNG | (“'“,ETIKETTIERUNG,“'“);
      # Suggestion for improvement:
      # labelGroups (legacy EMU 'legal' directive) MUST NOT be quoted, to distinguish labelGroups from ordinary label or label pattern:
      # BNF__: ETIKETT = LABEL_GROUP_NAME | ETIKETTIERUNG | (“'“,ETIKETTIERUNG,“'“);
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
          lvlDefs=database[['DBconfig']][['levelDefinitions']]
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
        # use all items of database if not set
        labels=database[['labels']]
      }
      res=query.database.level.label(ldf=labels,levelName=lvlName,cond)
      res[['projectionItems']]=NULL
      if(projectionLevel){
        rIts=res[['items']]
        # projection of the result items themself
        res[['projectionItems']]=data.frame(seqStartId=rIts[,'seqStartId'],seqEndId=rIts[,'seqEndId'],seqLen=rIts[,'seqLen'],pSeqStartId=rIts[,'seqStartId'],pSeqEndId=rIts[,'seqEndId'],pSeqLen=rIts[,'seqLen'],pLevel=rIts[,'level'],stringsAsFactors = FALSE)
      }
      return(res)
    }
  }
  stop("Syntax error: No operator found.")
}

query.database.eql.KONJA<-function(database,q){
   # BNF: KONJA = EA,{'&',EA};
    qTrim=str_trim(q)
    conditions=list()
    # initialize with empty result
    res=create.subtree(items=EMPTY_RESULT_DF,links=NULL,resultLevel=NULL,projectionItems=NULL)
    startPos=1
    p=0
    items=database[['items']]
    labels=database[['labels']]
    resultLevel=NULL
    while(p>=0){
      p=get.string.position(string=qTrim,searchStr='&',pos=startPos,literalQuote="'")
      if(p==-1){
        condStr=str_trim(substring(qTrim,startPos))
      }else{
        condStr=str_trim(substr(qTrim,startPos,p-1))
        
        startPos=p+1
      }
      #cat(condStr,"\n")
      res=query.database.eql.EA(database,condStr,items=items,labels=labels)
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
        items=sqldf("SELECT i.* FROM items i,seqIts s WHERE i.id=s.seqStartId")
        labels=sqldf("SELECT l.* FROM labels l,seqIts s WHERE l.itemID=s.seqStartId")
      }
    }
    res[['items']][,'level']=resultLevel
    res[['resultLevel']]=resultLevel
    return(res)
}

query.database.eql.in.bracket<-function(database,q){
  parseRes=list()
  qTrim=str_trim(q)
  # parse SEQA or DOMA
  seqPos=get.string.position.outside.brackets(qTrim,'->',literalQuote="'",bracket=c('[',']'))
  domPos=get.string.position.outside.brackets(qTrim,'^',literalQuote="'",bracket=c('[',']'))
  if(seqPos!=-1 || domPos!=-1){
    items=database[['items']]
    links=database[['linksExt']]
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
    lRes=query.database.with.eql(database,left)
    rRes=query.database.with.eql(database,right) 
    
    # get items on dominance compare levels
    lResIts=lRes[['items']]
    lResLvl=lRes[['resultLevel']]
    lResPIts=lRes[['projectionItems']]
    
    lLvlItems=NULL
    # sqldf cannot handle empty data frames 
    lResItsNrows=nrow(lResIts)
    if(lResItsNrows==0){
      res=create.subtree(items=lResIts,links=NULL,resultLevel=lResLvl,projectionItems=lResPIts)
      return(res)
    }
    #else{
    #  lqStr=paste0("SELECT i.* FROM lResIts ls,items i WHERE i.id=ls.seqStartId AND level='",lResLvl,"'")
    #  #lqStr=paste0("SELECT i.* FROM lResIts ls,items i WHERE i.id=ls.seqStartId")
    #  lLvlItems=sqldf(lqStr)
    #}
    rResIts=rRes[['items']]
    rResLvl=rRes[['resultLevel']]
    rResPIts=rRes[['projectionItems']]
    
    rLvlItems=NULL
    # sqldf cannot handle empty data frames 
    rResItsNrows=nrow(rResIts)
    if(rResItsNrows==0){
      res=create.subtree(items=rResIts,links=NULL,resultLevel=lResLvl,projectionItems=rResPIts)
      return(res)
    }
    #else{
    #  rqStr=paste0("SELECT i.* FROM rResIts rs,items i WHERE i.id=rs.seqStartId AND level='",rResLvl,"'")
    #  #rqStr=paste0("SELECT i.* FROM rResIts rs,items i WHERE i.id=rs.seqStartId")
    #  rLvlItems=sqldf(rqStr)
    #}
    #lLvlItemsNrows=nrow(lLvlItems)
    #rLvlItemsNrows=nrow(rLvlItems)
    #if(lLvlItemsNrows==0){    
    #  res=create.subtree(items=,lLvlItems,links=NULL,resultLevel=lResLvl,projectionItems=NULL)
    #  return(res)
    #} 
    #if( rLvlItemsNrows==0){
    #  
    #  res=create.subtree(items=rLvlItems,links=NULL,resultLevel=lResLvl,projectionItems=NULL)
    #  return(res)
    #}
    
    if(domPos!=-1){
      # parse DOMA
      # query the result level of left term
      
      linksNrows=nrow(links)
      if(linksNrows==0){
        res=create.subtree(items=EMPTY_RESULT_DF,links=NULL,resultLevel=lResLvl,projectionItems=NULL)
        return(res)
      }
      
      # right seq items
      #rSeqIts=list.seq.items(rResIts) 
      rSeqIts=sqldf("SELECT i.* FROM items i,items s,items e,rResIts r WHERE s.id=r.seqStartId AND e.id=r.seqEndId AND i.session=s.session AND i.bundle=s.bundle AND i.level=s.level AND i.seqIdx>=s.seqIdx AND i.seqIdx<=e.seqIdx")
      lSeqRes=EMPTY_RESULT_DF
      
      # build dominance SQL query string 
      itemsSameBundleCond="ils.session=irs.session AND ils.session=ile.session AND ile.session=ire.session AND ils.bundle=irs.bundle AND ils.bundle=ile.bundle AND ile.bundle=ire.bundle"
      linkSameBundleCond1="k.session=ils.session AND k.session=irs.session AND k.bundle=ils.bundle AND k.bundle=irs.bundle"
      linkSameBundleCond2="m.session=ils.session AND m.session=irs.session AND m.bundle=ils.bundle AND m.bundle=irs.bundle"
      lDomQuerySelectStr="lid.seqStartId,lid.seqEndId,lid.seqLen,lid.level"
      rDomQuerySelectStr="rid.seqStartId AS rsId,rid.seqEndId AS reId,rid.seqLen AS rL,rid.level AS rLev"
      domQueryFromStr="lResIts lid, rResIts rid,items ils, items irs,items ile,items ire"
      domQueryStrCond0=paste0("ils.id=lid.seqStartId AND irs.id=rid.seqStartId AND ile.id=lid.seqEndId AND ire.id=rid.seqEndId AND ",itemsSameBundleCond)
      # The query has now the corners of the dominance "trapeze" in ils,ile,irs,ire
      # Check sequence start item of left result on existence of a link to the start item of the right sequence 
      domQueryStrCond1=paste0("EXISTS (SELECT * FROM links k WHERE ",linkSameBundleCond1," AND ((k.fromID=ils.itemID AND k.toID=irs.itemID) OR (k.toID=ils.itemID AND k.fromID=irs.itemID)))")
      # ... and sequence end item of left result on existence of a link to the end item of the right sequence 
      domQueryStrCond2=paste0("EXISTS (SELECT * FROM links m WHERE ",linkSameBundleCond2," AND ((m.fromID=ile.itemID AND m.toID=ire.itemID) OR (m.toID=ile.itemID AND m.fromID=ire.itemID)))")
      
      # concatenate the query string
      domQueryStrTail=paste0(" FROM ",domQueryFromStr," WHERE ", domQueryStrCond0, " AND ", domQueryStrCond1," AND ",domQueryStrCond2)
      lrDomQueryStr=paste0("SELECT DISTINCT ",lDomQuerySelectStr,",",rDomQuerySelectStr,domQueryStrTail)
      #cat("dominance query string: ",domQueryStr,"\n")
      #
      # Experimenting with SQLITE indices ...
     #cat("left res its:",nrow(lResIts)," right res its: ",nrow(rResIts),"\n")
      itemsIdxSql='CREATE INDEX items_idx ON items(id,session,bundle,level,itemID,seqIdx)'
      lResIdxSql='CREATE INDEX lResIts_idx ON lResIts(seqStartId,seqEndId,seqLen,level)'
      #rResIdxSql='CREATE INDEX rResIts_idx ON rResIts(seqStartId,seqEndId,seqLen,level)' 
      linksIdxSql='CREATE INDEX links_idx ON links(session,bundle,fromID,toID)'
      
#       idcSql=c('CREATE INDEX items_id_idx ON items(id)','CREATE INDEX items_sb_idx ON items(session,bundle)',
#                'CREATE INDEX links_sb_idx ON links(session,bundle)',
#                'CREATE INDEX links_fromID_idx ON links(fromID)',
#                'CREATE INDEX links_toID_idx ON links(toID)'
#                # this indices cause a dramatic performance regression (Why???)  
#                #'CREATE INDEX rResIts_se_idx ON rResIts(seqStartId,seqEndId)',
#               #'CREATE INDEX lResIts_se_idx ON lResIts(seqStartId,seqEndId)'
#               )
# 
#      
    # myserious: query is much slower if rResIts_Idx is calculated as well
      lrExpRes=sqldf(c(itemsIdxSql,linksIdxSql,lResIdxSql,lrDomQueryStr))
      #lrExpRes=sqldf(c(idcSql,lrDomQueryStr))
      
      #lExpRes=data.frame(seqStartId=lrExpRes[,'seqStartId'],seqEndId=lrExpRes[,'seqEndId'],seqLen=lrExpRes[,'seqLen'],level=lrExpRes[,'level'],stringsAsFactors = FALSE)
      # lrExpRes might have double items, use a distinct select to create the data.frame for left term
      # for example in the query "[ Syllable=S ^ Phonetic=s ]" on ae there exists one Syllable S which dominates two Phonetic s items 
      # Fix for issue #12
      lrExpResIdxSql='CREATE INDEX lrExpRes_idx ON lrExpRes(seqStartId,seqEndId,seqLen,level)'
      #lExpRes=sqldf(c(lrExpResIdxSql,"SELECT DISTINCT seqStartId,seqEndId,seqLen,level FROM lrExpRes"))
      lExpRes=sqldf("SELECT DISTINCT seqStartId,seqEndId,seqLen,level FROM lrExpRes")
      lPrjIts=NULL
      rPrjIts=NULL
      if(nrow(lrExpRes)>0){
        if(!is.null(lResPIts)){
          # TODO
          #lPrjIts=reduce.projection.items(lExpRes,lResPIts)
          qStr="SELECT i.seqStartId,i.seqEndId,i.seqLen,pi.pSeqStartId,pi.pSeqEndId,pi.pSeqLen,pi.pLevel FROM lrExpRes i,lResPIts pi WHERE i.seqStartId=pi.seqStartId AND i.seqEndId=pi.seqEndId"
          #rQStr="SELECT rpi.* FROM rightProjectionItems rpi WHERE EXISTS (SELECT i.seqStartId FROM resultItems i WHERE i.seqStartId=rpi.seqStartId && i.seqEndId=rpi.seqEndId)"
          
          #qStr=paste0(lQStr," UNION ",rQStr)
          lPrjIts=sqldf(qStr)
        }
        
        if(!is.null(rResPIts) && nrow(rResPIts)>0){
          qStr="SELECT i.seqStartId,i.seqEndId,i.seqLen,pi.pSeqStartId,pi.pSeqEndId,pi.pSeqLen,pi.pLevel FROM lrExpRes i,rResPIts pi WHERE i.rsId=pi.seqStartId AND i.reId=pi.seqEndId"
          #rQStr="SELECT rpi.* FROM rightProjectionItems rpi WHERE EXISTS (SELECT i.seqStartId FROM resultItems i WHERE i.seqStartId=rpi.seqStartId && i.seqEndId=rpi.seqEndId)"
          
          #qStr=paste0(lQStr," UNION ",rQStr)
          rPrjIts=sqldf(qStr)
          #rExpRes=data.frame(seqStartId=lrExpRes[,'rsId'],seqEndId=lrExpRes[,'reId'])
          #cat("dominance query string: ",domQueryStr,"\n")
          #rExpRes=sqldf(rDomQueryStr)
          #rPrjIts=reduce.projection.items(rExpRes,rResPIts)
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
      if(lResLvl!=rResLvl){
        stop("Levels of sequence query '",qTrim,"' do not match. (",lResLvl," not equal ",rResLvl,")")
      }
      #seqQueryStr=paste0("SELECT lid.seqStartId,rid.seqEndId FROM lResIts lid, rResIts rid,items il, items ir WHERE il.id=lid.seqEndId AND ir.id=rid.seqStartId AND il.level='",lResLvl,"' AND il.level=ir.level AND il.bundle=ir.bundle AND ir.seqIdx=il.seqIdx+1")
      #lrSeqQueryStr=paste0("SELECT lid.seqStartId,lid.seqEndId AS leId,rid.seqStartId AS rsId,rid.seqEndId FROM lResIts lid, rResIts rid,items il, items ir WHERE il.id=lid.seqEndId AND ir.id=rid.seqStartId AND il.level='",lResLvl,"' AND il.level=ir.level AND il.bundle=ir.bundle AND ir.seqIdx=il.seqIdx+1")
      lrSeqQueryStr=paste0("SELECT lid.seqStartId,lid.seqEndId AS leId,rid.seqStartId AS rsId,rid.seqEndId,lid.seqLen+rid.seqLen AS seqLen,lid.level FROM lResIts lid, rResIts rid,items il, items ir WHERE il.id=lid.seqEndId AND ir.id=rid.seqStartId AND il.level=ir.level AND il.session=ir.session AND il.bundle=ir.bundle AND ir.seqIdx=il.seqIdx+1")
      lrExpRes=sqldf(lrSeqQueryStr)
      
      # select final result columns
      lExpRes=data.frame(seqStartId=lrExpRes[,'seqStartId'],seqEndId=lrExpRes[,'seqEndId'],seqLen=lrExpRes[,'seqLen'],level=lrExpRes[,'level'],stringsAsFactors = FALSE)
      
      lPrjIts=NULL
      if(!is.null(lResPIts)){
        # TODO
        #lPrjIts=reduce.projection.items(lExpRes,lResPIts)
        qStr="SELECT i.seqStartId,i.seqEndId,pi.pSeqStartId,pi.pSeqEndId,pi.pSeqLen,pi.pLevel FROM lrExpRes i,lResPIts pi WHERE i.seqStartId=pi.seqStartId AND i.leId=pi.seqEndId"
        #rQStr="SELECT rpi.* FROM rightProjectionItems rpi WHERE EXISTS (SELECT i.seqStartId FROM resultItems i WHERE i.seqStartId=rpi.seqStartId && i.seqEndId=rpi.seqEndId)"
        
        #qStr=paste0(lQStr," UNION ",rQStr)
        lPrjIts=sqldf(qStr)
      }
      rPrjIts=NULL
      if(!is.null(rResPIts)){
        
        qStr="SELECT i.seqStartId,i.seqEndId,pi.pSeqStartId,pi.pSeqEndId,pi.pSeqLen,pi.pLevel FROM lrExpRes i,rResPIts pi WHERE i.rsId=pi.seqStartId AND i.seqEndId=pi.seqEndId"
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
    
    res=create.subtree(items=lExpRes,links=NULL,resultLevel=lResLvl,projectionItems=prjIts)
    return(res)
  }
  stop("Syntax error: Expected sequence '->' or domination '^' operator.")
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
query.database.with.eql.seglist<-function(database,query){

  rs=query.database.with.eql(database,query)
  segList=convert.query.result.to.seglist(database,rs)
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
query.database.with.eql<-function(database,query){
  parseRes=list()
  qTrim=str_trim(query)
  
  brOpenPos=get.char.position(qTrim,'[',literalQuote="'")
  if(brOpenPos==-1){
    res=query.database.eql.KONJA(database,qTrim)
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
    inBrRes=query.database.eql.in.bracket(database,inBr)
    inBrRes[['queryStr']]=query
    return(inBrRes)
    
  }
  stop("Unknown syntax error.")
}

##' Query EMU database
##' @description Query an EMU database
##' @param dbObj object of class emuDB
##' @param query EQL query string
##' @param queryLang query language
##' @param resultType type (class name) of result
##' @return result set object of class resultType (e.g. EMU seglist 'emusegs')
##' @author Klaus Jaensch
##' @import sqldf stringr
##' @seealso \code{\link{load.emuDB}}
##' @keywords emuDB database query Emu EQL 
##' @examples
##' \dontrun{
##' ## Query database object ae with EQL query [Phonetic=t -> Phonetic=s] 
##' ## and store result seglist in variable segListTs
##' 
##' seglistTs=query(ae,"[Phonetic=t -> Phonetic=s]",resultType='emusegs')
##' 
##' ## Query seglist from database object ae with EQL query [Syllable=S ^ Phoneme=t]
##' 
##' query(ae,"[Syllable=S ^ Phoneme=t]",resultType='emusegs')
##' 
##' }
##' @export
"query"<-function(dbObj,query,queryLang=NULL,resultType=NULL){
  UseMethod("query")
}

#query.default <- query


##' @export
"query.emuDB"<-function(dbObj,query,queryLang='EQL2',resultType=NULL){
  dbClass=class(dbObj)
  if(dbClass=='emuDB'){
    if(queryLang=='EQL2'){
      if(is.null(resultType)){
        return(query.database.with.eql(dbObj,query))
      }else{
        if(resultType=='emusegs'){
          return(query.database.with.eql.seglist(dbObj,query))
        }else{
          stop("Unknown result type: '",resultType,"'. Supported result types: 'emusegs', NULL")
        }
      }
    }else{
      stop("Unknown query language '",queryLang,"'.")
    }
  }else{
    NextMethod()
  }
}


##' Requery EMU database
##' @description Requery an EMU database
##' @param dbObj object of class emuDB
##' @param segs segment list
##' @param level level of input segment list
##' @param targetlevel target level
##' @param justlabels labels only
##' @param sequence integer which determins sibling position relative to segment in segs 
##' @param longerok TODO ??
##' @param resultType type (class name) of result
##' @return result set object of class resultType (e.g. EMU seglist 'emusegs')
##' @author Klaus Jaensch
##' @import sqldf stringr
##' @seealso \code{\link{load.emuDB}}
##' @keywords emuDB database query Emu EQL 
##' @examples
##' \dontrun{
##' ## Requery phonetic context of syllable
##' 
##' requery(ae,sl1,level='Syllable',targetlevel='Phonetic')
##' 
##' ## Requery previous element on phonetic level
##' 
##' requery(ae,sl2,level='Phonetic',sequence=-1)
##' 
##' }
"requery"<-function(dbObj,segs, level,targetlevel=level, justlabels=FALSE, sequence=0, longerok=FALSE,resultType=NULL){
  UseMethod("requery")
}



"requery.emuDB"<-function(dbObj,segs, level, targetlevel=level, justlabels=FALSE, sequence=0, longerok=FALSE,resultType=NULL){
  stop("Not yet implemented!")
  dbClass=class(dbObj)
  if(dbClass=='emuDB'){
    if(queryLang=='EQL2'){
      if(is.null(resultType)){
        return(requery.database.with.eql(dbObj,query))
      }else{
        if(resultType=='emusegs'){
          return(requery.database.with.eql.seglist(dbObj,query))
        }else{
          stop("Unknown result type: '",resultType,"'. Supported result types: 'emusegs', NULL")
        }
      }
    }else{
      stop("Unknown query language '",queryLang,"'.")
    }
  }else{
    NextMethod()
  }
}

