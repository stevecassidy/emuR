


convert.query.result.to.seglist<-function(dbConfig,result){
  its=NULL
  
  items=getQueryTmpEmuDBs()[['queryItems']]
  
  bundles=c()
  labels=c()
  start=c()
  end=c()
  slType=NULL
  projectionItems=result[['projectionItems']]
  if(!is.null(projectionItems)){ 
    its=data.frame(db_uuid=projectionItems[,'db_uuid'],session=projectionItems[,'session'],bundle=projectionItems[,'bundle'],seqStartId=projectionItems[,'pSeqStartId'],seqEndId=projectionItems[,'pSeqEndId'],seqLen=projectionItems[,'pSeqLen'],level=projectionItems[,'pLevel'],stringsAsFactors = FALSE)
  }else{
    its=result[['items']]
  }
  itCount=nrow(its)
  if(itCount>0){
    links=getQueryTmpEmuDBs()[['queryLinksExt']]
    
    lblsDf=getQueryTmpEmuDBs()[['queryLabels']]
    itemsIdxSql='CREATE INDEX items_idx ON items(itemID,session,bundle,level,itemID,seqIdx,type,sampleRate,sampleStart,sampleDur,samplePoint)'
    resIdxSql='CREATE INDEX its_idx ON its(db_uuid,session,bundle,seqStartId,seqEndId,seqLen,level)'
    
    #labelsIdxSql='CREATE INDEX labels_idx ON lblsDf(itemID,name)'
    labelsIdxSql='CREATE INDEX labels_idx ON lblsDf(itemID,name)'
    # get max length
    #itemsIdxSql='CREATE INDEX items_idx ON items(seqLen)'
    maxSeqLenDf=sqldf(c(resIdxSql,"SELECT max(seqLen) AS maxSeqLen FROM its"))
    maxSeqLen=maxSeqLenDf[1,'maxSeqLen']
    #cat("Max seq len: ",maxSeqLen,"\n")
    
    # build query
    # TODO assume equal seq len for each sequence for now!!
    # this is not the case for queries like emu.query("andosl","*","[#[Phonetic=t -> Phonetic=S] -> #Phonetic=I]")
    # which are not allowed by BNF but in fact they are working with Emu 2.3
    # result would be a mix with t->S and I items (sequence lengths 2 and 1)
    
    # build dynamic query to build label string for variable result sequence length
    selectStr="SELECT s.db_uuid || '_' || s.session || '_' || s.bundle || '_' || s.itemID || '_' || e.itemID AS id, "
    fromStr='FROM items s,items e,its q,'
    whereStr='WHERE s.db_uuid=q.db_uuid AND s.session=q.session AND s.bundle=q.bundle AND s.itemID=q.seqStartId AND e.db_uuid=q.db_uuid AND e.session=q.session AND e.bundle=q.bundle AND e.itemID=q.seqEndId AND '
    for(seqIdx in 1:maxSeqLen){
      selectStr=paste0(selectStr,'(SELECT l.label FROM lblsDf l WHERE l.db_uuid=i',seqIdx,'.db_uuid AND l.session=i',seqIdx,'.session AND l.bundle=i',seqIdx,'.bundle AND   l.itemID=i',seqIdx,'.itemID AND l.name=q.level) AS lbl',seqIdx)
      #selectLblStr=paste0(selectLblStr,)
      fromStr=paste0(fromStr,'items i',seqIdx)
      offset=seqIdx-1
      whereStr=paste0(whereStr,'i',seqIdx,'.db_uuid=s.db_uuid AND i',seqIdx,'.session=s.session AND i',seqIdx,'.bundle=s.bundle AND i',seqIdx,'.level=s.level AND i',seqIdx,'.seqIdx=s.seqIdx+',offset)
      if(seqIdx<maxSeqLen){
        selectStr=paste0(selectStr,',')
        fromStr=paste0(fromStr,',')
        whereStr=paste0(whereStr,' AND ')
      }
    }
    
    seqLblQStr=paste0(selectStr,' ',fromStr,' ',whereStr,' ORDER BY id')
    #cat(seqLblQStr,"\n")
    #st=system.time((seqLbls=sqldf(c(itemsIdxSql,resIdxSql,labelsIdxSql,seqLblQStr))))
    #cat(st,"\n")
    seqLbls=sqldf(c(itemsIdxSql,resIdxSql,labelsIdxSql,seqLblQStr))
    
    # query seglist data except labels
    # for this data the information in start end item of the sequence is sufficient
    # it takes only the start  and end items of the query result in account
    # the CASE WHEN THEN ELSE END terms are necessary to get the start and end samples of sequences which are not segment levels and therefore have no time information  
    hasLinks=(nrow(links)>0)
    
    # select columns: id,session,bundle,startItemId,endItemID ,type ...
    q="SELECT s.db_uuid || '_' || s.session || '_' || s.bundle || '_' || s.itemID || '_' || e.itemID AS id,s.session,s.bundle,s.itemID AS startItemID ,e.itemID AS endItemID,s.type, "
    
    # find sequence start position
    # use sample start of sequence start item for type SEGMENT and samplePoint for type EVENT 
    q=paste0(q,"CASE s.type \
             WHEN 'SEGMENT' THEN s.sampleStart \
             WHEN 'EVENT' THEN s.samplePoint ")
    # calculate start sample for type ITEM
    if(hasLinks){
      # items of type ITEM have no (sample) time information
      # therefore we search for linked SEGMENT items and take their start sample position
      q=paste0(q," ELSE (SELECT i.sampleStart FROM items i WHERE i.db_uuid=s.db_uuid AND i.session=s.session AND i.bundle=s.bundle AND i.type='SEGMENT' AND \
               EXISTS (SELECT * FROM links l WHERE s.db_uuid=s.db_uuid AND s.session=l.session AND s.bundle=l.bundle AND s.itemID=l.fromID AND i.db_uuid=l.db_uuid AND i.session=l.session AND i.bundle=l.bundle AND i.itemID=l.toID AND l.toSeqIdx=0)) ")
    }else{
      # TODO
      # No sample start information. (throw error ?)
    }
    q=paste0(q," END AS sampleStart, ")
    
    # find sequence end position
    # use sample start plus sample duration of sequence end item for type SEGMENT and zero for type EVENT
    # TODO is zero correct here ?? should it be e.samplePoint instead ??
    q=paste0(q," CASE s.type \
             WHEN 'SEGMENT' THEN (e.sampleStart+e.sampleDur) \
             WHEN 'EVENT' THEN 0 ")  
    if(hasLinks){
      # items of type ITEM have no (sample) time information
      # therefore we search for linked SEGMENT items and take their end sample position
      q=paste0(q," ELSE (SELECT i.sampleStart+i.sampleDur FROM items i WHERE i.db_uuid=e.db_uuid AND i.session=e.session AND i.bundle=e.bundle AND i.type='SEGMENT'  AND \
               EXISTS (SELECT * FROM links l WHERE e.db_uuid=l.db_uuid AND e.session=l.session AND e.bundle=l.bundle AND e.itemID=l.fromID AND i.db_uuid=l.db_uuid AND i.session=l.session AND i.bundle=l.bundle AND i.itemID=l.toID AND l.toSeqIdx+1=l.toSeqLen)) ")
    }
    q=paste0(q,"END AS sampleEnd, ")
    
    # find samplerate
    # use sample rate of sequence start item for type SEGMENT and EVENT 
    q=paste0(q,"CASE s.type \
             WHEN 'SEGMENT' THEN s.sampleRate \
             WHEN 'EVENT' THEN s.sampleRate ")
    if(hasLinks){
      # items of type ITEM have no sample rate information
      # therefore we search for linked SEGMENT items and take their start sample position
      # TODO Can we use EVENT items as well ?
      q=paste0(q," ELSE (SELECT i.sampleRate FROM items i WHERE i.db_uuid=s.db_uuid AND i.session=s.session AND i.bundle=s.bundle AND i.type='SEGMENT' AND \
               EXISTS (SELECT * FROM links l WHERE s.itemID=l.fromID AND i.itemID=l.toID AND i.db_uuid=l.db_uuid AND i.session=l.session AND i.bundle=l.bundle AND l.toSeqIdx=0)) ")
    }else{
      # TODO no samplerate , error ??
    }
    
    q=paste0(q," END AS sampleRate ")
    
    # from clause
    q=paste0(q," FROM items s,items e,its r ")
    
    # where clause: make sure start and end are in same emuDB, session and bundle, select start and end id
    q=paste0(q," WHERE e.db_uuid=s.db_uuid AND e.session=s.session AND e.bundle=s.bundle AND r.db_uuid=s.db_uuid AND r.session=s.session AND r.bundle=s.bundle AND s.itemID=r.seqStartId AND e.itemID=r.seqEndId AND e.level=s.level ")
    
    # order by id
    q=paste0(q," ORDER BY id")
    
    # build indices
    #itemsIdxSql='CREATE INDEX items_idx ON items(id,session,bundle,level,itemID,seqIdx,type)' # very slow !!
    itemsIdxSql='CREATE INDEX items_idx ON items(type,db_uuid,session,bundle,level,itemID,seqIdx)'
    linksIdxSql='CREATE INDEX links_idx ON links(db_uuid,session,bundle,fromID,toID,toSeqIdx,toSeqLen)'
    if(hasLinks){
      #st=system.time((segListData=sqldf(c(itemsIdxSql,linksIdxSql,resIdxSql,q))))
      #cat(st," (with links)\n")
      segListData=sqldf(c(itemsIdxSql,linksIdxSql,resIdxSql,q))
    }else{
      #st=system.time((segListData=sqldf(c(itemsIdxSql,resIdxSql,q))))
      #cat(st," (without links)\n")
      segListData=sqldf(c(itemsIdxSql,resIdxSql,q))
    }
    # Note: CASE s.type WHEN 'SEGMENT' OR 'EVENT' did not work.
    
    if(itCount>0){
      for(itIdx in 1:itCount){
        
        id=segListData[itIdx,'id']
        idSeqLbls=seqLbls[itIdx,'id']
        if(id!=idSeqLbls){
          stop("Internal error: Mismatch of sequence label and seglist table IDs: ",id," not equal to ",idSeqLbls)
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
          sStart=segListData[itIdx,'sampleStart']
          sEnd=segListData[itIdx,'sampleEnd']
          sRate=segListData[itIdx,'sampleRate']
          if(is.na(sStart) || is.na(sEnd) || is.na(sRate)){
            seqStart=NA
            seqEnd=NA
          }else{
            seqStart=((segListData[itIdx,'sampleStart']+0.5)/segListData[itIdx,'sampleRate'])*1000
            seqEnd=((segListData[itIdx,'sampleEnd']+1.5)/segListData[itIdx,'sampleRate'])*1000
          }
          slType='segment'
        }
        start=c(start,seqStart)
        end=c(end,seqEnd)
        
      }
    }
    }
  segList=make.seglist(labels=labels, start=start, end=end, utts=bundles, query=result[['queryStr']], type=slType, database=dbConfig[['name']])
  return(segList)
}


convert.query.result.to.segmentlist<-function(dbConfig,result,timeRefSegmentLevel=NULL){
  its=NULL
  
  items=getQueryTmpEmuDBs()[['queryItems']]
  
  bundles=c()
  labels=c()
  start=c()
  end=c()
  slType=NULL
  
  projectionItems=result[['projectionItems']]
  if(!is.null(projectionItems)){ 
    # projection result (hashtag marker)
    its=data.frame(db_uuid=projectionItems[,'db_uuid'],session=projectionItems[,'session'],bundle=projectionItems[,'bundle'],seqStartId=projectionItems[,'pSeqStartId'],seqEndId=projectionItems[,'pSeqEndId'],seqLen=projectionItems[,'pSeqLen'],level=projectionItems[,'pLevel'],stringsAsFactors = FALSE)
  }else{
    # use "normal" result
    its=result[['items']]
  }
  
  # get distinct result levels ...
  distinctLevels=sqldf("SELECT DISTINCT level FROM its")
  for(attrNm in distinctLevels[,'level']){
    
    lvlNm=get.level.name.for.attribute(dbConfig = dbConfig,attributeName = attrNm)
    ld=get.levelDefinition(DBconfig = dbConfig,name = lvlNm)
    #cat("Level ",ld['name']," type ",ld['type'],"\n")
    if(ld['type']=='ITEM'){
      segLvlNms=find.segment.levels(dbConfig,attrNm)
      if(!is.null(timeRefSegmentLevel)){
        if(!(timeRefSegmentLevel %in% segLvlNms)){
          stop("Cannot resolve time information for result level '",attrNm,"' using segment time reference level '",timeRefSegmentLevel,"'\nPlease set one of these levels for timeRefSegmentLevel parameter: ",paste(segLvlNms,collapse=', '),".")
        }
      }else{
        segLvlsCnt=length(segLvlNms)
        if(segLvlsCnt>1){
          
          stop("Segment time information derivation for level '",attrNm,"' is ambiguous:\nThe level is linked to multiple segment levels: ",paste(segLvlNms,collapse=', '),"\nPlease select one of these levels using the 'timeRefSegmentLevel' query parameter.")
        }
      }
    }
    
  }
  
  
  itCount=nrow(its)
  if(itCount==0){
    its=data.frame(db_uuid=character(0),session=character(0),bundle=character(0),seqStartId=integer(0),seqEndId=integer(0),seqLen=integer(0),level=character(0),stringsAsFactors = FALSE)
  }
  links=getQueryTmpEmuDBs()[['queryLinksExt']]
  
  lblsDf=getQueryTmpEmuDBs()[['queryLabels']]
  itemsIdxSql='CREATE INDEX items_idx ON items(itemID,db_uuid,session,bundle,level,itemID,seqIdx,type,sampleRate,sampleStart,sampleDur,samplePoint)'
  resIdxSql='CREATE INDEX its_idx ON its(db_uuid,session,bundle,seqStartId,seqEndId,seqLen,level)'
  
  #labelsIdxSql='CREATE INDEX labels_idx ON lblsDf(itemID,name)'
  labelsIdxSql='CREATE INDEX labels_idx ON lblsDf(itemID,db_uuid,session,bundle,name)'
  # get max length
  #itemsIdxSql='CREATE INDEX items_idx ON items(seqLen)'
  if(itCount>0){
    maxSeqLenDf=sqldf(c(resIdxSql,"SELECT max(seqLen) AS maxSeqLen FROM its"))
    maxSeqLen=maxSeqLenDf[1,'maxSeqLen']
    
  }else{
    maxSeqLen=1L
  }
  
  
  # query seglist data except labels
  # for this data the information in start end item of the sequence is sufficient
  # it takes only the start  and end items of the query result in account
  # the CASE WHEN THEN ELSE END terms are necessary to get the start and end samples of sequences which are not segment levels and therefore have no time information  
  hasLinks=(nrow(links)>0)
  
  
  # check for ambigious time information (multiple SEGMENT levels)
  # TODO   
  #if(hasLinks){
  #  # items of type ITEM have no (sample) time information
  #  # therefore we search for linked SEGMENT items and take their start sample position
  #  qStr=paste0("SELECT count(l.fromID), FROM links l,items s,items e,its r WHERE s.db_uuid=s.db_uuid AND s.session=l.session AND s.bundle=l.bundle AND s.itemID=l.fromID  AND l.toSeqIdx=0 AND e.db_uuid=s.db_uuid AND e.session=s.session AND e.bundle=s.bundle AND r.db_uuid=s.db_uuid AND r.session=s.session AND r.bundle=s.bundle AND s.itemID=r.seqStartId AND e.itemID=r.seqEndId AND e.level=s.level")
  #  print(sqldf(qStr))
  #}
  
  
  
  
  
  # select columns: id,session,bundle,startItemId,endItemID ,type ...
  selectStr="SELECT s.db_uuid ,s.session,s.bundle,s.itemID AS startItemID ,e.itemID AS endItemID,s.type, "
  
  # find sequence start position
  # use sample start of sequence start item for type SEGMENT and samplePoint for type EVENT 
  selectStr=paste0(selectStr,"CASE s.type \
                   WHEN 'SEGMENT' THEN s.sampleStart \
                   WHEN 'EVENT' THEN s.samplePoint ")
  # calculate start sample for type ITEM
  if(hasLinks){
    # items of type ITEM have no (sample) time information
    # therefore we search for linked SEGMENT items and take their start sample position
    selectStr=paste0(selectStr," ELSE (SELECT i.sampleStart FROM items i WHERE i.db_uuid=s.db_uuid AND i.session=s.session AND i.bundle=s.bundle AND i.type='SEGMENT' AND ")
    if(!is.null(timeRefSegmentLevel)){
      selectStr=paste0(selectStr," i.level='",timeRefSegmentLevel,"' AND ")
    }
    selectStr=paste0(selectStr," EXISTS (SELECT * FROM links l WHERE s.db_uuid=s.db_uuid AND s.session=l.session AND s.bundle=l.bundle AND s.itemID=l.fromID AND i.db_uuid=l.db_uuid AND i.session=l.session AND i.bundle=l.bundle AND i.itemID=l.toID AND l.toSeqIdx=0)) ")
  }else{
    # TODO
    # No sample start information. (throw error ?)
  }
  selectStr=paste0(selectStr," END AS sampleStart, ")
  
  # find sequence end position
  # use sample start plus sample duration of sequence end item for type SEGMENT and zero for type EVENT
  # TODO is zero correct here ?? should it be e.samplePoint instead ??
  selectStr=paste0(selectStr," CASE s.type \
                   WHEN 'SEGMENT' THEN (e.sampleStart+e.sampleDur) \
                   WHEN 'EVENT' THEN NULL ")  
  if(hasLinks){
    # items of type ITEM have no (sample) time information
    # therefore we search for linked SEGMENT items and take their end sample position
    
    selectStr=paste0(selectStr," ELSE (SELECT i.sampleStart+i.sampleDur FROM items i WHERE i.db_uuid=e.db_uuid AND i.session=e.session AND i.bundle=e.bundle AND i.type='SEGMENT' AND ")
    if(!is.null(timeRefSegmentLevel)){
      selectStr=paste0(selectStr," i.level='",timeRefSegmentLevel,"' AND ")
    }
    selectStr=paste0(selectStr," EXISTS (SELECT * FROM links l WHERE e.db_uuid=l.db_uuid AND e.session=l.session AND e.bundle=l.bundle AND e.itemID=l.fromID AND i.db_uuid=l.db_uuid AND i.session=l.session AND i.bundle=l.bundle AND i.itemID=l.toID AND l.toSeqIdx+1=l.toSeqLen)) ")
    
  }
  selectStr=paste0(selectStr,"END AS sampleEnd, ")
  
  # find samplerate
  # use sample rate of sequence start item for type SEGMENT and EVENT 
  selectStr=paste0(selectStr,"CASE s.type \
                   WHEN 'SEGMENT' THEN s.sampleRate \
                   WHEN 'EVENT' THEN s.sampleRate ")
  if(hasLinks){
    # items of type ITEM have no sample rate information
    # therefore we search for linked SEGMENT items and take their start sample position
    # TODO Can we use EVENT items as well ?
    selectStr=paste0(selectStr," ELSE (SELECT i.sampleRate FROM items i WHERE i.db_uuid=s.db_uuid AND i.session=s.session AND i.bundle=s.bundle AND i.type='SEGMENT' AND ")
    if(!is.null(timeRefSegmentLevel)){
      selectStr=paste0(selectStr," i.level='",timeRefSegmentLevel,"' AND ")
    }
    selectStr=paste0(selectStr," EXISTS (SELECT * FROM links l WHERE s.itemID=l.fromID AND i.itemID=l.toID AND i.db_uuid=l.db_uuid AND i.session=l.session AND i.bundle=l.bundle AND l.toSeqIdx=0)) ")
    
  }else{
    # TODO no samplerate , error ??
  }
  
  selectStr=paste0(selectStr," END AS sampleRate, ")
  
  # from clause
  fromStr="FROM items s,items e,its r, "
  
  # where clause: make sure start and end are in same emuDB, session and bundle, select start and end id
  whereStr="WHERE e.db_uuid=s.db_uuid AND e.session=s.session AND e.bundle=s.bundle AND r.db_uuid=s.db_uuid AND r.session=s.session AND r.bundle=s.bundle AND s.itemID=r.seqStartId AND e.itemID=r.seqEndId AND e.level=s.level AND "
  
  # order
  #orderStr="ORDER BY s.db_uuid,s.session,s.bundle,startItemID,endItemID"
  orderStr=''
  
  # append terms depending on maximum sequence length
  # build query for label sequence string
  # TODO assume equal seq len for each sequence for now!!
  # this is not the case for queries like emu.query("andosl","*","[#[Phonetic=t -> Phonetic=S] -> #Phonetic=I]")
  # which are not allowed by BNF and in emuR but in fact they are working with Emu 2.3 and emuR requery may produce such results
  # result would be a mix with t->S and I items (sequence lengths 2 and 1)
  for(seqIdx in 1:maxSeqLen){
    selectStr=paste0(selectStr,'(SELECT l.label FROM lblsDf l WHERE l.db_uuid=i',seqIdx,'.db_uuid AND l.session=i',seqIdx,'.session AND l.bundle=i',seqIdx,'.bundle AND   l.itemID=i',seqIdx,".itemID AND l.name=r.level)")
    
    #selectLblStr=paste0(selectLblStr,)
    fromStr=paste0(fromStr,'items i',seqIdx)
    offset=seqIdx-1
    whereStr=paste0(whereStr,'i',seqIdx,'.db_uuid=s.db_uuid AND i',seqIdx,'.session=s.session AND i',seqIdx,'.bundle=s.bundle AND i',seqIdx,'.level=s.level AND i',seqIdx,'.seqIdx=s.seqIdx+',offset)
    if(seqIdx<maxSeqLen){
      #selectStrL=paste0(selectStrL,',')
      selectStr=paste0(selectStr," || '->' || ")
      fromStr=paste0(fromStr,',')
      whereStr=paste0(whereStr,' AND ')
    }
  }
  selectStr=paste0(selectStr," AS labels ")
  
  
  queryStr=paste(selectStr,fromStr,whereStr,orderStr,sep = ' ')
  
  # build indices
  #itemsIdxSql='CREATE INDEX items_idx ON items(id,session,bundle,level,itemID,seqIdx,type)' # very slow !!
  itemsIdxSql='CREATE INDEX items_idx ON items(type,db_uuid,session,bundle,level,itemID,seqIdx)'
  linksIdxSql='CREATE INDEX links_idx ON links(db_uuid,session,bundle,fromID,toID,toSeqIdx,toSeqLen)'
  if(hasLinks){
    #st=system.time((segListData=sqldf(c(itemsIdxSql,linksIdxSql,resIdxSql,q))))
    #cat(st," (with links)\n")
    segListData=sqldf(c(itemsIdxSql,linksIdxSql,resIdxSql,labelsIdxSql,queryStr))
  }else{
    #st=system.time((segListData=sqldf(c(itemsIdxSql,resIdxSql,q))))
    #cat(st," (without links)\n")
    segListData=sqldf(c(itemsIdxSql,resIdxSql,labelsIdxSql,queryStr))
  }
  # Note: CASE s.type WHEN 'SEGMENT' OR 'EVENT' did not work.
  
  #print(segListData)
  
  # convert samples to milliseconds using SQL:
  seglist=sqldf("SELECT \
                       labels,
                       CASE type WHEN 'EVENT' THEN \
                        CAST (sampleStart AS REAL)/ CAST( sampleRate AS REAL) * 1000.0 \
                       ELSE \
                        (CAST (sampleStart AS REAL) + 0.5 ) / CAST( sampleRate AS REAL) * 1000.0 \
                       END AS start, \
                       CASE type WHEN 'EVENT' THEN \
                         0.0
                       ELSE \
                        (CAST (sampleEnd AS REAL) + 1.5 ) / CAST( sampleRate AS REAL) * 1000.0 \
                       END AS end, \
                       session || ':' || bundle AS utts, \
                       db_uuid,session,bundle,startItemID,endItemID,type,sampleStart,sampleEnd,sampleRate \
                      FROM segListData")
  # set emusegs type attribute, default 'segment'
  slType='segment'
  if(nrow(seglist)>0){
    # set to event only if all rows are of type EVENT
    dTypes=sqldf("SELECT DISTINCT type FROM seglist")
    if(nrow(dTypes)==1){
      if(dTypes[1,1]=='EVENT'){
        slType='event'
      }
    }
  }
  segmentList=make.emuRsegs(dbName = dbConfig[['name']],seglist = seglist,query = result[['queryStr']],type = slType)
  return(segmentList)
}

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