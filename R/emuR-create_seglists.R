
convert_queryResultToEmusegs<-function(emuDBhandle){
  emuRsegs = convert_queryResultToEmuRsegs(emuDBhandle)
  emusegs = as.emusegs(emuRsegs)
  return(emusegs)
}


convert_queryResultToEmuRsegs <- function(emuDBhandle, timeRefSegmentLevel=NULL){
  itemsTableName = "items_filtered_tmp"
  labelsTableName = "labels_filtered_tmp"
  linksExtTableName = "links_ext_filtered_tmp"
  
  bundles=c()
  labels=c()
  start=c()
  end=c()
  slType=NULL
  projectionItemsN = DBI::dbGetQuery(emuDBhandle$connection, paste0("SELECT COUNT(*) AS n FROM interm_res_proj_items_tmp_root"))$n
  if(projectionItemsN > 0){ 
    # use projection items
    itsTableName = "interm_res_proj_items_tmp_root"
    seqStartIdColName = "pSeqStartId"
    seqEndIdColName = "pSeqEndId"
    seqLenColName = "pSeqLen"
    levelColName = "pLevel"
  }else{
    # use "normal" items
    itsTableName = "interm_res_items_tmp_root"
    seqStartIdColName = "seqStartId"
    seqEndIdColName = "seqEndId"
    seqLenColName = "seqLen"
    levelColName = "level"
  }
  
  # get distinct result levels
  distinctLevels = DBI::dbGetQuery(emuDBhandle$connection, paste0("SELECT DISTINCT ", levelColName, " FROM ", itsTableName))
  for(attrNm in distinctLevels[,levelColName]){
    
    lvlNm = get_levelNameForAttributeName(emuDBhandle, attributeName = attrNm)
    ld = get_levelDefinition(emuDBhandle, name = lvlNm)

    if(ld['type']=='ITEM'){
      segLvlNms = find_segmentLevels(emuDBhandle, attrNm)
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
  
  itCount = DBI::dbGetQuery(emuDBhandle$connection, paste0("SELECT COUNT(*) AS n FROM ", itsTableName))$n
  if(itCount > 0){
    maxSeqLen = DBI::dbGetQuery(emuDBhandle$connection, paste0("SELECT max(", seqLenColName, ") AS maxSeqLen FROM ", itsTableName))$maxSeqLen
  }else{
    maxSeqLen=1L
  }
  
  # query seglist data except labels
  # for this data the information in start end item of the sequence is sufficient
  # it takes only the start and end items of the query result into account
  # the CASE WHEN THEN ELSE END terms are necessary to get the start and end samples of sequences which are not segment levels and therefore have no time information
  linksExtFilteredCount = DBI::dbGetQuery(emuDBhandle$connection, paste0("SELECT COUNT(*) AS n FROM links_ext_filtered_tmp"))$n
  hasLinks = (linksExtFilteredCount > 0)

  # select columns: id,session,bundle,startItemId,endItemID ,type ...
  selectStr=paste0("SELECT s.db_uuid ,s.session,s.bundle,s.itemID AS startItemID ,e.itemID AS endItemID,r.", levelColName, ",s.type, ")
  
  # find sequence start position
  # use sample start of sequence start item for type SEGMENT and samplePoint for type EVENT 
  selectStr=paste0(selectStr,"CASE s.type \
                   WHEN 'SEGMENT' THEN s.sampleStart \
                   WHEN 'EVENT' THEN s.samplePoint ")
  # calculate start sample for type ITEM
  if(hasLinks){
    # items of type ITEM have no (sample) time information
    # therefore we search for linked SEGMENT items and take their start sample position
    selectStr=paste0(selectStr," ELSE (SELECT i.sampleStart FROM ", itemsTableName, " i WHERE i.db_uuid=s.db_uuid AND i.session=s.session AND i.bundle=s.bundle AND i.type='SEGMENT' AND ")
    if(!is.null(timeRefSegmentLevel)){
      selectStr=paste0(selectStr," i.level='",timeRefSegmentLevel,"' AND ")
    }
    selectStr=paste0(selectStr," EXISTS (SELECT * FROM ", linksExtTableName, " l WHERE s.db_uuid=s.db_uuid AND s.session=l.session AND s.bundle=l.bundle AND s.itemID=l.fromID AND i.db_uuid=l.db_uuid AND i.session=l.session AND i.bundle=l.bundle AND i.itemID=l.toID AND l.toSeqIdx=0)) ")
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
    selectStr=paste0(selectStr," ELSE (SELECT i.sampleStart+i.sampleDur FROM ", itemsTableName, " i WHERE i.db_uuid=e.db_uuid AND i.session=e.session AND i.bundle=e.bundle AND i.type='SEGMENT' AND ")
    if(!is.null(timeRefSegmentLevel)){
      selectStr=paste0(selectStr," i.level='",timeRefSegmentLevel,"' AND ")
    }
    selectStr=paste0(selectStr," EXISTS (SELECT * FROM ", linksExtTableName, " l WHERE e.db_uuid=l.db_uuid AND e.session=l.session AND e.bundle=l.bundle AND e.itemID=l.fromID AND i.db_uuid=l.db_uuid AND i.session=l.session AND i.bundle=l.bundle AND i.itemID=l.toID AND l.toSeqIdx+1=l.toSeqLen)) ")
    
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
    selectStr=paste0(selectStr," ELSE (SELECT i.sampleRate FROM ", itemsTableName, " i WHERE i.db_uuid=s.db_uuid AND i.session=s.session AND i.bundle=s.bundle AND i.type='SEGMENT' AND ")
    if(!is.null(timeRefSegmentLevel)){
      selectStr=paste0(selectStr," i.level='",timeRefSegmentLevel,"' AND ")
    }
    selectStr=paste0(selectStr," EXISTS (SELECT * FROM ", linksExtTableName, " l WHERE s.itemID=l.fromID AND i.itemID=l.toID AND i.db_uuid=l.db_uuid AND i.session=l.session AND i.bundle=l.bundle AND l.toSeqIdx=0)) ")
    
  }else{
    # TODO no samplerate , error ??
  }
  
  selectStr=paste0(selectStr," END AS sampleRate, ")
  
  # from clause
  fromStr=paste0("FROM ", itemsTableName, " s,", itemsTableName, " e,", itsTableName, " r, ")
  
  # where clause: make sure start and end are in same emuDB, session and bundle, select start and end id
  whereStr=paste0("WHERE e.db_uuid=s.db_uuid AND e.session=s.session AND e.bundle=s.bundle AND r.db_uuid=s.db_uuid AND r.session=s.session AND r.bundle=s.bundle AND s.itemID=", seqStartIdColName, " AND e.itemID=", seqEndIdColName ," AND e.level=s.level AND ")
  
  # order
  orderStr="ORDER BY s.db_uuid,s.session,s.bundle,startItemID,endItemID"
  
  # append terms depending on maximum sequence length
  # build query for label sequence string
  # TODO assume equal seq len for each sequence for now!!
  # this is not the case for queries like emu.query("andosl","*","[#[Phonetic=t -> Phonetic=S] -> #Phonetic=I]")
  # which are not allowed by BNF and in emuR but in fact they are working with Emu 2.3 and emuR requery may produce such results
  # result would be a mix with t->S and I items (sequence lengths 2 and 1)
  for(seqIdx in 1:maxSeqLen){
    selectStr=paste0(selectStr, "(SELECT l.label FROM ", labelsTableName, " l WHERE l.db_uuid=i", seqIdx, ".db_uuid AND l.session=i", seqIdx, ".session AND l.bundle=i", seqIdx, ".bundle AND l.itemID=i", seqIdx, ".itemID AND l.name=r.", levelColName, ")")
    
    fromStr=paste0(fromStr, itemsTableName, " i",seqIdx)
    offset=seqIdx-1
    whereStr=paste0(whereStr, "i", seqIdx, ".db_uuid=s.db_uuid AND i", seqIdx, ".session=s.session AND i", seqIdx, ".bundle=s.bundle AND i", seqIdx ,".level=s.level AND i", seqIdx, ".seqIdx=s.seqIdx+", offset)
    if(seqIdx<maxSeqLen){
      selectStr=paste0(selectStr," || '->' || ")
      fromStr=paste0(fromStr,',')
      whereStr=paste0(whereStr,' AND ')
    }
  }
  selectStr=paste0(selectStr," AS labels ")

  queryStr=paste(selectStr,fromStr,whereStr,orderStr,sep = ' ')
  
  # convert samples to milliseconds SQL string:
  queryStrInclConvert = paste0("SELECT labels,
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
                       db_uuid,session,bundle,startItemID,endItemID,", levelColName, " AS level,type,sampleStart,sampleEnd,sampleRate \
                      FROM (", queryStr, ") ORDER BY db_uuid,session,bundle,sampleStart")
  
  seglist = DBI::dbGetQuery(emuDBhandle$connection, queryStrInclConvert)
  
  # set emusegs type attribute, default 'segment'
  slType='segment'
  if(nrow(seglist)>0){
    # set to event only if all rows are of type EVENT
    if(all(seglist$type == "EVENT")){
        slType='event'
    }
  }
  queryStr = DBI::dbGetQuery(emuDBhandle$connection, "SELECT queryStr FROM interm_res_meta_infos_tmp_root")$queryStr
  segmentList=make.emuRsegs(dbName = emuDBhandle$dbName, seglist = seglist, query = queryStr, type = slType)
  return(segmentList)
}


# convert to emuRsegs segemnt list, variable sequenec length of input allowed
convert_queryResultToVariableEmuRsegs <- function(emuDBhandle, timeRefSegmentLevel=NULL){
  its=NULL
  
  bundles=c()
  labels=c()
  start=c()
  end=c()
  slType=NULL
  
  itsTableName = "interm_res_items_tmp_root"
  
  # get distinct result levels
  distinctLevels=DBI::dbGetQuery(emuDBhandle$connection, paste0("SELECT DISTINCT level FROM ", itsTableName))
  for(attrNm in distinctLevels[,'level']){
    lvlNm=get_levelNameForAttributeName(emuDBhandle,attributeName = attrNm)
    ld=get_levelDefinition(emuDBhandle, name = lvlNm)

    if(ld['type']=='ITEM'){
      segLvlNms=find_segmentLevels(emuDBhandle,attrNm)
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
  
  
  # itCount=nrow(its)
  itCount = DBI::dbGetQuery(emuDBhandle$connection, paste0("SELECT COUNT(*) AS N FROM ", itsTableName))$N
  if(itCount==0){
    its=data.frame(db_uuid=character(0),session=character(0),bundle=character(0),seqStartId=integer(0),seqEndId=integer(0),seqLen=integer(0),level=character(0),stringsAsFactors = FALSE)
  }

  if(itCount>0){
    maxSeqLenDf=DBI::dbGetQuery(emuDBhandle$connection, paste0("SELECT max(seqLen) AS maxSeqLen FROM ", itsTableName))
    maxSeqLen=maxSeqLenDf[1,'maxSeqLen']
    
    # for string conacatenation: we need all occuring seq lengths 
    # distinct sequence lengths
    # distinctSeqLens=sqldf(c(resIdxSql,"SELECT DISTINCT seqLen FROM its"))
    distinctSeqLens = DBI::dbGetQuery(emuDBhandle$connection, paste0("SELECT DISTINCT seqLen FROM ", itsTableName))
  }else{
    maxSeqLen=1L
  }
  
  
  # query seglist data except labels
  # for this data the information in start end item of the sequence is sufficient
  # it takes only the start  and end items of the query result in account
  # the CASE WHEN THEN ELSE END terms are necessary to get the start and end samples of sequences which are not segment levels and therefore have no time information  
  hasLinks = DBI::dbGetQuery(emuDBhandle$connection, paste0("SELECT COUNT(*) AS n FROM links_ext"))$n > 0
  

  # select columns: id,session,bundle,startItemId,endItemID ,type ...
  selectStr=paste0("SELECT s.db_uuid ,s.session,s.bundle,s.itemID AS startItemID ,e.itemID AS endItemID, r.level, s.type, ")
  
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
    selectStr=paste0(selectStr," EXISTS (SELECT * FROM links_ext l WHERE s.db_uuid=s.db_uuid AND s.session=l.session AND s.bundle=l.bundle AND s.itemID=l.fromID AND i.db_uuid=l.db_uuid AND i.session=l.session AND i.bundle=l.bundle AND i.itemID=l.toID AND l.toSeqIdx=0)) ")
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
    selectStr=paste0(selectStr," EXISTS (SELECT * FROM links_ext l WHERE e.db_uuid=l.db_uuid AND e.session=l.session AND e.bundle=l.bundle AND e.itemID=l.fromID AND i.db_uuid=l.db_uuid AND i.session=l.session AND i.bundle=l.bundle AND i.itemID=l.toID AND l.toSeqIdx+1=l.toSeqLen)) ")
    
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
    selectStr=paste0(selectStr," EXISTS (SELECT * FROM links_ext l WHERE s.itemID=l.fromID AND i.itemID=l.toID AND i.db_uuid=l.db_uuid AND i.session=l.session AND i.bundle=l.bundle AND l.toSeqIdx=0)) ")
    
  }else{
    # TODO no samplerate , error ??
  }
  
  selectStr=paste0(selectStr," END AS sampleRate, ")
  
  # from clause
  fromStr=paste0("FROM items s,items e,", itsTableName, " r ")
  
  # where clause: make sure start and end are in same emuDB, session and bundle, select start and end id
  whereStr=paste0("WHERE e.db_uuid=s.db_uuid AND e.session=s.session AND e.bundle=s.bundle AND r.db_uuid=s.db_uuid AND r.session=s.session AND r.bundle=s.bundle AND s.itemID=r.seqStartId AND e.itemID=r.seqEndId AND e.level=s.level ")
  
  # order
  orderStr="ORDER BY s.db_uuid,s.session,s.bundle,startItemID,endItemID"
  
  if(itCount>0){
    selectStr=paste0(selectStr," CASE r.seqLen ")
    distinctSeqLensLength=nrow(distinctSeqLens)
    # for each seq len, which occurs in the input segment list
    for(si in 1:distinctSeqLensLength){
      seqLen=distinctSeqLens[si,1]
      # append terms depending on maximum sequence length
      # build query for label sequence string
      # emuR hierachical requery produces results with differnet seq lengths per row
      selectStr=paste0(selectStr,"WHEN ",seqLen," THEN ")
      for(seqIdx in 1:seqLen){
        selectStr=paste0(selectStr,'(SELECT l.label FROM labels l,items i WHERE l.db_uuid=i.db_uuid AND l.session=i.session AND l.bundle=i.bundle AND l.itemID=i.itemID AND l.name=r.level AND ')
        
        offset=seqIdx-1
        selectStr=paste0(selectStr,'i.db_uuid=s.db_uuid AND i.session=s.session AND i.bundle=s.bundle AND i.level=s.level AND i.seqIdx=s.seqIdx+',offset,")")
        if(seqIdx<seqLen){
          selectStr=paste0(selectStr," || '->' || ")
        }
      }
    }
    selectStr=paste0(selectStr," END AS labels ")
  }else{
    selectStr=paste0(selectStr," '' AS labels ")
  }
  queryStr=paste(selectStr,fromStr,whereStr,orderStr,sep = ' ')
  
  # convert samples to milliseconds using SQL:
  seglist=DBI::dbGetQuery(emuDBhandle$connection, paste0("SELECT \
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
                       db_uuid,session,bundle,startItemID,endItemID,level,type,sampleStart,sampleEnd,sampleRate \
                      FROM (", queryStr, ")"))
  
  # set emusegs type attribute, default 'segment'
  slType='segment'
  if(nrow(seglist)>0){
    # set to event only if all rows are of type EVENT
    dTypes=unique(seglist$level)
    if(length(dTypes)==1){
      if(dTypes[1]=='EVENT'){
        slType='event'
      }
    }
  }
  segmentList=make.emuRsegs(dbName = emuDBhandle$dbName, seglist = seglist,query = "FROM REQUERY", type = slType)
  return(segmentList)
}

##################################
##################################
##################################
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