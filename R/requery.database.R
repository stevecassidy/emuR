require(sqldf)
require(stringr)

##' Requery sequential context of segment list in an emuDB
##' @description Function to requery sequential context of a segment list queried from an emuDB
##' @details Builds a new segment list on the same hierarchical level and the same length as the segment list given in \code{seglist}. The resulting segments usually have different start position and length (in terms of items of the respective level) controlled by the \code{offset},\code{offsetRef} and \code{length} parameters.
##' A segment here is defined as a single item or a chain of items from the respective level, e.g. if a level in a bundle instance has labels 'a', 'b' and 'c' in that order, 'a' or 'a->b' oder 'a->b->c' are all valid segments, but not 'a->c'.
##' \code{offsetRef} determines if the position offset is referenced to the start or the end item of the segments in the input list \code{seglist}; parameter \code{offset} determines the offset of the resulting item start position to this reference item;
##' parameter \code{length} sets the item length of the result segments.
##' If the requested segments are out of bundle item boundaries and parameter \code{ignoreOutOfBounds} is \code{FALSE} (the default), an error is generated. To get residual resulting segments that lie within the bounds the \code{ignoreOutOfBounds} parameter can be set to \code{TRUE}.
##' The returned segment list is usually of the same length and order as the input \code{seglist}; if \code{ignoreOutOfBounds=FALSE}, the resulting segment list may be out of sync.
##' @param seglist segment list to requery on (type: 'emuRsegs')
##' @param offset start item offset in sequence (default is 0, meaning the start or end item of the input segment)
##' @param offsetRef reference item for offset: 'START' for first and 'END' for last item of segment
##' @param length item length of segments in the returned segment list
##' @param ignoreOutOfBounds ignore result segments that are out of bundle bounds
##' @param dbUUID optional UUID of emuDB, if the emuDB name in the input segment list is not unique
##' @return result set object of class 'emuRsegs' containing the requeried segments
##' @author Klaus Jaensch
##' @import sqldf
##' @export
##' @seealso \code{\link{query}} \code{\link{requery_hier}} \code{\link{emuRsegs}}
##' @keywords emuDB database requery
##' @examples
##' \dontrun{
##' 
##' ## Requery previous item of 'p' on level 'Phonetic'
##' sl1=query('ae','Phonetic=p')
##' 
##' requery_seq(sl1,offset=-1)
##' 
##' ## Requery context (adding previuos and following elements) of 'p' on phonetic level
##'
##' requery_seq(sl1,offset=-1,length=3)
##' 
##' ## Requery previous item of n->t sequence
##' sl2=query('ae',"[Phoneme=n -> Phoneme=t]")
##' 
##' requery_seq(sl2,offset=-1)
##' 
##' ## Requery last item within n->t sequence
##' 
##' requery_seq(sl2,offsetRef='END')
##' 
##' ## Requery following item after n->t sequence
##' 
##' requery_seq(sl2,offset=1,offsetRef='END')
##' 
##' ## Requery context (previous and following items) of n->t sequence
##' 
##' requery_seq(sl2,offset=-1,length=4)
##' 
##' ## Requery next word contexts (sequence includes target word)
##' 
##' sl3=query('ae',"Text=to")
##' requery_seq(sl3,length=2)
##' 
##' ## Requery following two word contexts, ignoring segment 
##' ## sequences that are out of bundle end bounds 
##' requery_seq(sl3,length=3,ignoreOutOfBounds=TRUE)
##' 
##' }
requery_seq<-function(seglist, offset=0,offsetRef='START',length=1,ignoreOutOfBounds=FALSE,dbUUID=NULL){
  if(!inherits(seglist,"emuRsegs")){
    stop("Segment list 'seglist' must be of type 'emuRsegs'. (Do not set a value for 'resultType' parameter in the query() command; then the default resultType=emuRsegs will be used)")
  }
  if(length<=0){
    stop("Parameter length must be greater than 0")
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
    
    items=dbReadTable(get_emuDBcon(dbUUID),'items')
    itemsIdxSql='CREATE INDEX items_idx ON items(itemID,db_uuid,session,bundle,level,itemID,seqIdx,type,sampleRate,sampleStart,sampleDur,samplePoint)'
    resIdxSql='CREATE INDEX its_idx ON its(db_uuid,session,bundle,seqStartId,seqEndId,seqLen,level)'
    #   
    #labelsIdxSql='CREATE INDEX labels_idx ON lblsDf(itemID,name)'
    labelsIdxSql='CREATE INDEX labels_idx ON lblsDf(itemID,db_uuid,session,bundle,name)'
    
    # check if out of boundaries
    
    
    # query for sequential requeries
    heQueryStr=paste0("SELECT il.db_uuid,il.session,il.bundle,il.itemID AS seqStartId,ir.itemID AS seqEndID,",length," AS seqLen,sl.level FROM seglist sl,items sll,items slr,items il, items ir \
                        WHERE \
                         il.db_uuid=ir.db_uuid AND il.session=ir.session AND il.bundle=ir.bundle AND \
                         il.db_uuid=sl.db_uuid AND il.session=sl.session AND il.bundle=sl.bundle AND \
                         sll.db_uuid=sl.db_uuid AND sll.session=sl.session AND sll.bundle=sl.bundle AND sl.startItemID=sll.itemID AND \
                         slr.db_uuid=sl.db_uuid AND slr.session=sl.session AND slr.bundle=sl.bundle AND sl.endItemID=slr.itemID AND ")
    if(offsetRef=='START'){
      heQueryStr=paste0(heQueryStr,"il.level=sll.level AND il.seqIdx=sll.seqIdx+",offset," AND \
                          ir.level=sll.level AND ir.seqIdx=sll.seqIdx+",offset+length-1)
    }else if(offsetRef=='END'){
      heQueryStr=paste0(heQueryStr,"il.level=slr.level AND il.seqIdx=slr.seqIdx+",offset," AND \
                          ir.level=slr.level AND ir.seqIdx=slr.seqIdx+",offset+length-1)
    }else{
      stop("Parameter offsetRef must be one of 'START' or 'END'\n")
    }
    heQueryStr=paste0(heQueryStr," ORDER BY il.ROWID");
    he=sqldf(heQueryStr)
    slLen=nrow(seglist)
    resLen=nrow(he)
    outOfBndCnt=slLen-resLen
    if(!ignoreOutOfBounds & outOfBndCnt>0){
      if(outOfBndCnt==slLen){
        stop("All (",outOfBndCnt,") of the requested sequence(s) is/are out of boundaries.")
      }else{
        stop(outOfBndCnt," of the requested sequence(s) is/are out of boundaries.\nSet parameter 'ignoreOutOfBounds=TRUE' to get residual result segments that lie within the bounds.")
      }
    }
    result=list(items=he)
    emuDBs.query.tmp<-list()
    emuDBs.query.tmp[['queryItems']]<-dbGetQuery(get_emuDBcon(dbConfig$UUID),paste0("SELECT * FROM items WHERE db_uuid='",dbUUID,"'"))
    emuDBs.query.tmp[['queryLabels']]<-dbGetQuery(get_emuDBcon(dbConfig$UUID),paste0("SELECT * FROM labels WHERE db_uuid='",dbUUID,"'"))
    emuDBs.query.tmp[['queryLinksExt']]<-dbGetQuery(get_emuDBcon(dbConfig$UUID),paste0("SELECT * FROM linksExt WHERE db_uuid='",dbUUID,"'"))
    setQueryTmpEmuDBs(emuDBs.query.tmp)
    trSl=convert.query.result.to.segmentlist.var(dbConfig = dbConfig,result=result)
    # free temp tables
    setQueryTmpEmuDBs(NULL)
    return(trSl)
  }
}

##' Requery hierarchical context of a segment list in an emuDB
##' @description Function to requery hierarchical context of a segment list queried from an emuDB
##' @details A segment is defined as a single item or a chain of items from the respective level, e.g. if a level in a bundle instance has labels 'a', 'b' and 'c' in that order, 'a' or 'a->b' oder 'a->b->c' are all valid segments, but not 'a->c'.
##' For each segment of the input segment list \code{seglist} the function checks the start and end item for hierarchically linked items in the given target level, and based on them constructs segments in the target level.
##' As the start item in the resulting segment the item with the lowest sample position is chosen; for the end item that with the highest sample position.
##' If result and input segment list have the same length (for each input segment one segment on the target level was found), the result segment list has the same length and order as the input list; 
##' in 'upwards' requeries this can cause a resulting segment list to contain two (or more) copies of the same segment, if the same item from the input list was linked twice or more to an item of the target level, e.g. a phoneme 'p' requeried to the word level might result in two identical segments 'Papa' in the result list. 
##' If the length of input and output list differ (e.g. because a link is missing in the emuDB), a synchronous ordering is not possible and therefore a warning is generated.
##' 
##' @param seglist segment list to requery on (type: \link{emuRsegs})
##' @param level character string: name of target level 
##' @param dbUUID optional UUID of emuDB, if the emuDB name in the input segment list is not unique
##' @return result set object of class \link{emuRsegs}
##' @author Klaus Jaensch
##' @import sqldf
##' @export
##' @seealso \code{\link{query}} \code{\link{requery_seq}} \code{\link{emuRsegs}}
##' @keywords emuDB database requery
##' @examples
##' \dontrun{
##' 
##' ## Downward requery: find 'Phoneme' sequences of all words 'beautiful' (of level 'Text')
##' ## Note that the resulting segments consists of phoneme sequences and have therefore 
##' ## the same length as the word segments.
##'
##' sl1=query('ae','Text=beautiful')
##' requery_hier(sl1,level='Phoneme')
##'
##' ## Upward requery: find all word segments that dominate a 'p' on level 'Phoneme'
##' ## Note that the resulting segments are larger than the input segments,
##' ## because they contain the complete words.
##' 
##' sl1=query('ae','Phonetic==p')
##' wl1=requery_hier(sl1,level='Text')
##' wl1
##' 
##' ## Why is there a 'p' the word 'emphazised'? Requery the whole words back down to 'Phoneme' level:
##'
##' requery_hier(wl1,level='Phoneme')
##'
##' ## ... because of 'stop epenthesis' a 'p' is inserted between 'm' and 'f'
##' 
##' ## Combined requery: last phonemes of all words beginning with 'an'.
##' ## Note that we use a regular expression 'an.*' (EQL operator '=~') in the query.
##' 
##' sl1=query('ae',"Text=~an.*")
##' requery_seq(requery_hier(sl1,level='Phoneme'),offsetRef = 'END')
##' 
##' }
requery_hier<-function(seglist,level=NULL,dbUUID=NULL){
  if(!inherits(seglist,"emuRsegs")){
    stop("Segment list 'seglist' must be of type 'emuRsegs'. (Do not set a value for 'resultType' parameter for the query, the default resultType will be used)")
  }
  
  
#   # level requeries
#   if(is.null(level) & is.null(targetLevel)){
#     # no operation, return input seglist
#     return(seglist) 
#   }
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
    
    items=dbReadTable(get_emuDBcon(dbUUID),'items')
    itemsIdxSql='CREATE INDEX items_idx ON items(itemID,db_uuid,session,bundle,level,itemID,seqIdx,type,sampleRate,sampleStart,sampleDur,samplePoint)'
    resIdxSql='CREATE INDEX its_idx ON its(db_uuid,session,bundle,seqStartId,seqEndId,seqLen,level)'
    #   
    #labelsIdxSql='CREATE INDEX labels_idx ON lblsDf(itemID,name)'
    labelsIdxSql='CREATE INDEX labels_idx ON lblsDf(itemID,db_uuid,session,bundle,name)'
    
    
    
    
    linksExt=dbReadTable(get_emuDBcon(dbUUID),'linksExt')
    itemsIdxSql='CREATE INDEX items_idx ON items(itemID,db_uuid,session,bundle,itemID,level,seqIdx)'
    linksIdxSql='CREATE INDEX linksExt_idx ON linksExt(db_uuid,session,bundle,fromID,toID)'
    
    
    targetRootLevelName=NULL
    if(is.null(level)){
      heQueryStr=paste0("SELECT il.db_uuid,il.session,il.bundle,il.itemID AS seqStartId,ir.itemID AS seqEndId,ir.seqIdx-il.seqIdx+1 AS seqLen,il.level \
                          FROM \
                          ( SELECT ils.*,min(ils.seqIdx) FROM items ils,items slil,seglist sl WHERE \
                          ils.db_uuid=sl.db_uuid AND ils.session=sl.session AND ils.bundle=sl.bundle AND \
                          slil.db_uuid=sl.db_uuid AND slil.session=sl.session AND slil.bundle=sl.bundle AND \
                          slil.itemID=sl.startItemID AND ils.level=slil.level AND (\
                          (ils.itemID=sl.startItemID) OR 
                          (EXISTS (SELECT * FROM linksExt lr \
                          WHERE lr.db_uuid=sl.db_uuid AND lr.session=sl.session AND lr.bundle=sl.bundle \
                          AND ((lr.fromID=sl.startItemID AND lr.toID=ils.itemID) OR (lr.fromID=ils.itemID AND lr.toID= sl.startItemID))\
                          )) \
                          ) GROUP BY sl.ROWID ) \
                          AS il JOIN \
                          ( SELECT irs.*,max(irs.seqIdx) FROM items irs,items slir,seglist sl WHERE \
                          irs.db_uuid=sl.db_uuid AND irs.session=sl.session AND irs.bundle=sl.bundle AND \
                          slir.db_uuid=sl.db_uuid AND slir.session=sl.session AND slir.bundle=sl.bundle AND \
                          slir.itemID=sl.endItemID AND irs.level=slir.level AND (\
                          (irs.itemID=sl.endItemID) OR
                          (EXISTS (SELECT * FROM linksExt lr \
                          WHERE lr.db_uuid=sl.db_uuid AND lr.session=sl.session AND lr.bundle=sl.bundle \
                          AND ((lr.fromID=sl.endItemID AND lr.toID=irs.itemID) OR (lr.fromID=irs.itemID AND lr.toID= sl.endItemID))\
                          )) \
                          ) GROUP BY sl.ROWID ) \
                          AS ir ON il.ROWID=ir.ROWID
                          ")
      #levelSeglist=contextRequery(seglist = seglist,targetLevel = level)
      
    }else{
     
      check_level_attribute_name(dbConfig,level)
      targetRootLevelName=get.level.name.for.attribute(dbConfig = dbConfig,attributeName = level)
      
      leftQuery=paste0("SELECT ils.*,min(ils.seqIdx) FROM seglist sll,items ils WHERE \
                         ils.db_uuid=sll.db_uuid AND ils.session=sll.session AND ils.bundle=sll.bundle AND \
                         ils.level='",targetRootLevelName,"' AND (\
                         (ils.itemID=sll.startItemID) OR 
                         (EXISTS (SELECT * FROM linksExt ll \
                         WHERE ll.db_uuid=sll.db_uuid AND ll.session=sll.session AND ll.bundle=sll.bundle \
                         AND ((ll.fromID=sll.startItemID AND ll.toID=ils.itemID) OR (ll.fromID=ils.itemID AND ll.toID= sll.startItemID))\
                         )) \
                         ) GROUP BY sll.ROWID ORDER BY sll.ROWID")
      
      rightQuery=paste0("SELECT irs.itemID AS seqEndId,irs.seqIdx AS rSeqIdx,max(irs.seqIdx) FROM seglist slr,items irs WHERE \
                          irs.db_uuid=slr.db_uuid AND irs.session=slr.session AND irs.bundle=slr.bundle AND \
                          irs.level='",targetRootLevelName,"' AND (\
                          (irs.itemID=slr.endItemID) OR
                          (EXISTS (SELECT * FROM linksExt lr \
                          WHERE lr.db_uuid=slr.db_uuid AND lr.session=slr.session AND lr.bundle=slr.bundle \
                          AND ((lr.fromID=slr.endItemID AND lr.toID=irs.itemID) OR (lr.fromID=irs.itemID AND lr.toID= slr.endItemID))\
                          )) \
                          ) GROUP BY slr.ROWID ORDER BY slr.ROWID")
      
      # The following code produces strange error on this data:
      # 
      #        > fsl=query('ae',"Text=~a.* & Num(Text,Phoneme)=2")
      #        > fsl
      #        segment  list from database:  ae 
      #        query was:  Text=~a.* & Num(Text,Phoneme)=2 
      #        labels    start      end session   bundle startItemID endItemID type
      #        1    are  662.475  775.525    0000 msajc022          19        19 ITEM
      #        2    and 1421.975 1495.325    0000 msajc023          43        43 ITEM   
      #        
      #        > contextRequery(fsl,targetLevel='Phoneme')
      #        segment  list from database:  ae 
      #        query was:  
      #          labels    start      end session   bundle startItemID endItemID type
      #        1   @->r  662.475  775.525    0000 msajc022         110       103 ITEM
      #        2   @->n 1421.975 1495.325    0000 msajc023         102       103 ITEM
      #         
      #         endItemID of first row is wrong !! 103 should be 111
      #        works perfectly for some other queries and works if splitted into three spearete sqldf calls. SQLite bug ?? 
      #        
      #        > packageVersion('RSQLite')
      #        [1] '1.0.0'
      
      
      #       heQueryStr=paste0("SELECT il.db_uuid,il.session,il.bundle,il.itemID AS seqStartId,ir.seqEndId,(ir.rSeqIdx-il.seqIdx+1) AS seqLen,'",targetLevel,"' AS level \
      #                         FROM 
      #                         ( SELECT ils.*,min(ils.seqIdx) FROM seglist sll,items ils WHERE \
      #                         ils.db_uuid=sll.db_uuid AND ils.session=sll.session AND ils.bundle=sll.bundle AND \
      #                         ils.level='",targetRootLevelName,"' AND (\
      #                         (ils.itemID=sll.startItemID) OR 
      #                         (EXISTS (SELECT * FROM linksExt ll \
      #                         WHERE ll.db_uuid=sll.db_uuid AND ll.session=sll.session AND ll.bundle=sll.bundle \
      #                             AND ((ll.fromID=sll.startItemID AND ll.toID=ils.itemID) OR (ll.fromID=ils.itemID AND ll.toID= sll.startItemID))\
      #                             )) \
      #                         ) GROUP BY sll.ROWID ORDER BY sll.ROWID) \
      #                         AS il JOIN \
      #                         ( SELECT irs.itemID AS seqEndId,irs.seqIdx AS rSeqIdx,max(irs.seqIdx) FROM seglist slr,items irs WHERE \
      #                         irs.db_uuid=slr.db_uuid AND irs.session=slr.session AND irs.bundle=slr.bundle AND \
      #                         irs.level='",targetRootLevelName,"' AND (\
      #                         (irs.itemID=slr.endItemID) OR
      #                         (EXISTS (SELECT * FROM linksExt lr \
      #                         WHERE lr.db_uuid=slr.db_uuid AND lr.session=slr.session AND lr.bundle=slr.bundle \
      #                             AND ((lr.fromID=slr.endItemID AND lr.toID=irs.itemID) OR (lr.fromID=irs.itemID AND lr.toID= slr.endItemID))\
      #                           )) \
      #                         ) GROUP BY slr.ROWID ORDER BY slr.ROWID) \
      #                         AS ir ON il.ROWID=ir.ROWID ")
      
      lIts=sqldf(c(itemsIdxSql,linksIdxSql,leftQuery))
      rIts=sqldf(c(itemsIdxSql,linksIdxSql,rightQuery))
      heQueryStr=paste0("SELECT il.db_uuid,il.session,il.bundle,il.itemID AS seqStartId,ir.seqEndId,(ir.rSeqIdx-il.seqIdx+1) AS seqLen,'",level,"' AS level \
                         FROM lIts il JOIN rIts ir ON il.ROWID=ir.ROWID ORDER BY il.ROWID")
    }
    
    he=sqldf(c(itemsIdxSql,linksIdxSql,heQueryStr))
    result=list(items=he)
    dbUUID=get_emuDB_UUID(dbName,dbUUID)
    db=.load.emuDB.DBI(uuid = dbUUID)
    dbConfig=db[['DBconfig']]
    
    emuDBs.query.tmp<-list()
    emuDBs.query.tmp[['queryItems']]<-dbGetQuery(get_emuDBcon(dbConfig$UUID),paste0("SELECT * FROM items WHERE db_uuid='",dbUUID,"'"))
    emuDBs.query.tmp[['queryLabels']]<-dbGetQuery(get_emuDBcon(dbConfig$UUID),paste0("SELECT * FROM labels WHERE db_uuid='",dbUUID,"'"))
    emuDBs.query.tmp[['queryLinksExt']]<-dbGetQuery(get_emuDBcon(dbConfig$UUID),paste0("SELECT * FROM linksExt WHERE db_uuid='",dbUUID,"'"))
    setQueryTmpEmuDBs(emuDBs.query.tmp)
    trSl=convert.query.result.to.segmentlist.var(dbConfig = dbConfig,result=result)
    inSlLen=nrow(seglist)
    trSlLen=nrow(trSl)
    # free temp tables
    setQueryTmpEmuDBs(NULL)
    if(inSlLen!=trSlLen){
      warning("Length of requery segment list (",trSlLen,") differs from input list (",inSlLen,")!")
    }
    return(trSl)
  }
}


