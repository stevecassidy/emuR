require(sqldf)
require(stringr)

##' Requery sequential context of segment list
##' @description WARNING! Experimental, syntax and semantics may change!! 
##' @param seglist segment list to requery on (type: 'emuRsegs')
##' @param offset start offset in sequence
##' @param offsetRef reference elemnt for offset: 'START' for first and 'END' for last element of segment list
##' @param length element length of returned segment list
##' @param resultType type (class name) of result
##' @param dbUUID optional UUID odf emuDB
##' @return result set object of class resultType (default: 'emuRsegs')
##' @author Klaus Jaensch
##' @import sqldf
##' @export
##' @seealso \code{\link{query}}
##' @keywords emuDB database requery Emu EQL2 
##' @examples
##' \dontrun{
##' 
##' ## Requery previous element of 'p' on phonetic level
##' sl1=query('ae','Phonetic=p')
##' 
##' requery_seq(sl1,offset=-1)
##' 
##' ## Requery context (adding previuos and following elements) of 'p' on phonetic level
##'
##' requery_seq(sl1,offset=-1,length=3)
##' 
##' ## Requery previous element of n->t sequence
##' sl2=query('ae',"[Phoneme=n -> Phoneme=t]")
##' 
##' requery_seq(sl2,offset=-1)
##' 
##' ## Requery last element of n->t sequence
##' 
##' requery_seq(sl2,offsetRef='END')
##' 
##' ## Requery following element of n->t sequence
##' 
##' requery_seq(sl2,offset=1,offsetRef='END')
##' 
##' ## Requery context (previuos and following) of n->t sequence
##' 
##' requery_seq(sl2,offset=-1,length=4)
##' 
##' }
requery_seq<-function(seglist, offset=0,offsetRef='START',length=1,ignoreOutOfBounds=FALSE,resultType=NULL,dbUUID=NULL){
  if(!inherits(seglist,"emuRsegs")){
    stop("Segment list 'seglist' must be of type 'emuRsegs'. (Do not set a value for 'resultType' parameter for the query, the default resultType wiil be used)")
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
    
    # create temporary items
    emuDBs.query.tmp=list()
    emuDBs.query.tmp[['queryItems']]<-dbGetQuery(get_emuDBcon(dbUUID),paste0("SELECT * FROM items WHERE db_uuid='",dbUUID,"'"))
    emuDBs.query.tmp[['queryLabels']]<-dbGetQuery(get_emuDBcon(dbUUID),paste0("SELECT * FROM labels WHERE db_uuid='",dbUUID,"'"))
    emuDBs.query.tmp[['queryLinksExt']]<-dbGetQuery(get_emuDBcon(dbUUID),paste0("SELECT * FROM linksExt WHERE db_uuid='",dbUUID,"'"))
    setQueryTmpEmuDBs(emuDBs.query.tmp)
    
    items=dbReadTable(get_emuDBcon(dbUUID),'items')
    itemsIdxSql='CREATE INDEX items_idx ON items(itemID,db_uuid,session,bundle,level,itemID,seqIdx,type,sampleRate,sampleStart,sampleDur,samplePoint)'
    resIdxSql='CREATE INDEX its_idx ON its(db_uuid,session,bundle,seqStartId,seqEndId,seqLen,level)'
    #   
    #labelsIdxSql='CREATE INDEX labels_idx ON lblsDf(itemID,name)'
    labelsIdxSql='CREATE INDEX labels_idx ON lblsDf(itemID,db_uuid,session,bundle,name)'
    
    # check if out of boundaries
    
    
    # query for sequential requeries
    heQueryStr=paste0("SELECT il.db_uuid,il.session,il.bundle,il.itemID AS seqStartId,ir.itemID AS seqEndID,",length," AS seqLen,il.level FROM seglist sl,items sll,items slr,items il, items ir \
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
    if(is.null(resultType)){
      trSl=convert.query.result.to.segmentlist.var(dbConfig = dbConfig,result=result)
    }else{
      if(resultType=='emuRsegs'){
        trSl=convert.query.result.to.segmentlist.var(dbConfig = dbConfig,result=result)
      }else if(resultType=='emusegs'){
        trSl=convert.query.result.to.seglist(dbConfig = dbConfig,result = result)
      }else{
        stop("Unknown result type: '",resultType,"'. Supported result types: 'emuRsegs','emusegs'")
      }
    }
    return(trSl)
  }
}

##' Requery hierarchical context of segment list
##' @description WARNING! Experimental, syntax and semantics may change!! 
##' @param seglist segment list to requery on (type: 'emuRsegs')
##' @param level character string: result level 
##' @param resultType type (class name) of result (for now only 'emuRsegs')
##' @param dbUUID optional UUID odf emuDB
##' @return result set object of class resultType (default: 'emuRsegs')
##' @author Klaus Jaensch
##' @import sqldf
##' @export
##' @seealso \code{\link{query}}
##' @keywords emuDB database requery Emu EQL2 
##' @examples
##' \dontrun{
##' 
##' ## Downward requery phoneme sequence of word (level Text) 'beautiful'
##' 
##' sl1=query('ae','Text=beautiful')
##' requery_hier(sl1,level='Phoneme')
##'
##' ## Upward requery words (level Text) of Phonetic p elements
##' 
##' sl1=query('ae','Phonetic=p')
##' requery_hier(sl1,level='Text')
##' 
##' ## Combined requery: last phonemes of words beginning with 'an'
##' 
##' sl1=query('ae',"Text=~an.*")
##' requery_seq(requery_hier(sl1,level='Phoneme'),offsetRef = 'END')
##' 
##' }
requery_hier<-function(seglist,level=NULL,resultType=NULL,dbUUID=NULL){
  if(!inherits(seglist,"emuRsegs")){
    stop("Segment list 'seglist' must be of type 'emuRsegs'. (Do not set a value for 'resultType' parameter for the query, the default resultType wiil be used)")
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
    
    # create temporary items
    emuDBs.query.tmp=list()
    emuDBs.query.tmp[['queryItems']]<-dbGetQuery(get_emuDBcon(dbUUID),paste0("SELECT * FROM items WHERE db_uuid='",dbUUID,"'"))
    emuDBs.query.tmp[['queryLabels']]<-dbGetQuery(get_emuDBcon(dbUUID),paste0("SELECT * FROM labels WHERE db_uuid='",dbUUID,"'"))
    emuDBs.query.tmp[['queryLinksExt']]<-dbGetQuery(get_emuDBcon(dbUUID),paste0("SELECT * FROM linksExt WHERE db_uuid='",dbUUID,"'"))
    setQueryTmpEmuDBs(emuDBs.query.tmp)
    
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
      #        [1] ‘1.0.0’
      
      
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
                         FROM lIts il JOIN rIts ir ON il.ROWID=ir.ROWID")
    }
    
    he=sqldf(c(itemsIdxSql,linksIdxSql,heQueryStr))
    result=list(items=he)
    if(is.null(resultType)){
      trSl=convert.query.result.to.segmentlist.var(dbConfig = dbConfig,result=result)
    }else{
      if(resultType=='emuRsegs'){
        trSl=convert.query.result.to.segmentlist.var(dbConfig = dbConfig,result=result)
      }else{
        stop("Unknown result type: '",resultType,"'. Supported result types: 'emuRsegs'")
      }
    }
    return(trSl)
  }
}


