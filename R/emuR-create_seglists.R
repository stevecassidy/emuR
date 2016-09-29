
convert_queryResultToEmusegs<-function(emuDBhandle, timeRefSegmentLevel=NULL, filteredTablesSuffix, noTimes = F){
  queryStr = DBI::dbGetQuery(emuDBhandle$connection, "SELECT query_str FROM interm_res_meta_infos_tmp_root")$query_str
  emuRsegs = convert_queryResultToEmuRsegs(emuDBhandle, timeRefSegmentLevel, filteredTablesSuffix, queryStr = queryStr, noTimes)
  emusegs = as.emusegs(emuRsegs)
  return(emusegs)
}

##################################
#
convert_queryResultToEmuRsegs <- function(emuDBhandle, timeRefSegmentLevel=NULL, filteredTablesSuffix, queryStr = "", noTimes = F){
  
  itemsTableName = paste0("items", filteredTablesSuffix)
  labelsTableName = paste0("labels", filteredTablesSuffix)
  
  projectionItemsN = 0 
  if(DBI::dbExistsTable(emuDBhandle$connection, "interm_res_proj_items_tmp_root")){
    projectionItemsN = DBI::dbGetQuery(emuDBhandle$connection, paste0("SELECT COUNT(*) AS n FROM interm_res_proj_items_tmp_root"))$n
  }
  
  if(projectionItemsN > 0){
    
    # as this function uses the query_databaseHier function which only works on
    # the interm_res_items_tmp_root table -> insert everything into interm_res_items_tmp_root
    DBI::dbGetQuery(emuDBhandle$connection, paste0("DELETE FROM interm_res_items_tmp_root"))
    DBI::dbGetQuery(emuDBhandle$connection, paste0("INSERT INTO interm_res_items_tmp_root ",
                                                   "SELECT DISTINCT db_uuid, session, bundle, ",
                                                   "p_seq_start_id AS seq_start_id, p_seq_end_id AS seq_end_id, p_seq_len AS seq_len, p_level AS level ", 
                                                   "FROM interm_res_proj_items_tmp_root"))
    
  }
  
  # check for empty result 
  itemsN = DBI::dbGetQuery(emuDBhandle$connection, paste0("SELECT COUNT(*) AS n FROM interm_res_items_tmp_root"))$n
  if(itemsN > 0 ){
    
    # use "normal" items
    itsTableName = "interm_res_items_tmp_root"
    seqStartIdColName = "seq_start_id"
    seqEndIdColName = "seq_end_id"
    seqLenColName = "seq_len"
    levelColName = "level"
    
    
    dbConfig = load_DBconfig(emuDBhandle)
    
    resultAttrDef = DBI::dbGetQuery(emuDBhandle$connection, paste0("SELECT DISTINCT ", levelColName, " FROM ", itsTableName))[[levelColName]]
    
    attrDefLn = get_levelNameForAttributeName(emuDBhandle, resultAttrDef)
    ld = get_levelDefinition(emuDBhandle, attrDefLn)
    
    # get labelIdx 
    for(i in 1:length(ld$attributeDefinitions)){
      if(ld$attributeDefinitions[[i]]$name == resultAttrDef){
        labelIdx = i
        break
      }
    }
    # create temp table that holds emuRsegs without labels
    DBI::dbGetQuery(emuDBhandle$connection, paste0("CREATE TEMP TABLE emursegs_tmp ( ",
                                                   "labels TEXT, ",
                                                   "start REAL, ",
                                                   "end REAL, ",
                                                   "utts TEXT, ",
                                                   "db_uuid VARCHAR(36), ",
                                                   "session TEXT, ",
                                                   "bundle TEXT, ",
                                                   "start_item_id INTEGER, ",
                                                   "end_item_id INTEGER, ",
                                                   "level TEXT, ",
                                                   "type TEXT, ",
                                                   "sample_start INTEGER, ",
                                                   "sample_end INTEGER, ",
                                                   "sample_rate INTEGER",
                                                   ");"))
    
    if(noTimes){ # no times are requested then that makes things a lot easier :-)
      DBI::dbGetQuery(emuDBhandle$connection, paste0("INSERT INTO emursegs_tmp ",
                                                     "SELECT DISTINCT'XXX' AS labels, ",
                                                     "NULL AS start, ",
                                                     "NULL AS end, ",
                                                     "interm_res_items_tmp_root.session || ':' || interm_res_items_tmp_root.bundle AS utts, ",
                                                     "interm_res_items_tmp_root.db_uuid, interm_res_items_tmp_root.session, interm_res_items_tmp_root.bundle, interm_res_items_tmp_root.seq_start_id AS startItemID, interm_res_items_tmp_root.seq_end_id AS endItemID, ",
                                                     "interm_res_items_tmp_root.level AS level, items_seq_start.type AS type, ",
                                                     "NULL AS sampleStart, NULL AS sampleEnd, items_seq_start.sample_rate AS sampleRate ",
                                                     "FROM interm_res_items_tmp_root, items AS items_seq_start, items AS items_seq_end, labels ",
                                                     "WHERE interm_res_items_tmp_root.db_uuid = items_seq_start.db_uuid AND interm_res_items_tmp_root.session = items_seq_start.session AND interm_res_items_tmp_root.bundle = items_seq_start.bundle AND interm_res_items_tmp_root.seq_start_id = items_seq_start.item_id ",
                                                     "AND interm_res_items_tmp_root.db_uuid = items_seq_end.db_uuid AND interm_res_items_tmp_root.session = items_seq_end.session AND interm_res_items_tmp_root.bundle = items_seq_end.bundle AND interm_res_items_tmp_root.seq_end_id = items_seq_end.item_id ",
                                                     "AND interm_res_items_tmp_root.db_uuid = labels.db_uuid AND interm_res_items_tmp_root.session = labels.session AND interm_res_items_tmp_root.bundle = labels.bundle AND interm_res_items_tmp_root.seq_end_id = labels.item_id AND labels.label_idx = ", labelIdx, " ",
                                                     "ORDER BY items_seq_start.db_uuid, items_seq_start.session, items_seq_start.bundle, items_seq_start.sample_start"))
      
    }else if(ld$type != "ITEM"){ # if level has time information, time can be calculated from sample values directly
      
      DBI::dbGetQuery(emuDBhandle$connection, paste0("INSERT INTO emursegs_tmp ",
                                                     "SELECT 'XXX' AS labels, ",
                                                     "CASE items_seq_start.type ",
                                                     " WHEN 'SEGMENT' THEN ",
                                                     "  CASE items_seq_start.sample_start ",
                                                     "  WHEN 0 THEN CAST(0.0 AS REAL) ",
                                                     "  ELSE (CAST (items_seq_start.sample_start AS REAL) - 0.5 ) / CAST(items_seq_start.sample_rate AS REAL) * 1000.0 ",
                                                     "  END",
                                                     " WHEN 'EVENT' THEN CAST (items_seq_start.sample_point AS REAL) / CAST(items_seq_start.sample_rate AS REAL) * 1000.0 ",
                                                     " ELSE 'SIC!! Something went wrong' ",
                                                     "END AS start, ",
                                                     "CASE items_seq_start.type ",
                                                     " WHEN 'SEGMENT' THEN (CAST (items_seq_end.sample_start + items_seq_end.sample_dur AS REAL) + 0.5) / CAST (items_seq_end.sample_rate AS REAL) * 1000.0 ",
                                                     " WHEN 'EVENT' THEN 0.0",
                                                     " ELSE 'SIC!! Something went wrong' ",
                                                     "END AS end, ",
                                                     "interm_res_items_tmp_root.session || ':' || interm_res_items_tmp_root.bundle AS utts, ",
                                                     "interm_res_items_tmp_root.db_uuid, interm_res_items_tmp_root.session, interm_res_items_tmp_root.bundle, interm_res_items_tmp_root.seq_start_id AS startItemID, interm_res_items_tmp_root.seq_end_id AS endItemID, ",
                                                     "interm_res_items_tmp_root.level AS level, items_seq_start.type AS type, ",
                                                     "items_seq_start.sample_start AS sampleStart, (items_seq_end.sample_start + items_seq_end.sample_dur) AS sampleEnd, items_seq_start.sample_rate AS sampleRate ",
                                                     "FROM interm_res_items_tmp_root, items AS items_seq_start, items AS items_seq_end, labels ",
                                                     "WHERE interm_res_items_tmp_root.db_uuid = items_seq_start.db_uuid AND interm_res_items_tmp_root.session = items_seq_start.session AND interm_res_items_tmp_root.bundle = items_seq_start.bundle AND interm_res_items_tmp_root.seq_start_id = items_seq_start.item_id ",
                                                     "AND interm_res_items_tmp_root.db_uuid = items_seq_end.db_uuid AND interm_res_items_tmp_root.session = items_seq_end.session AND interm_res_items_tmp_root.bundle = items_seq_end.bundle AND interm_res_items_tmp_root.seq_end_id = items_seq_end.item_id ",
                                                     "AND interm_res_items_tmp_root.db_uuid = labels.db_uuid AND interm_res_items_tmp_root.session = labels.session AND interm_res_items_tmp_root.bundle = labels.bundle AND interm_res_items_tmp_root.seq_end_id = labels.item_id AND labels.label_idx = ", labelIdx, " ",
                                                     "ORDER BY items_seq_start.db_uuid, items_seq_start.session, items_seq_start.bundle, items_seq_start.sample_start"))
      
    }else{
      segLvlNms = find_segmentLevels(emuDBhandle, resultAttrDef)
      
      if(!is.null(timeRefSegmentLevel)){
        if(!(timeRefSegmentLevel %in% segLvlNms)){
          stop("Cannot resolve time information for result level '",resultAttrDef,"' using segment time reference level '",timeRefSegmentLevel,"'\nPlease set one of these levels for timeRefSegmentLevel parameter: ",paste(segLvlNms,collapse=', '),".")
        }
        lnwt = timeRefSegmentLevel # level name with time
      }else{
        segLvlsCnt=length(segLvlNms)
        if(segLvlsCnt>1){
          stop("Segment time information derivation for level '",resultAttrDef,"' is ambiguous:\nThe level is linked to multiple segment levels: ",paste(segLvlNms,collapse=', '),"\nPlease select one of these levels using the 'timeRefSegmentLevel' query parameter.")
        }
        lnwt = segLvlNms[1] # level name with time
      }
      
      # insert all time items into new table
      timeItemsTableSuffix = "time_level_items"
      create_intermResTmpQueryTablesDBI(emuDBhandle, suffix = timeItemsTableSuffix)
      
      DBI::dbGetQuery(emuDBhandle$connection, paste0("INSERT INTO interm_res_items_tmp_", timeItemsTableSuffix, " ",
                                                     "SELECT db_uuid, session, bundle, item_id AS seq_start_id, item_id AS seq_end_id, 1 AS seq_len, level  FROM ", itemsTableName, " ",
                                                     "WHERE db_uuid ='", emuDBhandle$UUID, "' AND level = '", lnwt, "' ",
                                                     "AND session IN (SELECT session FROM interm_res_items_tmp_root) ",
                                                     "AND bundle IN (SELECT bundle FROM interm_res_items_tmp_root) "
      ))
      
      
      query_databaseHier(emuDBhandle, firstLevelName = lnwt, secondLevelName = attrDefLn, leftTableSuffix = timeItemsTableSuffix, rightTableSuffix = "root", filteredTablesSuffix) # result written to lr_exp_res_tmp table
      
      # calculate left and right times and store in tmp table
      DBI::dbGetQuery(emuDBhandle$connection, paste0("INSERT INTO emursegs_tmp ",
                                                     "SELECT 'XXX' AS labels, ",
                                                     "CASE itl.type ",
                                                     " WHEN 'SEGMENT' THEN ",
                                                     "  CASE min(itl.sample_start) ",
                                                     "  WHEN 0 THEN CAST(0.0 AS REAL) ",
                                                     "  ELSE (CAST (min(itl.sample_start) AS REAL) - 0.5 ) / CAST(itl.sample_rate AS REAL) * 1000.0 ",
                                                     "  END",
                                                     " WHEN 'EVENT' THEN 'Not implemented yet!'",
                                                     " ELSE 'SIC!! Something went wrong' ",
                                                     "END AS start, ",
                                                     "CASE itl.type ",
                                                     " WHEN 'SEGMENT' THEN (CAST (max(itr.sample_start + itr.sample_dur) AS REAL) + 0.5) / CAST (itr.sample_rate AS REAL) * 1000.0 ",
                                                     " WHEN 'EVENT' THEN 0.0",
                                                     " ELSE 'SIC!! Something went wrong' ",
                                                     "END AS end, ",
                                                     "lr_exp_res_tmp.session || ':' || lr_exp_res_tmp.bundle AS utts, ",
                                                     "lr_exp_res_tmp.db_uuid, lr_exp_res_tmp.session, lr_exp_res_tmp.bundle, lr_exp_res_tmp.r_seq_start_id AS start_item_id, lr_exp_res_tmp.r_seq_end_id AS end_item_id, ",
                                                     "'", resultAttrDef, "' AS level, '", ld$type, "' AS type, ",
                                                     "min(itl.sample_start + 0) AS sample_start, max(itr.sample_start + itr.sample_dur) AS sample_end, itl.sample_rate AS sample_rate ",
                                                     "FROM interm_res_items_tmp_root AS irit, lr_exp_res_tmp, ", itemsTableName, " AS itl, ", itemsTableName, " AS itr ", # items table left & right
                                                     "WHERE irit.db_uuid = lr_exp_res_tmp.db_uuid AND irit.session = lr_exp_res_tmp.session AND irit.bundle = lr_exp_res_tmp.bundle AND irit.seq_start_id = lr_exp_res_tmp.r_seq_start_id AND irit.seq_end_id = lr_exp_res_tmp.r_seq_end_id ",
                                                     "AND lr_exp_res_tmp.db_uuid = itl.db_uuid AND lr_exp_res_tmp.session = itl.session AND lr_exp_res_tmp.bundle = itl.bundle AND lr_exp_res_tmp.l_seq_start_id = itl.item_id ",
                                                     "AND lr_exp_res_tmp.db_uuid = itr.db_uuid AND lr_exp_res_tmp.session = itr.session AND lr_exp_res_tmp.bundle = itr.bundle AND lr_exp_res_tmp.l_seq_end_id = itr.item_id ",
                                                     "GROUP BY irit.rowid, irit.db_uuid, irit.session, irit.bundle, irit.seq_start_id, irit.seq_end_id ", # using irit.rowid to preserve duplicates (requery only)
                                                     "ORDER BY lr_exp_res_tmp.db_uuid, lr_exp_res_tmp.session, lr_exp_res_tmp.bundle, min(itl.sample_start)",
                                                     ""))
      
    }
    
    # construct labels
    DBI::dbGetQuery(emuDBhandle$connection, paste0("CREATE INDEX IF NOT EXISTS emursegs_tmp_idx ON emursegs_tmp(db_uuid, session, bundle, start_item_id, end_item_id)"))
    
    seglist = DBI::dbGetQuery(emuDBhandle$connection, paste0("SELECT GROUP_CONCAT(labels.label, '->') AS labels, emursegs_tmp.start, emursegs_tmp.end, emursegs_tmp.utts, emursegs_tmp.db_uuid, emursegs_tmp.session, emursegs_tmp.bundle, ",
                                                             "emursegs_tmp.start_item_id AS startItemID, emursegs_tmp.end_item_id AS endItemID, emursegs_tmp.level, ",
                                                             "emursegs_tmp.type, emursegs_tmp.sample_start AS sampleStart, emursegs_tmp.sample_end AS sampleEnd, emursegs_tmp.sample_rate AS sampleRate ",
                                                             "FROM emursegs_tmp, ", itemsTableName, " AS itl, ", itemsTableName, " AS itr, ", itemsTableName, " AS iseq, ", labelsTableName, " AS labels ", # items table left & right
                                                             "WHERE emursegs_tmp.db_uuid = itl.db_uuid AND emursegs_tmp.session = itl.session AND emursegs_tmp.bundle = itl.bundle AND emursegs_tmp.start_item_id = itl.item_id ",
                                                             "AND emursegs_tmp.db_uuid = itr.db_uuid AND emursegs_tmp.session = itr.session AND emursegs_tmp.bundle = itr.bundle AND emursegs_tmp.end_item_id = itr.item_id ",
                                                             "AND itl.db_uuid = iseq.db_uuid AND itl.session = iseq.session AND itl.bundle = iseq.bundle AND itl.level = iseq.level ",
                                                             "AND iseq.seq_idx >= itl.seq_idx  AND iseq.seq_idx <= itr.seq_idx ", # join all seq. items
                                                             "AND iseq.db_uuid = labels.db_uuid AND iseq.session = labels.session AND iseq.bundle = labels.bundle AND iseq.item_id = labels.item_id ",
                                                             "AND labels.label_idx = ", labelIdx, " ",
                                                             "GROUP BY emursegs_tmp.rowid, emursegs_tmp.db_uuid, emursegs_tmp.session, emursegs_tmp.bundle, emursegs_tmp.start_item_id, emursegs_tmp.end_item_id", # once again using rowid to preserve duplicates (requery only)
                                                             ""))
    
    # drop temp table
    DBI::dbGetQuery(emuDBhandle$connection, paste0("DROP TABLE IF EXISTS emursegs_tmp"))
  }else{
    seglist = data.frame(labels = character(), start = numeric(), end = numeric(), utts = character(),
                         db_uuid = character(), session = character(), bundle = character(),
                         startItemID = numeric(), endItemID = numeric(), level = character(),
                         type = character(), sampleStart = numeric(), sampleEnd = numeric(), sampleRate = numeric(),
                         stringsAsFactors = F)
  }
  # set emusegs type attribute, default 'segment'
  slType='segment'
  if(nrow(seglist)>0){
    # set to event only if all rows are of type EVENT
    if(all(seglist$type == "EVENT")){
      slType='event'
    }
  }
  # queryStr = DBI::dbGetQuery(emuDBhandle$connection, "SELECT query_str FROM interm_res_meta_infos_tmp_root")$query_str
  segmentList=make.emuRsegs(dbName = emuDBhandle$dbName, seglist = seglist, query = queryStr, type = slType)
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