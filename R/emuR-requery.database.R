requireNamespace("stringr", quietly = T)

database.DDL.emuRsegsTmp = 'CREATE TEMP TABLE emursegs_tmp (
  labels TEXT,
  start FLOAT,
  end FLOAT,
  utts TEXT,
  db_uuid VARCHAR(36) NOT NULL,
  session TEXT, 
  bundle TEXT,
  start_item_id INTEGER,
  end_item_id INTEGER,
  level TEXT,
  start_item_seq_idx INTEGER,
  end_item_seq_idx INTEGER,
  type TEXT,
  sample_start INTEGER,
  sample_end  INTEGER,
  sample_rate FLOAT
);'

create_requeryTmpTables <- function(emuDBhandle){
  DBI::dbExecute(emuDBhandle$connection, database.DDL.emuRsegsTmp)
}

drop_requeryTmpTables <- function(emuDBhandle){
  if("emursegs_tmp" %in% DBI::dbListTables(emuDBhandle$connection)){
    DBI::dbExecute(emuDBhandle$connection, "DROP TABLE IF EXISTS emursegs_tmp")
  }
}

check_emuRsegsForRequery <- function(sl){
  
  if(length(unique(sl$level)) != 1){
    warning("emuRsegs contains segments/annotation items of multiple levels!")
  }
  
  sl_df = as.data.frame(sl)
  
  sl_df_sorted = dplyr::arrange_(sl_df, "session", "bundle", "sample_start")
  comp_res = compare::compare(sl_df, sl_df_sorted, allowAll = F)
  if(!comp_res$result){
    warning("emuRsegs is not ordered correctly (by session; bundle; sample_start)! Hence, the ordering of the resulting emuRsegs object of the requery will NOT be the same! Use sort(emuRsegs) to sort the emuRsegs object correctly!")
  }

}


##' Requery sequential context of segment list in an emuDB
##' @description Function to requery sequential context of a segment list queried from an emuDB
##' @details Builds a new segment list on the same hierarchical level and the same length as the segment list given in \code{seglist}. The resulting segments usually have different start position and length (in terms of items of the respective level) controlled by the \code{offset},\code{offsetRef} and \code{length} parameters.
##' A segment here is defined as a single item or a chain of items from the respective level, e.g. if a level in a bundle instance has labels 'a', 'b' and 'c' in that order, 'a' or 'a->b' oder 'a->b->c' are all valid segments, but not 'a->c'.
##' \code{offsetRef} determines if the position offset is referenced to the start or the end item of the segments in the input list \code{seglist}; parameter \code{offset} determines the offset of the resulting item start position to this reference item;
##' parameter \code{length} sets the item length of the result segments.
##' If the requested segments are out of bundle item boundaries and parameter \code{ignoreOutOfBounds} is \code{FALSE} (the default), an error is generated. To get residual resulting segments that lie within the bounds the \code{ignoreOutOfBounds} parameter can be set to \code{TRUE}.
##' The returned segment list is usually of the same length and order as the input \code{seglist}; if \code{ignoreOutOfBounds=FALSE}, the resulting segment list may be out of sync.
##' @param emuDBhandle emuDB handle as returned by \code{\link{load_emuDB}}
##' @param seglist segment list to requery on (type: 'emuRsegs')
##' @param offset start item offset in sequence (default is 0, meaning the start or end item of the input segment)
##' @param offsetRef reference item for offset: 'START' for first and 'END' for last item of segment
##' @param length item length of segments in the returned segment list
##' @param ignoreOutOfBounds ignore result segments that are out of bundle bounds
##' @param calcTimes calculate times for resulting segments (results in \code{NA} values for start and end times in emuseg/emuRsegs). As it can be very computationally expensive to 
##' calculate the times for large nested hierarchies, it can be turned off via this boolean parameter.
##' @param timeRefSegmentLevel set time segment level from which to derive time information. It is only necessary to set this parameter if more than one child level contains time information and the queried parent level is of type ITEM.
##' @param verbose be verbose. Set this to \code{TRUE} if you wish to choose which path to traverse on intersecting hierarchies. If set to \code{FALSE} (the default) all paths will be traversed (= legacy EMU bahaviour).
##' @return result set object of class 'emuRsegs' containing the requeried segments
##' @export
##' @seealso \code{\link{query}} \code{\link{requery_hier}} \code{\link{emuRsegs}}
##' @keywords emuDB database requery
##' @examples
##' \dontrun{
##' 
##' ##################################
##' # prerequisite: loaded ae emuDB 
##' # (see ?load_emuDB for more information)
##' 
##' ## Requery previous item of 'p' on level 'Phonetic'
##' sl1 = query(ae, "Phonetic == p")
##' 
##' requery_seq(ae, sl1, offset = -1)
##' 
##' ## Requery context (adding previuos and following elements) of 'p' on phonetic level
##'
##' requery_seq(ae, sl1, offset = -1, length = 3)
##' 
##' ## Requery previous item of n->t sequence
##' sl2 = query(ae, "[Phoneme == n -> Phoneme == t]")
##' 
##' requery_seq(ae, sl2, offset = -1)
##' 
##' ## Requery last item within n->t sequence
##' 
##' requery_seq(ae, sl2, offsetRef = 'END')
##' 
##' ## Requery following item after n->t sequence
##' 
##' requery_seq(ae, sl2, offset = 1, offsetRef = 'END')
##' 
##' ## Requery context (previous and following items) of n->t sequence
##' 
##' requery_seq(ae, sl2, offset = -1, length = 4)
##' 
##' ## Requery next word contexts (sequence includes target word)
##' 
##' sl3 = query(ae, "Text == to")
##' requery_seq(ae, sl3, length = 2)
##' 
##' ## Requery following two word contexts, ignoring segment 
##' ## sequences that are out of bundle end bounds 
##' requery_seq(ae, sl3, length = 3, ignoreOutOfBounds = TRUE)
##' 
##' }
requery_seq<-function(emuDBhandle, seglist, offset = 0, offsetRef = 'START', 
                      length = 1, ignoreOutOfBounds = FALSE, calcTimes = TRUE, 
                      timeRefSegmentLevel = NULL, verbose = FALSE){
  
  if(!inherits(seglist,"emuRsegs")){
    stop("Segment list 'seglist' must be of type 'emuRsegs'. (Do not set a value for 'resultType' parameter in the query() command; then the default resultType=emuRsegs will be used)")
  }
  if(length<=0){
    stop("Parameter length must be greater than 0")
  }
  
  if(nrow(seglist)==0){
    # empty seglist, return the empty list
    return(seglist)
  }else{
    check_emuRsegsForRequery(seglist)
    
    # drop create tmp tables and recreate (will ensure they are empty)
    drop_requeryTmpTables(emuDBhandle)
    create_requeryTmpTables(emuDBhandle)
    # place in emuRsegsTmp table
    DBI::dbExecute(emuDBhandle$connection, "DELETE FROM emursegs_tmp;") # delete 
    
    DBI::dbWriteTable(emuDBhandle$connection, "emursegs_tmp", as.data.frame(seglist), append=T, row.names = F) # append to make sure field names don't get overwritten
    
    # load config
    dbConfig=load_DBconfig(emuDBhandle)
    
    # query for sequential requeries
    # heQueryStr=paste0("SELECT il.db_uuid,il.session,il.bundle, il.item_id AS seq_start_id, ir.item_id AS seq_end_id,",length," AS seq_len, sl.level, il.seq_idx AS seq_start_seq_idx, ir.seq_idx AS seq_end_seq_idx ",
    #                   "FROM emursegs_tmp sl, items sll, items slr, items il, items ir ",
    #                   "WHERE il.db_uuid=ir.db_uuid AND il.session=ir.session AND il.bundle=ir.bundle AND ",
    #                   "il.db_uuid=sl.db_uuid AND il.session=sl.session AND il.bundle=sl.bundle AND ",
    #                   "sll.db_uuid=sl.db_uuid AND sll.session=sl.session AND sll.bundle=sl.bundle AND sl.start_item_id=sll.item_id AND ",
    #                   "slr.db_uuid=sl.db_uuid AND slr.session=sl.session AND slr.bundle=sl.bundle AND sl.end_item_id=slr.item_id AND ")
    
    if(FALSE){ # here the boolean input parameter should be 
      join_type = "LEFT JOIN"
    }else{
      join_type = "JOIN"
    }
    
    if(offsetRef=='START'){
      heQueryStr=paste0("SELECT sl.db_uuid, sl.session, sl.bundle, items_start.item_id AS seq_start_id, items_end.item_id AS seq_end_id, ", length, " AS seq_len, sl.level, items_start.seq_idx AS seq_start_seq_idx, items_end.seq_idx AS seq_end_seq_idx ",
                        "FROM emursegs_tmp sl ",
                        join_type, " items items_start ON sl.db_uuid = items_start.db_uuid AND sl.session = items_start.session AND sl.bundle = items_start.bundle AND sl.level = items_start.level AND sl.start_item_seq_idx + ", offset, " = items_start.seq_idx ",
                        join_type, " items items_end ON sl.db_uuid = items_end.db_uuid AND sl.session = items_end.session AND sl.bundle = items_end.bundle  AND sl.level = items_end.level AND sl.start_item_seq_idx + ", offset + length - 1, " = items_end.seq_idx ",
                        "")
      
      #heQueryStr=paste0(heQueryStr,"il.level = sll.level AND il.seq_idx = sll.seq_idx + ", offset, " AND ",
      #                  "ir.level=sll.level AND ir.seq_idx=sll.seq_idx+",offset+length-1)
    }else if(offsetRef=='END'){
      heQueryStr=paste0("SELECT sl.db_uuid, sl.session, sl.bundle, items_start.item_id AS seq_start_id, items_end.item_id AS seq_end_id, ", length, " AS seq_len, sl.level, items_start.seq_idx AS seq_start_seq_idx, items_end.seq_idx AS seq_end_seq_idx ",
                        "FROM emursegs_tmp sl ",
                        join_type, " items items_start ON sl.db_uuid = items_start.db_uuid AND sl.session = items_start.session AND sl.bundle = items_start.bundle AND sl.level = items_start.level AND sl.end_item_seq_idx + ", offset, " = items_start.seq_idx ",
                        join_type, " items items_end ON sl.db_uuid = items_end.db_uuid AND sl.session = items_end.session AND sl.bundle = items_end.bundle  AND sl.level = items_end.level AND sl.end_item_seq_idx + ", offset + length - 1, " = items_end.seq_idx ",
                        "")
      #heQueryStr=paste0(heQueryStr,"il.level=slr.level AND il.seq_idx=slr.seq_idx+",offset," AND ",
      #                  "ir.level=slr.level AND ir.seq_idx=slr.seq_idx+",offset+length-1)
    }else{
      stop("Parameter offsetRef must be one of 'START' or 'END'\n")
    }
    #heQueryStr=paste0(heQueryStr," ORDER BY il.ROWID");
    he = DBI::dbGetQuery(emuDBhandle$connection, heQueryStr)
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
    
    # drop and create tmpQueryTables and write to table
    drop_allTmpTablesDBI(emuDBhandle)
    create_tmpFilteredQueryTablesDBI(emuDBhandle)
    DBI::dbWriteTable(emuDBhandle$connection, "interm_res_items_tmp_root", he, overwrite=T)
    
    trSl=convert_queryResultToEmuRsegs(emuDBhandle, timeRefSegmentLevel = timeRefSegmentLevel, filteredTablesSuffix = "", queryStr = "FROM REQUERY", calcTimes, verbose)
    drop_allTmpTablesDBI(emuDBhandle)
    
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
##' @param emuDBhandle emuDB handle as returned by \code{\link{load_emuDB}}
##' @param seglist segment list to requery on (type: \link{emuRsegs})
##' @param level character string: name of target level
##' @param collapse collapse the found items in the requested level to a sequence (concatenated with ->). If set to \code{FALSE} separate items as new entries in the emuRsegs object are returned.
##' @param calcTimes calculate times for resulting segments (results in \code{NA} values for start and end times in emuseg/emuRsegs). As it can be very computationally expensive to 
##' calculate the times for large nested hierarchies, it can be turned off via this boolean parameter.
##' @param timeRefSegmentLevel set time segment level from which to derive time information. It is only necessary to set this parameter if more than one child level contains time information and the queried parent level is of type ITEM.
##' @param verbose be verbose. Set this to \code{TRUE} if you wish to choose which path to traverse on intersecting hierarchies. If set to \code{FALSE} (the default) all paths will be traversed (= legacy EMU bahaviour).
##' @return result set object of class \link{emuRsegs}
##' @export
##' @seealso \code{\link{query}} \code{\link{requery_seq}} \code{\link{emuRsegs}}
##' @keywords emuDB database requery
##' @examples
##' \dontrun{
##' 
##' ##################################
##' # prerequisite: loaded ae emuDB 
##' # (see ?load_emuDB for more information)
##' 
##' ## Downward requery: find 'Phoneme' sequences of all words 'beautiful' (of level 'Text')
##' ## Note that the resulting segments consists of phoneme sequences and have therefore 
##' ## the same length as the word segments.
##'
##' sl1 = query(ae, "Text == beautiful")
##' requery_hier(ae, sl1, level = "Phoneme")
##'
##' ## Upward requery: find all word segments that dominate a 'p' on level 'Phoneme'
##' ## Note that the resulting segments are larger than the input segments,
##' ## because they contain the complete words.
##' 
##' sl1 = query(ae, "Phonetic == p")
##' requery_hier(ae, sl1, level = 'Text')
##' 
##' ## Why is there a 'p' the word 'emphazised'? Requery the whole words back down to 'Phoneme' level:
##'
##' requery_hier(ae, sl1, level = 'Phoneme')
##'
##' ## ... because of 'stop epenthesis' a 'p' is inserted between 'm' and 'f'
##' 
##' ## Combined requery: last phonemes of all words beginning with 'an'.
##' ## Note that we use a regular expression 'an.*' (EQL operator '=~') in the query.
##' 
##' sl1=query(ae, "Text =~ an.*")
##' requery_seq(ae, requery_hier(ae, sl1, level = 'Phoneme'), offsetRef = 'END')
##' 
##' }
requery_hier<-function(emuDBhandle, seglist, level, collapse = TRUE, 
                       calcTimes = T, timeRefSegmentLevel = NULL, verbose = FALSE){
  
  if(!inherits(seglist,"emuRsegs")){
    stop("Segment list 'seglist' must be of type 'emuRsegs'. (Do not set a value for 'resultType' parameter for the query, the default resultType will be used)")
  }
  

  if(nrow(seglist)==0){
    # empty seglist, return the empty list
    return(seglist)
  }else{
    
    check_emuRsegsForRequery(seglist)
    
    # drop create tmp tables and recreate (will ensure they are empty)
    drop_allTmpTablesDBI(emuDBhandle)
    create_requeryTmpTables(emuDBhandle)
    drop_tmpFilteredQueryTablesDBI(emuDBhandle)
    create_tmpFilteredQueryTablesDBI(emuDBhandle)
    
    # place in emursegs_tmp table
    DBI::dbExecute(emuDBhandle$connection, "DELETE FROM emursegs_tmp;")
    
    DBI::dbWriteTable(emuDBhandle$connection, "emursegs_tmp", as.data.frame(seglist), append=T, row.names = F) # append to avoid rewirte of col names
    
    # get level for attribute definition specified in seglist
    segListLevel = DBI::dbGetQuery(emuDBhandle$connection, "SELECT DISTINCT level FROM emursegs_tmp;")$level
    
    if(length(segListLevel) > 1){
      stop("Multiple levels found in seglist! This is not supported by requery_hier()!")
    }
    
    
    seglistAttrDefLn = get_levelNameForAttributeName(emuDBhandle, segListLevel)
    # get level for req level (which is actually a attribute definition)
    reqAttrDef = level
    check_levelAttributeName(emuDBhandle, reqAttrDef) # check if valid attr. def
    
    reqAttrDefLn = get_levelNameForAttributeName(emuDBhandle, reqAttrDef)
    
    if(seglistAttrDefLn != reqAttrDefLn){
      
      # insert all original emuRsegs items new table
      origSeglistItemsTableSuffix = "orig_seglist_items"
      create_intermResTmpQueryTablesDBI(emuDBhandle, suffix = origSeglistItemsTableSuffix)
      
      DBI::dbExecute(emuDBhandle$connection, paste0("INSERT INTO interm_res_items_tmp_", origSeglistItemsTableSuffix, " ",
                                                     "SELECT erst.db_uuid, erst.session, erst.bundle, erst.start_item_id AS seq_start_id, erst.end_item_id AS seq_end_id, (i_end.seq_idx - i_start.seq_idx) + 1 AS seq_len, '", level, "' AS level, erst.start_item_seq_idx AS seq_start_seq_idx, erst.end_item_seq_idx AS seq_end_seq_idx ",
                                                     "FROM emursegs_tmp AS erst, items AS i_start, items AS i_end ",
                                                     "WHERE erst.db_uuid = i_start.db_uuid AND erst.session = i_start.session AND erst.bundle = i_start.bundle AND erst.start_item_id = i_start.item_id ", 
                                                     "AND erst.db_uuid = i_end.db_uuid AND erst.session = i_end.session AND erst.bundle = i_end.bundle AND erst.end_item_id = i_end.item_id "))
      
      
      # don't need requery tmp tables any more -> drop them
      drop_requeryTmpTables(emuDBhandle)
      
      # insert all items from requested level into new table
      reqLevelItemsTableSuffix = "req_level_items"
      create_intermResTmpQueryTablesDBI(emuDBhandle, suffix = reqLevelItemsTableSuffix)
      
      DBI::dbExecute(emuDBhandle$connection, paste0("INSERT INTO interm_res_items_tmp_", reqLevelItemsTableSuffix, " ",
                                                     "SELECT db_uuid, session, bundle, item_id AS start_item_id, item_id AS seq_end_id, 1 AS seq_len, level, seq_idx AS seq_start_seq_idx, seq_idx AS seq_end_seq_idx ",
                                                     "FROM items ",
                                                     "WHERE db_uuid ='", emuDBhandle$UUID, "' AND level = '", reqAttrDefLn, "'"))
      
      query_databaseHier(emuDBhandle, firstLevelName = seglistAttrDefLn, secondLevelName = reqAttrDefLn, leftTableSuffix = origSeglistItemsTableSuffix, rightTableSuffix = reqLevelItemsTableSuffix, "", verbose = verbose) # result written to lr_exp_res_tmp table
      
      # move query_databaseHier results into interm_res_items_tmp_root 
      create_intermResTmpQueryTablesDBI(emuDBhandle)
      # create temp table to insert 
      DBI::dbExecute(emuDBhandle$connection,paste0("CREATE TEMP TABLE IF NOT EXISTS seq_idx_tmp ( ",
                                                    "db_uuid VARCHAR(36), ",
                                                    "session TEXT, ",
                                                    "bundle TEXT, ",
                                                    "level TEXT,",
                                                    "min_seq_idx INTEGER, ",
                                                    "max_seq_idx INTEGER)"))
      
      # first step in two step process to group by and get seq min/max
      # then extract according item_id
      if(collapse){
        DBI::dbExecute(emuDBhandle$connection, paste0("INSERT INTO seq_idx_tmp ",
                                                       "SELECT lr.db_uuid, lr.session, lr.bundle, r_level AS level, min(i_start.seq_idx) AS min_seq_idx, max(i_end.seq_idx) AS max_seq_idx ",
                                                       "FROM lr_exp_res_tmp AS lr, items AS i_start, items AS i_end ",
                                                       "WHERE lr.db_uuid = i_start.db_uuid AND lr.session = i_start.session AND lr.bundle = i_start.bundle AND lr.r_seq_start_id = i_start.item_id ",
                                                       "AND lr.db_uuid = i_end.db_uuid AND lr.session = i_end.session AND lr.bundle = i_end.bundle AND lr.r_seq_end_id = i_end.item_id ",
                                                       "GROUP BY lr.db_uuid, lr.session, lr.bundle, lr.l_seq_start_id, lr.l_seq_end_id",
                                                       ""))
      }else{
        # use right side of lr_exp_res_tmp if not collapsing
        DBI::dbExecute(emuDBhandle$connection, paste0("INSERT INTO seq_idx_tmp ",
                                                       "SELECT lr.db_uuid, lr.session, lr.bundle, r_level AS level, min(i_start.seq_idx) AS min_seq_idx, max(i_end.seq_idx) AS max_seq_idx ",
                                                       "FROM lr_exp_res_tmp AS lr, items AS i_start, items AS i_end ",
                                                       "WHERE lr.db_uuid = i_start.db_uuid AND lr.session = i_start.session AND lr.bundle = i_start.bundle AND lr.r_seq_start_id = i_start.item_id ",
                                                       "AND lr.db_uuid = i_end.db_uuid AND lr.session = i_end.session AND lr.bundle = i_end.bundle AND lr.r_seq_end_id = i_end.item_id ",
                                                       "GROUP BY lr.db_uuid, lr.session, lr.bundle, lr.r_seq_start_id, lr.r_seq_end_id",
                                                       ""))
        
      }
      #extract according item_id#s
      DBI::dbExecute(emuDBhandle$connection, paste0("INSERT INTO interm_res_items_tmp_root ",
                                                     "SELECT sit.db_uuid, sit.session, sit.bundle, i_min_idx.item_id AS seq_start_id, i_max_idx.item_id AS seq_end_id, (sit.max_seq_idx - sit.min_seq_idx) + 1 AS seq_len, '", reqAttrDef, "' AS level, sit.min_seq_idx AS seq_start_seq_idx, sit.max_seq_idx AS seq_end_seq_idx ",
                                                     "FROM seq_idx_tmp AS sit, items AS i_min_idx, items AS i_max_idx ",
                                                     "WHERE sit.db_uuid = i_min_idx.db_uuid AND sit.session = i_min_idx.session AND sit.bundle = i_min_idx.bundle AND sit.level = i_min_idx.level AND sit.min_seq_idx = i_min_idx.seq_idx ",
                                                     "AND sit.db_uuid = i_max_idx.db_uuid AND sit.session = i_max_idx.session AND sit.bundle = i_max_idx.bundle AND sit.level = i_max_idx.level AND sit.max_seq_idx = i_max_idx.seq_idx",
                                                     ""))
      
      # drop temp table
      DBI::dbExecute(emuDBhandle$connection,paste0("DROP TABLE IF EXISTS seq_idx_tmp"))
    }else{
      # just reset level as convert_queryResultToEmuRsegs does the rest!
      create_intermResTmpQueryTablesDBI(emuDBhandle)
      
      DBI::dbExecute(emuDBhandle$connection, paste0("INSERT INTO interm_res_items_tmp_root ",
                                                     "SELECT erst.db_uuid, erst.session, erst.bundle, erst.start_item_id AS seq_start_id, erst.end_item_id AS seq_end_id, (i_end.seq_idx - i_start.seq_idx) + 1 AS seq_len, '", level, "' AS level, erst.start_item_seq_idx AS seq_start_seq_idx, erst.end_item_seq_idx AS seq_end_seq_idx ",
                                                     "FROM emursegs_tmp AS erst, items AS i_start, items AS i_end ",
                                                     "WHERE erst.db_uuid = i_start.db_uuid AND erst.session = i_start.session AND erst.bundle = i_start.bundle AND erst.start_item_id = i_start.item_id ", 
                                                     "AND erst.db_uuid = i_end.db_uuid AND erst.session = i_end.session AND erst.bundle = i_end.bundle AND erst.end_item_id = i_end.item_id "))
      # don't need requery tmp tables any more -> drop them
      drop_requeryTmpTables(emuDBhandle)
      
    }
    # trSl=convert_queryResultToVariableEmuRsegs(emuDBhandle)
    trSl=convert_queryResultToEmuRsegs(emuDBhandle, timeRefSegmentLevel = timeRefSegmentLevel, filteredTablesSuffix = "", queryStr = "FROM REQUERY", calcTimes, verbose = verbose)
    inSlLen=nrow(seglist)
    trSlLen=nrow(trSl)
    
    if(verbose & inSlLen!=trSlLen){
      warning("Length of requery segment list (",trSlLen,") differs from input list (",inSlLen,")!")
    }
    
    drop_allTmpTablesDBI(emuDBhandle)
    return(trSl)
  }
}

#######################
# FOR DEVELOPMENT
library('testthat')
test_file("tests/testthat/test_aaa_initData.R")
test_file('tests/testthat/test_emuR-requery.database.R')


