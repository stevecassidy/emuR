##' Autobuild links between two levels using their time information
##' 
##' Autobuild links between two time levels. This is typically done when converting from 
##' a database / annotation format that allows parallel time tiers / levels but does not permit annotational units 
##' to be linked to each other, except by matching time information (such as Praat's TextGrid format). 
##' The super-level has to be of the 
##' type SEGMENT and the sub-level either of type EVENT or of type SEGMENT. If
##' this is the case and a according link definition is defined for the emuDB,
##' this function automatically links the events or segments of the sub-level which occur
##' within (startSample to (startSample + sampleDur)) the segments of the super-level to those segments.
##' 
##' The type of link definition (ONE_TO_MANY, MANY_TO_MANY, ONE_TO_ONE) is relevant whether a link
##' is generated or not (e.g. overlapping segments are linked in a MANY_TO_MANY relationship 
##' but not in a ONE_TO_MANY relationship). For more information on the structural 
##' elements of an emuDB see \code{vignette(emuDB)}.
##' 
##' @param emuDBhandle emuDB handle as returned by \code{\link{load_emuDB}}
##' @param superlevelName name of level to link from (link definition required in emuDB)
##' @param sublevelName name of level to link to (link definition required in emuDB)
##' @param writeToFS should changes be written to file system (_annot.json files) after completing autobuild process (intended for expert use only)
##' @param convertSuperlevel if set to TRUE a backup of the superlevel will be created and the actual
##' superlevel will be converted to a level of type ITEM
##' @param backupLevelAppendStr string appended to level name for backup level
##' @param verbose show progress bars and further information
##' @export
##' @keywords emuR autobuild
##' @seealso add_linkDefinition
##' @examples 
##' \dontrun{
##' 
##' ##################################
##' # prerequisite: loaded myTGcolDB emuDB 
##' # (see ?create_emuRdemoData, ?convert_TextGridCollection, 
##' #  and vignette(emuR_intro) for more information)
##' 
##' # add linkDefinition as one has to be present for
##' # the autobuild function to work
##' add_linkDefinition(emuDBhandle = myTGcolDB, 
##'                    type = "ONE_TO_MANY",
##'                    superlevelName = "Syllable",
##'                    sublevelName = "Phoneme")
##'   
##' # envoke autobuild function to build hierarchy for converted TextGridCollection
##' autobuild_linkFromTimes(emuDBhandle = myTGcolDB, 
##'                         superlevelName = "Syllable",
##'                         sublevelName = "Phoneme",
##'                         convertSuperlevel = TRUE)
##' 
##' }
autobuild_linkFromTimes <- function(emuDBhandle, superlevelName, sublevelName, writeToFS = TRUE, 
                                    convertSuperlevel = FALSE, backupLevelAppendStr = '-autobuildBackup', verbose = TRUE){
  
  dbConfig = load_DBconfig(emuDBhandle)
  
  foundSuperLevelDev = NULL
  foundSubLevelDev = NULL
  foundLinkDef = NULL
  
  # check if linkDefinition exists and levelDefinitions (LD) of superlevelName is of type SEGMENT and LD of subleveName is of type EVENT | SEGMENT 
  found = FALSE
  for(ld in dbConfig$linkDefinitions){
    if (ld$superlevelName == superlevelName && ld$sublevelName == sublevelName){
      levDefSuper = get_levelDefinition(emuDBhandle, ld$superlevelName)
      levDefSub = get_levelDefinition(emuDBhandle, ld$sublevelName)
      
      if(levDefSuper$type == 'SEGMENT' && (levDefSub$type == 'SEGMENT' || levDefSub$type == 'EVENT')){
        found = TRUE
        foundSuperLevelDev = levDefSuper
        foundSubLevelDev = levDefSub
        foundLinkDef = ld
        break
      }
    }
  }
  
  if(!found){
    stop('Did not find linkDefintion for: ', superlevelName, '->', sublevelName, '. Defined linkDefinitions are: ', sapply(dbConfig$linkDefinitions, function(x){paste0(x$superlevelName, '->', x$sublevelName, '; ')}))
  }
  
  if(convertSuperlevel){
    # check if backup links exist
    res = DBI::dbGetQuery(emuDBhandle$connection, paste0("SELECT * FROM items ", 
                                                         "WHERE db_uuid ='", emuDBhandle$UUID, "' ",
                                                         "  AND level = '", paste0(superlevelName, backupLevelAppendStr), "'"))
    
    if(dim(res)[1] !=0){
      stop("Can not backup level! Items table already has entries belonging to level: ", paste0(superlevelName, backupLevelAppendStr))
    }
    
    
    # 
    if(length(foundSuperLevelDev$attributeDefinitions) > 1){
      stop("Backup of parellel labels not implemented yet!")
    }
    
    # create temp tables
    DBI::dbGetQuery(emuDBhandle$connection, "CREATE TEMP TABLE IF NOT EXISTS bndl_max_item_id_tmp (
                    db_uuid VARCHAR(36),
                    session TEXT,
                    bundle TEXT,
                    bndl_max_item_id INTEGER,
                    PRIMARY KEY (db_uuid, session, bundle)
    )")
    
    # create bndl_max_item_id_tmp table
    DBI::dbGetQuery(emuDBhandle$connection, paste0("INSERT INTO bndl_max_item_id_tmp ",
                                                   "SELECT db_uuid, session, bundle, max(item_id) AS bndl_max_item_id FROM items WHERE db_uuid = '", emuDBhandle$UUID, "' ",
                                                   "GROUP BY db_uuid, session, bundle"))
    

    #
    # qRes = DBI::dbGetQuery(emuDBhandle$connection, paste0("INSERT INTO labels ",
    #                                                       "SELECT '", emuDBhandle$UUID, "', lt.session, lt.bundle, lt.item_id + bndlMaxValue AS item_id, label_idx, lt.name || '", backupLevelAppendStr, "' AS name, label FROM ",
    #                                                       "(SELECT * FROM ",
    #                                                       "  items AS it",
    #                                                       "  JOIN ",
    #                                                       "  (",
    #                                                       "   SELECT db_uuid, session, bundle, MAX(item_id) AS 'bndlMaxValue'",
    #                                                       "   FROM items GROUP BY bundle",
    #                                                       "  ) AS maxIdRes",
    #                                                       " WHERE it.db_uuid = maxIdRes.db_uuid ",
    #                                                       "   AND it.session = maxIdRes.session ",
    #                                                       "   AND it.bundle = maxIdRes.bundle",
    #                                                       "   AND it.level = '", superlevelName, "'",
    #                                                       ") AS tmp ",
    #                                                       "JOIN labels AS lt ", 
    #                                                       "WHERE lt.db_uuid=tmp.db_uuid ",
    #                                                       "  AND lt.session=tmp.session ",
    #                                                       "  AND lt.bundle=tmp.bundle ",
    #                                                       "  AND lt.item_id=tmp.item_id"))
    
    # backup labels belonging to superlevel (labels have to be backed up before items to avoid maxID problem (maybe should rewrite query to avoid this in future versions using labels table to determin
    # maxID))
    DBI::dbGetQuery(emuDBhandle$connection, paste0("INSERT INTO labels ",
                                                   "SELECT l.db_uuid, l.session, l.bundle, (l.item_id + mid.bndl_max_item_id) AS item_id, l.label_idx, l.name || '", backupLevelAppendStr, "' AS name, l.label ",
                                                   "FROM items AS it, labels AS l, bndl_max_item_id_tmp AS mid ",
                                                   "WHERE it.db_uuid = '", emuDBhandle$UUID, "' AND it.db_uuid = l.db_uuid AND it.session = l.session AND it.bundle = l.bundle AND it.item_id = l.item_id ",
                                                   "AND it.db_uuid = mid.db_uuid AND it.session = mid.session AND it.bundle = mid.bundle ",
                                                   "AND it.level = '", superlevelName, "'"))
    
    
    
    # qRes = DBI::dbGetQuery(emuDBhandle$connection, paste0("INSERT INTO items ",
    #                                                       "SELECT '", emuDBhandle$UUID, "', it.session, it.bundle, it.item_id + bndlMaxValue AS item_id, it.level || '", backupLevelAppendStr, "' AS level, it.type, it.seq_idx, it.sample_rate, it.sample_point, it.sample_start, it.sample_dur FROM ",
    #                                                       "items AS it ",
    #                                                       "JOIN ",
    #                                                       "(SELECT db_uuid, session, bundle, MAX(item_id) AS 'bndlMaxValue' FROM ",
    #                                                       "  items GROUP BY bundle ", 
    #                                                       ") as maxIdRes ",
    #                                                       "WHERE it.db_uuid = maxIdRes.db_uuid ",
    #                                                       "   AND it.session = maxIdRes.session ",
    #                                                       "   AND it.bundle = maxIdRes.bundle",
    #                                                       "   AND it.level = '", superlevelName, "'"))    
    
    # backup items belonging to superlevel (=duplicate level with new ids)    
    DBI::dbGetQuery(emuDBhandle$connection, paste0("INSERT INTO items ",
                                                   "SELECT items.db_uuid, items.session, items.bundle, (item_id + bndl_max_item_id) AS item_id, items.level || '", backupLevelAppendStr, "' AS level, type, seq_idx, sample_rate, sample_point, sample_start, sample_dur ",
                                                   "FROM items, bndl_max_item_id_tmp ", 
                                                   "WHERE items.db_uuid = bndl_max_item_id_tmp.db_uuid AND items.session = bndl_max_item_id_tmp.session ",
                                                   "AND items.bundle = bndl_max_item_id_tmp.bundle AND items.level = '", superlevelName, "'"))
    # drop temp tables
    DBI::dbGetQuery(emuDBhandle$connection, paste0("DROP TABLE IF EXISTS ", "bndl_max_item_id_tmp"))
    
  }
  
  # create temp table to store all links in. Duplicates will be removed in a sep. query -> performance improvement!
  DBI::dbGetQuery(emuDBhandle$connection, paste0("CREATE TEMP TABLE IF NOT EXISTS autob_all_links_tmp (",
                                                 "db_uuid VARCHAR(36),",
                                                 "session TEXT,",
                                                 "bundle TEXT,",
                                                 "from_id INTEGER,",
                                                 "to_id INTEGER)"))
  
  # query DB depending on type of sublevelDefinition 
  if(foundSubLevelDev$type == 'EVENT'){
    # DBI::dbGetQuery(emuDBhandle$connection, paste0("INSERT INTO links (db_uuid, session, bundle, from_id, to_id)",
    #                                    " SELECT * FROM",
    #                                    " (SELECT super.db_uuid, super.session, super.bundle, super.item_id AS 'from_id', sub.item_id AS 'to_id'", 
    #                                    " FROM items AS 'super' JOIN items AS 'sub' ",
    #                                    " WHERE super.level = '", superlevelName, "'", " AND sub.level = '", sublevelName, "'", 
    #                                    " AND super.db_uuid = '", emuDBhandle$UUID, "' AND sub.db_uuid = '", emuDBhandle$UUID, "'",
    #                                    " AND super.session = sub.session", " AND super.bundle = sub.bundle",
    #                                    " AND (sub.sample_point + 0 >= super.sample_start + 0) AND sub.sample_point <= (super.sample_start + super.sample_dur)) AS res", # + 0 added to ensure numeric comparison
    #                                    " WHERE NOT EXISTS (SELECT lt.from_id, lt.to_id FROM links lt WHERE lt.session = res.session AND lt.bundle = res.bundle AND lt.from_id = res.from_id AND lt.to_id = res.to_id)")) # no duplicates
    
    # get all links and store in temp table
    DBI::dbGetQuery(emuDBhandle$connection, paste0("INSERT INTO autob_all_links_tmp (db_uuid, session, bundle, from_id, to_id) ",
                                                   "SELECT super.db_uuid, super.session, super.bundle, super.item_id AS 'from_id', sub.item_id AS 'to_id' ", 
                                                   "FROM items AS 'super', items AS 'sub' ",
                                                   "WHERE super.level = '", superlevelName, "'", " AND sub.level = '", sublevelName, "' ", 
                                                   "AND super.db_uuid = '", emuDBhandle$UUID, "' AND sub.db_uuid = '", emuDBhandle$UUID, "' ",
                                                   "AND super.session = sub.session", " AND super.bundle = sub.bundle ",
                                                   "AND (sub.sample_point + 0 >= super.sample_start + 0) AND sub.sample_point <= (super.sample_start + super.sample_dur)"))
    
    
  }else{
    
    if(ld$type == "ONE_TO_MANY"){
      
      # DBI::dbGetQuery(emuDBhandle$connection, paste0("INSERT INTO links (db_uuid, session, bundle, from_id, to_id)",
      #                                          " SELECT * FROM",
      #                                          " (SELECT super.db_uuid, super.session, super.bundle, super.item_id AS 'from_id', sub.item_id AS 'to_id'", 
      #                                          " FROM items as super JOIN items as sub",
      #                                          " WHERE (super.level = '", superlevelName, "'", " AND sub.level = '", sublevelName, "'", 
      #                                          " AND super.db_uuid = '", emuDBhandle$UUID, "' AND sub.db_uuid = '", emuDBhandle$UUID, "'",
      #                                          " AND super.session = sub.session AND super.bundle = sub.bundle",
      #                                          " AND (sub.sample_start + 0 >= super.sample_start + 0)) AND ((sub.sample_start + sub.sample_dur) <= (super.sample_start + super.sample_dur))) AS res", # + 0 added to ensure numeric comparison
      #                                          " WHERE NOT EXISTS (SELECT lt.from_id, lt.to_id FROM links lt WHERE lt.session = res.session AND lt.bundle = res.bundle AND lt.from_id = res.from_id AND lt.to_id = res.to_id)"))
      
      # get all links and store in temp table
      DBI::dbGetQuery(emuDBhandle$connection, paste0("INSERT INTO autob_all_links_tmp (db_uuid, session, bundle, from_id, to_id) ",
                                                     "SELECT super.db_uuid, super.session, super.bundle, super.item_id AS 'from_id', sub.item_id AS 'to_id' ", 
                                                     "FROM items as super JOIN items as sub ",
                                                     "WHERE (super.level = '", superlevelName, "'", " AND sub.level = '", sublevelName, "' ", 
                                                     "AND super.db_uuid = '", emuDBhandle$UUID, "' AND sub.db_uuid = '", emuDBhandle$UUID, "' ",
                                                     "AND super.session = sub.session AND super.bundle = sub.bundle ",
                                                     "AND (sub.sample_start + 0 >= super.sample_start + 0)) AND ((sub.sample_start + sub.sample_dur) <= (super.sample_start + super.sample_dur))")) # + 0 added to ensure numeric comparison
      
      
    }else if(ld$type == "MANY_TO_MANY"){
      
      # DBI::dbGetQuery(emuDBhandle$connection, paste0("INSERT INTO links (db_uuid, session, bundle, from_id, to_id)",
      #                                                " SELECT * FROM",
      #                                                " (SELECT super.db_uuid, super.session, super.bundle, super.item_id AS 'from_id', sub.item_id AS 'to_id'", 
      #                                                " FROM items as super JOIN items as sub",
      #                                                " WHERE super.level = '", superlevelName, "'", " AND sub.level = '", sublevelName, "'", 
      #                                                " AND super.db_uuid = '", emuDBhandle$UUID, "' AND sub.db_uuid = '", emuDBhandle$UUID, "'",
      #                                                " AND super.session = sub.session AND super.bundle = sub.bundle",
      #                                                " AND (((sub.sample_start + 0 >= super.sample_start + 0) AND ((sub.sample_start + sub.sample_dur) <= (super.sample_start + super.sample_dur)))", # within
      #                                                " OR ((sub.sample_start + 0 <= super.sample_start + 0) AND ((sub.sample_start + sub.sample_dur) >= (super.sample_start + 0)) AND ((sub.sample_start + sub.sample_dur) <= (super.sample_start + super.sample_dur)))", # left overlap
      #                                                " OR ((sub.sample_start + 0 >= super.sample_start + 0) AND ((sub.sample_start + 0) <= (super.sample_start + super.sample_dur)) AND ((sub.sample_start + sub.sample_dur) >= (super.sample_start + super.sample_dur)))", # right overlap
      #                                                " OR ((sub.sample_start + 0 <= super.sample_start + 0) AND ((sub.sample_start + sub.sample_dur) >= (super.sample_start + super.sample_dur)))", # left and right overlap
      #                                                ")) AS res", # right overlap
      #                                                " WHERE NOT EXISTS (SELECT lt.from_id, lt.to_id FROM links lt WHERE lt.session = res.session AND lt.bundle = res.bundle AND lt.from_id = res.from_id AND lt.to_id = res.to_id)"))
      
      # get all links and store in temp table
      DBI::dbGetQuery(emuDBhandle$connection, paste0("INSERT INTO autob_all_links_tmp (db_uuid, session, bundle, from_id, to_id) ",
                                                     "SELECT super.db_uuid, super.session, super.bundle, super.item_id AS 'from_id', sub.item_id AS 'to_id' ", 
                                                     "FROM items as super JOIN items as sub ",
                                                     "WHERE super.level = '", superlevelName, "'", " AND sub.level = '", sublevelName, "' ", 
                                                     "AND super.db_uuid = '", emuDBhandle$UUID, "' AND sub.db_uuid = '", emuDBhandle$UUID, "' ",
                                                     "AND super.session = sub.session AND super.bundle = sub.bundle ",
                                                     "AND (((sub.sample_start + 0 >= super.sample_start + 0) AND ((sub.sample_start + sub.sample_dur) <= (super.sample_start + super.sample_dur))) ", # within
                                                     "OR ((sub.sample_start + 0 <= super.sample_start + 0) AND ((sub.sample_start + sub.sample_dur) >= (super.sample_start + 0)) AND ((sub.sample_start + sub.sample_dur) <= (super.sample_start + super.sample_dur))) ", # left overlap
                                                     "OR ((sub.sample_start + 0 >= super.sample_start + 0) AND ((sub.sample_start + 0) <= (super.sample_start + super.sample_dur)) AND ((sub.sample_start + sub.sample_dur) >= (super.sample_start + super.sample_dur))) ", # right overlap
                                                     "OR ((sub.sample_start + 0 <= super.sample_start + 0) AND ((sub.sample_start + sub.sample_dur) >= (super.sample_start + super.sample_dur))))")) # left and right overlap
      
    }else if(ld$type == "ONE_TO_ONE"){
      
      # DBI::dbGetQuery(emuDBhandle$connection, paste0("INSERT INTO links (db_uuid, session, bundle, from_id, to_id)",
      #                                                " SELECT * FROM",
      #                                                " (SELECT super.db_uuid, super.session, super.bundle, super.item_id AS 'from_id', sub.item_id AS 'to_id'", 
      #                                                " FROM items as super JOIN items as sub",
      #                                                " WHERE (super.level = '", superlevelName, "'", " AND sub.level = '", sublevelName, "'", 
      #                                                " AND super.db_uuid = '", emuDBhandle$UUID, "' AND sub.db_uuid = '", emuDBhandle$UUID, "'",
      #                                                " AND super.session = sub.session AND super.bundle = sub.bundle",
      #                                                " AND (sub.sample_start + 0 = super.sample_start + 0)) AND ((sub.sample_start + sub.sample_dur) = (super.sample_start + super.sample_dur))) AS res", # are exatly the same
      #                                                " WHERE NOT EXISTS (SELECT lt.from_id, lt.to_id FROM links lt WHERE lt.session = res.session AND lt.bundle = res.bundle AND lt.from_id = res.from_id AND lt.to_id = res.to_id)"))

      # get all links and store in temp table
      DBI::dbGetQuery(emuDBhandle$connection, paste0("INSERT INTO autob_all_links_tmp (db_uuid, session, bundle, from_id, to_id) ",
                                                     "SELECT super.db_uuid, super.session, super.bundle, super.item_id AS 'from_id', sub.item_id AS 'to_id' ", 
                                                     "FROM items as super JOIN items as sub ",
                                                     "WHERE (super.level = '", superlevelName, "'", " AND sub.level = '", sublevelName, "' ", 
                                                     "AND super.db_uuid = '", emuDBhandle$UUID, "' AND sub.db_uuid = '", emuDBhandle$UUID, "' ",
                                                     "AND super.session = sub.session AND super.bundle = sub.bundle ",
                                                     "AND (sub.sample_start + 0 = super.sample_start + 0)) AND ((sub.sample_start + sub.sample_dur) = (super.sample_start + super.sample_dur)) ")) # are exatly the same
                                                     
      
    }
  }
  
  # remove duplicates with left join and insert into links table
  DBI::dbGetQuery(emuDBhandle$connection, paste0("INSERT INTO links ",
                                                 "SELECT * ",
                                                 "FROM autob_all_links_tmp ",
                                                 "LEFT JOIN links USING(db_uuid, session, bundle, from_id, to_id) ",
                                                 "WHERE links.from_id IS NULL"))
  
  
  # drop temp tables
  DBI::dbGetQuery(emuDBhandle$connection, paste0("DROP TABLE IF EXISTS ", "autob_all_links_tmp"))
  
  
  if(convertSuperlevel){
    # change levelDefinition type
    for(i in 1:length(dbConfig$levelDefinitions)){
      if(dbConfig$levelDefinitions[[i]]$name == superlevelName){
        dbConfig$levelDefinitions[[i]]$type = 'ITEM'
      }
    }
    # generate levelDefinition for backup level
    foundSuperLevelDev$name = paste0(foundSuperLevelDev$name, backupLevelAppendStr)
    for(i in 1:length(foundSuperLevelDev$attributeDefinitions)){
      foundSuperLevelDev$attributeDefinitions[[i]]$name = paste0(foundSuperLevelDev$attributeDefinitions[[i]]$name, backupLevelAppendStr)
    }
    dbConfig$levelDefinitions[[length(dbConfig$levelDefinitions) + 1]] = foundSuperLevelDev
    
    # convert superlevel to ITEM level
    DBI::dbGetQuery(emuDBhandle$connection, paste0("UPDATE items SET type = 'ITEM', sample_point = null, sample_start = null, sample_dur = null WHERE db_uuid='", emuDBhandle$UUID, "' AND level ='", superlevelName,"'"))
  }
  
  # rebuild redundant links and
  if(verbose){
    cat("rebuilding redundant links and position of links... (this may take a while)")
  }
  build_allRedundantLinks(emuDBhandle)
  calculate_postionsOfLinks(emuDBhandle)
  
  
  # store changes to disc
  if(writeToFS){
    # write DBconfig to disc
    store_DBconfig(emuDBhandle, dbConfig)
    rewrite_allAnnots(emuDBhandle, verbose=verbose)
  }
  
  # update MD5sums in bundle table
  bndls = list_bundles(emuDBhandle)
  for(i in 1:nrow(bndls)){
    curBndl = bndls[i,]
    annotJSONfilePath = file.path(emuDBhandle$basePath, paste0(curBndl$session, session.suffix), paste0(curBndl$name, bundle.dir.suffix), paste0(curBndl$name, bundle.annotation.suffix, ".json"))
    newMD5sum = tools::md5sum(annotJSONfilePath)                        
    DBI::dbGetQuery(emuDBhandle$connection, paste0("UPDATE bundle SET md5_annot_json = '", newMD5sum, "' WHERE db_uuid ='", emuDBhandle$UUID, "' AND session='", curBndl$session, "' AND name='", curBndl$name, "'"))
  }
  
}

# FOR DEVELOPMENT 
# library('testthat')
# test_file('tests/testthat/test_aaa_initData.R')
# test_file('tests/testthat/test_emuR-autobuild.R')

