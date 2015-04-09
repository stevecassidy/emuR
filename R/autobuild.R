##' Autobuild links between two levels using their time information
##' 
##' Autobuild links between two time levels. The super-level has to be of the 
##' type SEGMENT and the sub-level either of type EVENT or of type SEGMENT. If
##' this is the case and a according link definition is present in DBconfig$linkDefintions,
##' this function automatically links the events or segments of the sub-level which occur
##' within (startSample to (startSample + sampleDur)) the segments of the super-level to those segments.
##' The linkDefinition$type (ONE_TO_MANY, MANY_TO_MANY, ONE_TO_ONE) is relevant whether a link
##' is genarated or not. See the autobuild vignette for more details. 
##' 
##' @param dbName name of EMU database
##' @param superlevelName name of level to link from (link definition required in db)
##' @param sublevelName name of level to link to (link definition required in db)
##' @param writeToDisc should changes be written to disk completing after autobuild
##' @param convertSuperlevel if set to TRUE a backup of the superlevel will be created and the actural
##' superlevel will be converted to a level of type ITEM
##' @param backupLevelAppendStr string appended to level name for backup level
##' @param dbUUID optional UUID of emuDB
##' @author Raphael Winkelmann
##' @import RSQLite
##' @keywords emuR autobuild
##
autobuild.linkFromTimes <- function(dbName, superlevelName, sublevelName, writeToDisc = TRUE, 
                                    convertSuperlevel = FALSE, backupLevelAppendStr = '-autobuildBackup',
                                    dbUUID = NULL){
  
  #########################
  # get dbObj
  dbUUID = get.emuDB.UUID(dbName = dbName, dbUUID = dbUUID)
  dbObj = .load.emuDB.DBI(uuid = dbUUID)
  
  foundSuperLevelDev = NULL
  foundSubLevelDev = NULL
  foundLinkDef = NULL
  
  # check if linkDefinition exists and levelDefinitions (LD) of superlevelName is of type SEGMENT and LD of subleveName is of type EVENT | SEGMENT 
  found = FALSE
  for(ld in dbObj$DBconfig$linkDefinitions){
    if (ld$superlevelName == superlevelName && ld$sublevelName == sublevelName){
      levDefSuper = get.levelDefinition(dbObj$DBconfig, ld$superlevelName)
      levDefSub = get.levelDefinition(dbObj$DBconfig, ld$sublevelName)
      
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
    stop('Did not find linkDefintion for: ', superlevelName, '->', sublevelName, '. Defined linkDefinitions are: ', sapply(dbObj$DBconfig$linkDefinitions, function(x){paste0(x$superlevelName, '->', x$sublevelName, '; ')}))
  }
  
  #   # table names
  #   itemsTableName = "emuR_emuDB_items_tmp"
  #   
  #   labelsTableName ="emuR_emuDB_labels_tmp"
  #   
  #   linksTableName = "emuR_emuDB_links_tmp"
  #   
  #   # Create an ephemeral in-memory RSQLite database
  #   con <- dbConnect(RSQLite::SQLite(), ":memory:")
  #   
  #   # inject dataframes
  #   dbWriteTable(con, itemsTableName, db$items)
  #   dbWriteTable(con, labelsTableName, db$labels)
  #   dbWriteTable(con, linksTableName, db$links)
  
  if(convertSuperlevel){
    # check if backup links exist
    res = dbGetQuery(emuDBs.con, paste0("SELECT * FROM items ", 
                                        "WHERE db_uuid ='", dbUUID, "' ",
                                        "  AND level = '", paste0(superlevelName, backupLevelAppendStr), "'"))
    
    if(dim(res)[1] !=0){
      stop("Can not backup level! Items table already has entries belonging to level: ", paste0(superlevelName, backupLevelAppendStr))
    }
    
    # backup items belonging to superlevel (=duplicate level with new ids)    
    dbSendQuery(emuDBs.con, paste0("INSERT INTO items ",
                                   "SELECT '", dbUUID, "', it.session, it.bundle, it.itemID + bndlMaxValue AS itemID, it.level || '", backupLevelAppendStr, "' AS level, it.type, it.seqIdx, it.sampleRate, it.samplePoint, it.sampleStart, it.sampleDur FROM ",
                                   "items AS it ",
                                   "JOIN ",
                                   "(SELECT db_uuid, session, bundle, MAX(itemID) AS 'bndlMaxValue' FROM ",
                                   "  items GROUP BY bundle ", 
                                   ") as maxIdRes ",
                                   "WHERE it.db_uuid = maxIdRes.db_uuid ",
                                   "   AND it.session = maxIdRes.session ",
                                   "   AND it.bundle = maxIdRes.bundle",
                                   "   AND it.level = '", superlevelName, "'"))
    
    # backup labels belonging to superlevel
    dbSendQuery(emuDBs.con, paste0("INSERT INTO labels ",
                                   "SELECT '", dbUUID, "', lt.session, lt.bundle, lt.itemID + bndlMaxValue AS itemID, labelIdx, lt.name || '", backupLevelAppendStr, "' AS name, label FROM ",
                                   "(SELECT * FROM ",
                                   "  items AS it",
                                   "  JOIN ",
                                   "  (",
                                   "   SELECT db_uuid, session, bundle, MAX(itemID) AS 'bndlMaxValue'",
                                   "   FROM items GROUP BY bundle",
                                   "  ) AS maxIdRes",
                                   " WHERE it.db_uuid = maxIdRes.db_uuid ",
                                   "   AND it.session = maxIdRes.session ",
                                   "   AND it.bundle = maxIdRes.bundle",
                                   "   AND it.level = '", superlevelName, "'",
                                   ") AS tmp ",
                                   "JOIN labels AS lt ", 
                                   "WHERE lt.db_uuid=tmp.db_uuid ",
                                   "  AND lt.session=tmp.session ",
                                   "  AND lt.bundle=tmp.bundle ",
                                   "  AND lt.itemID=tmp.itemID"))
    
#     print(res)
  }
  # query DB depending on type of sublevelDefinition 
  if(foundSubLevelDev$type == 'EVENT'){
    
    dbSendQuery(emuDBs.con, paste0("INSERT INTO links (db_uuid, session, bundle, fromID, toID)",
                                   " SELECT * FROM",
                                   " (SELECT super.db_uuid, super.session, super.bundle, super.itemID AS 'fromID', sub.itemID AS 'toID'", 
                                   " FROM items AS 'super' JOIN items AS 'sub' ",
                                   " WHERE super.level = '", superlevelName, "'", " AND sub.level = '", sublevelName, "'", 
                                   " AND super.db_uuid = '", dbUUID, "' AND sub.db_uuid = '", dbUUID, "'",
                                   " AND super.session = sub.session", " AND super.bundle = sub.bundle",
                                   " AND (sub.samplePoint + 0 >= super.sampleStart + 0) AND sub.samplePoint <= (super.sampleStart + super.sampleDur)) AS res", # + 0 added to ensure numeric comparison
                                   " WHERE NOT EXISTS (SELECT lt.fromID, lt.toID FROM links lt WHERE lt.session = res.session AND lt.bundle = res.bundle AND lt.fromID = res.fromID AND lt.toID = res.toID)"))
    
  }else{
    
    if(ld$type == "ONE_TO_MANY"){
      
      dbSendQuery(emuDBs.con, paste0("INSERT INTO links (db_uuid, session, bundle, fromID, toID)",
                                     " SELECT * FROM",
                                     " (SELECT super.db_uuid, super.session, super.bundle, super.itemID AS 'fromID', sub.itemID AS 'toID'", 
                                     " FROM items as super JOIN items as sub",
                                     " WHERE (super.level = '", superlevelName, "'", " AND sub.level = '", sublevelName, "'", 
                                     " AND super.db_uuid = '", dbUUID, "' AND sub.db_uuid = '", dbUUID, "'",
                                     " AND super.session = sub.session AND super.bundle = sub.bundle",
                                     " AND (sub.sampleStart + 0 >= super.sampleStart + 0)) AND ((sub.sampleStart + sub.sampleDur) <= (super.sampleStart + super.sampleDur))) AS res", # + 0 added to ensure numeric comparison
                                     " WHERE NOT EXISTS (SELECT lt.fromID, lt.toID FROM links lt WHERE lt.session = res.session AND lt.bundle = res.bundle AND lt.fromID = res.fromID AND lt.toID = res.toID)"))
      
    }else if(ld$type == "MANY_TO_MANY"){
      
      dbSendQuery(emuDBs.con, paste0("INSERT INTO links (db_uuid, session, bundle, fromID, toID)",
                                     " SELECT * FROM",
                                     " (SELECT super.db_uuid, super.session, super.bundle, super.itemID AS 'fromID', sub.itemID AS 'toID'", 
                                     " FROM items as super JOIN items as sub",
                                     " WHERE super.level = '", superlevelName, "'", " AND sub.level = '", sublevelName, "'", 
                                     " AND super.db_uuid = '", dbUUID, "' AND sub.db_uuid = '", dbUUID, "'",
                                     " AND super.session = sub.session AND super.bundle = sub.bundle",
                                     " AND (((sub.sampleStart + 0 >= super.sampleStart + 0) AND ((sub.sampleStart + sub.sampleDur) <= (super.sampleStart + super.sampleDur)))", # within
                                     " OR ((sub.sampleStart + 0 <= super.sampleStart + 0) AND ((sub.sampleStart + sub.sampleDur) >= (super.sampleStart + 0)) AND ((sub.sampleStart + sub.sampleDur) <= (super.sampleStart + super.sampleDur)))", # left overlap
                                     " OR ((sub.sampleStart + 0 >= super.sampleStart + 0) AND ((sub.sampleStart + 0) <= (super.sampleStart + super.sampleDur)) AND ((sub.sampleStart + sub.sampleDur) >= (super.sampleStart + super.sampleDur)))", # right overlap
                                     " OR ((sub.sampleStart + 0 <= super.sampleStart + 0) AND ((sub.sampleStart + sub.sampleDur) >= (super.sampleStart + super.sampleDur)))", # left and right overlap
                                     ")) AS res", # right overlap
                                     " WHERE NOT EXISTS (SELECT lt.fromID, lt.toID FROM links lt WHERE lt.session = res.session AND lt.bundle = res.bundle AND lt.fromID = res.fromID AND lt.toID = res.toID)"))
      
    }else if(ld$type == "ONE_TO_ONE"){
      
      dbSendQuery(emuDBs.con, paste0("INSERT INTO links (db_uuid, session, bundle, fromID, toID)",
                                     " SELECT * FROM",
                                     " (SELECT super.db_uuid, super.session, super.bundle, super.itemID AS 'fromID', sub.itemID AS 'toID'", 
                                     " FROM items as super JOIN items as sub",
                                     " WHERE (super.level = '", superlevelName, "'", " AND sub.level = '", sublevelName, "'", 
                                     " AND super.db_uuid = '", dbUUID, "' AND sub.db_uuid = '", dbUUID, "'",
                                     " AND super.session = sub.session AND super.bundle = sub.bundle",
                                     " AND (sub.sampleStart + 0 = super.sampleStart + 0)) AND ((sub.sampleStart + sub.sampleDur) = (super.sampleStart + super.sampleDur))) AS res", # are exatly the same
                                     " WHERE NOT EXISTS (SELECT lt.fromID, lt.toID FROM links lt WHERE lt.session = res.session AND lt.bundle = res.bundle AND lt.fromID = res.fromID AND lt.toID = res.toID)"))
      
    }
  }
  
  
  if(convertSuperlevel){
    print('should get to here')
    
    # generate levelDefinition for backup level
    foundSuperLevelDev$name = paste0(foundSuperLevelDev$name, backupLevelAppendStr)
    for(i in 1:length(foundSuperLevelDev$attributeDefinitions)){
      foundSuperLevelDev$attributeDefinitions[[i]]$name = paste0(foundSuperLevelDev$attributeDefinitions[[i]]$name, backupLevelAppendStr)
    }
    dbObj$DBconfig$levelDefinitions[[length(dbObj$DBconfig$levelDefinitions) + 1]] = foundSuperLevelDev
    
    # convert superlevel to ITEM level
    res = dbSendQuery(emuDBs.con, paste0("UPDATE items SET type = 'ITEM', samplePoint = null, sampleStart = null, sampleDur = null WHERE db_uuid='", dbUUID, "' AND level ='", superlevelName,"'"))
    dbClearResult(res)
    print('done with insert into stuff')
  }
  
  # print resulting table
  #     res = dbSendQuery(con, paste0("SELECT * FROM ", labelsTableName, ""))
  #     print(dbReadTable(con, labelsTableName))
  
  # store changes to disc
  if(writeToDisc){
    print('right here bro!')
    for(sess in db$sessions){
      for(bndl in sess$bundles){
        bndlRep=get.bundle(db, sess$name, bndl$name)
        store.bundle.annotation(db,bndlRep)
      }
    }
    # write DBconfig to disc
    .store.schema(dbObj)
  }else{
    .store.DBconfig.DBI(dbObj$DBconfig)
  }
  # Disconnect from the database
  #   dbDisconnect(con)
  
  #   return(db)
  
}

# FOR DEVELOPMENT 
library('testthat') 
test_file('tests/testthat/test_autobuild.R')
