##' Autobuild links between two levels using their time information
##' 
##' Autobuild links between two time levels. The super-level has to be of the 
##' type SEGMENT and the sub-level either of type EVENT or of type SEGMENT. If
##' this is the case and a according link definition is present in db$DBconfig$linkDefintions,
##' this function automatically links the events or segments of the sub-level which occur
##' within (startSample to (startSample + sampleDur)) the segments of the super-level to those segments.
##' The linkDefinition$type (ONE_TO_MANY, MANY_TO_MANY, ONE_TO_ONE) is relavant whether a link
##' is genarated or not. See the autobuild vignette for more details. 
##' 
##' @param db emuDB object to perform autobuild on
##' @param superlevelName name of level to link from (link definition required in db)
##' @param sublevelName name of level to link to (link definition required in db)
##' @param writeToDisc should changes be written to disk completing after autobuild
##' @return an emuDB object
##' @author Raphael Winkelmann
##' @import RSQLite
##' @keywords emuR autobuild
##
autobuild.linkFromTimes <- function(db, superlevelName, sublevelName, writeToDisc = TRUE){
  
  foundSuperLevelDev = NULL
  foundSubLevelDev = NULL
  foundLinkDef = NULL
  
  backupLevelAppendStr = '_autob_BU'
  
  # check if linkDefinition exists and levelDefinitions (LD) of superlevelName is of type SEGMENT and LD of subleveName is of type EVENT | SEGMENT 
  found = FALSE
  for(ld in db$DBconfig$linkDefinitions){
    if (ld$superlevelName == superlevelName && ld$sublevelName == sublevelName){
      levDefSuper = get.levelDefinition(db$DBconfig, ld$superlevelName)
      levDefSub = get.levelDefinition(db$DBconfig, ld$sublevelName)
      
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
    stop('Did not find linkDefintion for: ', superlevelName, '->', sublevelName, '. Defined linkDefinitions are: ', sapply(db$DBconfig$linkDefinitions, function(x){paste0(x$superlevelName, '->', x$sublevelName, '; ')}))
  }
  
  # table names
  itemsTableName = "emuR_emuDB_items_tmp"
  
  labelsTableName ="emuR_emuDB_labels_tmp"
  
  linksTableName = "emuR_emuDB_links_tmp"
  
  # Create an ephemeral in-memory RSQLite database
  con <- dbConnect(RSQLite::SQLite(), ":memory:")
  
  # inject dataframes
  dbWriteTable(con, itemsTableName, db$items)
  dbWriteTable(con, labelsTableName, db$labels)
  dbWriteTable(con, linksTableName, db$links)
  
  # backup superlevel (duplicate new level with new ids ) SIC should do precheck if level exists
  res = dbSendQuery(con, paste0("INSERT INTO ", itemsTableName,
                                " SELECT '", db$name,"' || '_' || session || '_' || bundle || '_' || maxIdRes.bndlMaxID || '_' || (itemID + bndlMaxValue) AS id, session, bundle, level || '", backupLevelAppendStr, "' AS level, itemID + bndlMaxValue, type, seqIdx, sampleRate, samplePoint, sampleStart, sampleDur, label",
                                " FROM (SELECT bundle AS 'bndlMaxID', MAX(itemID) AS 'bndlMaxValue' FROM ", itemsTableName, " GROUP BY bundle) as maxIdRes JOIN ", 
                                itemsTableName, " AS it WHERE maxIdRes.bndlMaxID = it.bundle AND level ='", superlevelName, "'"))
  
  res = dbSendQuery(con, paste0("SELECT * FROM ", itemsTableName, " WHERE level = 'Phonetic_autob_BU'"))
  print(dbFetch(res))
  print(dbFetch(res))
  
  # query DB depending on type of sublevelDefinition 
  if(foundSubLevelDev$type == 'EVENT'){
    
    dbSendQuery(con, paste0("INSERT INTO ", linksTableName, " (session, bundle, fromID, toID)",
                            " SELECT * FROM",
                            " (SELECT super.session, super.bundle, super.itemID AS 'fromID', sub.itemID AS 'toID'", 
                            " FROM ", itemsTableName, " AS 'super' JOIN ", itemsTableName, " AS 'sub' ",
                            " WHERE super.level = '", superlevelName, "'", " AND sub.level = '", sublevelName, "'", 
                            " AND super.session = sub.session", " AND super.bundle = sub.bundle",
                            " AND (sub.samplePoint + 0 >= super.sampleStart + 0) AND sub.samplePoint <= (super.sampleStart + super.sampleDur)) AS res", # + 0 added to ensure numeric comparison
                            " WHERE NOT EXISTS (SELECT lt.fromID, lt.toID FROM ", linksTableName, " lt WHERE lt.session = res.session AND lt.bundle = res.bundle AND lt.fromID = res.fromID AND lt.toID = res.toID)"))
    
  }else{
    
    if(ld$type == "ONE_TO_MANY"){
      
      dbSendQuery(con, paste0("INSERT INTO ", linksTableName, " (session, bundle, fromID, toID)",
                              " SELECT * FROM",
                              " (SELECT super.session, super.bundle, super.itemID AS 'fromID', sub.itemID AS 'toID'", 
                              " FROM ", itemsTableName, " as super JOIN ", itemsTableName, " as sub",
                              " WHERE (super.level = '", superlevelName, "'", " AND sub.level = '", sublevelName, "'", 
                              " AND super.session = sub.session AND super.bundle = sub.bundle",
                              " AND (sub.sampleStart + 0 >= super.sampleStart + 0)) AND ((sub.sampleStart + sub.sampleDur) <= (super.sampleStart + super.sampleDur))) AS res", # + 0 added to ensure numeric comparison
                              " WHERE NOT EXISTS (SELECT lt.fromID, lt.toID FROM ", linksTableName, " lt WHERE lt.session = res.session AND lt.bundle = res.bundle AND lt.fromID = res.fromID AND lt.toID = res.toID)"))
      
    }else if(ld$type == "MANY_TO_MANY"){
      
      dbSendQuery(con, paste0("INSERT INTO ", linksTableName, " (session, bundle, fromID, toID)",
                              " SELECT * FROM",
                              " (SELECT super.session, super.bundle, super.itemID AS 'fromID', sub.itemID AS 'toID'", 
                              " FROM ", itemsTableName, " as super JOIN ", itemsTableName, " as sub",
                              " WHERE super.level = '", superlevelName, "'", " AND sub.level = '", sublevelName, "'", 
                              " AND super.session = sub.session AND super.bundle = sub.bundle",
                              " AND (((sub.sampleStart + 0 >= super.sampleStart + 0) AND ((sub.sampleStart + sub.sampleDur) <= (super.sampleStart + super.sampleDur)))", # within
                              " OR ((sub.sampleStart + 0 <= super.sampleStart + 0) AND ((sub.sampleStart + sub.sampleDur) >= (super.sampleStart + 0)) AND ((sub.sampleStart + sub.sampleDur) <= (super.sampleStart + super.sampleDur)))", # left overlap
                              " OR ((sub.sampleStart + 0 >= super.sampleStart + 0) AND ((sub.sampleStart + 0) <= (super.sampleStart + super.sampleDur)) AND ((sub.sampleStart + sub.sampleDur) >= (super.sampleStart + super.sampleDur)))", # right overlap
                              " OR ((sub.sampleStart + 0 <= super.sampleStart + 0) AND ((sub.sampleStart + sub.sampleDur) >= (super.sampleStart + super.sampleDur)))", # left and right overlap
                              ")) AS res", # right overlap
                              " WHERE NOT EXISTS (SELECT lt.fromID, lt.toID FROM ", linksTableName, " lt WHERE lt.session = res.session AND lt.bundle = res.bundle AND lt.fromID = res.fromID AND lt.toID = res.toID)"))
      
    }else if(ld$type == "ONE_TO_ONE"){
      
      dbSendQuery(con, paste0("INSERT INTO ", linksTableName, " (session, bundle, fromID, toID)",
                              " SELECT * FROM",
                              " (SELECT super.session, super.bundle, super.itemID AS 'fromID', sub.itemID AS 'toID'", 
                              " FROM ", itemsTableName, " as super JOIN ", itemsTableName, " as sub",
                              " WHERE (super.level = '", superlevelName, "'", " AND sub.level = '", sublevelName, "'", 
                              " AND super.session = sub.session AND super.bundle = sub.bundle",
                              " AND (sub.sampleStart + 0 = super.sampleStart + 0)) AND ((sub.sampleStart + sub.sampleDur) = (super.sampleStart + super.sampleDur))) AS res", # are exatly the same
                              " WHERE NOT EXISTS (SELECT lt.fromID, lt.toID FROM ", linksTableName, " lt WHERE lt.session = res.session AND lt.bundle = res.bundle AND lt.fromID = res.fromID AND lt.toID = res.toID)"))
      
    }
  }
  
  # extract link dataframe and assign them to db Obj
  db[['links']]=dbReadTable(con, linksTableName)
  
  # store changes to disc
  if(writeToDisc){
    store.emuDB(db, db$basePath)
  }
  
  # Disconnect from the database
  dbDisconnect(con)
  
  return(db)
  
}

# FOR DEVELOPMENT 
library('testthat') 
test_file('tests/testthat/test_autobuild.R')
