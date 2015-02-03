autobuild.linkFromTimes <- function(db, superlevelName, sublevelName){
  
  foundSuperLevelDev = NULL
  foundSubLevelDev = NULL
  
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
        break
      }
    }
  }
  
  if(!found){
    stop('Did not find linkDefintion for: ', superlevelName, ' ', sublevelName, '. Defined linkDefinitions are: ', sapply(db$DBconfig$linkDefinitions, function(x){paste0(x$superlevelName, '->', x$sublevelName, '; ')}))
  }
  
  
  itemsTableName = "emuR_emuDB_items_tmp"
  
  labelsTableName ="emuR_emuDB_labels_tmp"
  
  linksTableName = "emuR_emuDB_links_tmp"
  
  # Create an ephemeral in-memory RSQLite database
  con <- dbConnect(RSQLite::SQLite(), ":memory:")
  
  # inject dataframes
  dbWriteTable(con, itemsTableName, db$items)
  dbWriteTable(con, labelsTableName, db$labels)
  dbWriteTable(con, linksTableName, db$links)

  # query DB depending on type of sublevelDefinition 
  if(foundSubLevelDev$type == 'EVENT'){
    res=dbSendQuery(con, paste0("INSERT INTO ", linksTableName, " (session, bundle, fromID, toID) ",
                            " SELECT it1.session, it1.bundle, it1.itemID, it2.itemID", 
                            " FROM ", itemsTableName, " as it1 JOIN ", itemsTableName, " as it2 ",
                            " WHERE it1.level = '", superlevelName, "'", " AND it2.level = '", sublevelName, "'", 
                            " AND it1.session = it2.session", " AND it1.bundle = it2.bundle",
                            " AND it2.samplePoint >= it1.sampleStart", " AND it2.samplePoint <= (it1.sampleStart + it1.sampleDur)")) # only for EVENT sublevel
  }else{
    dbSendQuery(con, paste0("INSERT INTO ", linksTableName, " (session, bundle, fromID, toID) ",
                            "SELECT it1.session, it1.bundle, it1.itemID, it2.itemID", 
                            " FROM ", itemsTableName, " as it1 JOIN ", itemsTableName, " as it2 ",
                            " WHERE it1.level = '", superlevelName, "'", " AND it2.level = '", sublevelName, "'", 
                            " AND it1.session = it2.session", " AND it1.bundle = it2.bundle",
                            " AND it2.sampleStart >= it1.sampleStart", " AND (it2.sampleStart + it2.sampleDur) <= (it1.sampleStart + it1.sampleDur)")) # only for SEGMENT sublevel    
  }
  
#     t = dbFetch(res)
#     dbClearResult(res)
#     print(t)
  
  res = dbSendQuery(con, paste0("SELECT *", 
                                " FROM ", linksTableName))
  t = dbFetch(res)
  dbClearResult(res)
  print(t)
  
  
  # Disconnect from the database
  dbDisconnect(con)
  
}

# FOR DEVELOPMENT 
library('testthat') 
test_file('tests/testthat/test_autobuild.R')
