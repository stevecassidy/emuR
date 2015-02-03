autobuild.linkFromTimes <- function(db, superlevelName, sublevelName){
  
  # check if linkDefinition exists and levelDefinitions (LD) of superlevelName is of type SEGMENT and LD of subleveName is of type EVENT | SEGMENT 
  found = FALSE
  for(ld in db$DBconfig$linkDefinitions){
    print(ld)
    if (ld$superlevelName == superlevelName && ld$sublevelName == sublevelName){
      levDefSuper = get.levelDefinition(db$DBconfig, ld$superlevelName)
      levDefSub = get.levelDefinition(db$DBconfig, ld$superlevelName)
      
      if(levDefSuper$type == 'SEGMENT' && (levDefSub$type == 'SEGMENT' || levDefSub$type == 'EVENT')){
        found = TRUE
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
  
  #initialize_database_tables(con, itemsTableName, labelsTableName, linksTableName)
  
  # inject dataframes
  dbWriteTable(con, itemsTableName, db$items)
  dbWriteTable(con, labelsTableName, db$labels)
  dbWriteTable(con, linksTableName, db$links)

  res = dbSendQuery(con, paste0("SELECT *", 
                                " FROM ", itemsTableName, " as it1 JOIN ", itemsTableName, " as it2 ",
                                " WHERE it1.level = '", superlevelName, "'", " AND it2.level = '", sublevelName, "'", 
                                " AND it1.session = it2.session", " AND it1.bundle = it2.bundle",
                                " AND it2.samplePoint >= it1.sampleStart", " AND it2.samplePoint <= (it1.sampleStart + it1.sampleDur)")) # SIC only works for EVENT sublevel
  t = dbFetch(res)
  dbClearResult(res)
  print(t)

  res = dbSendQuery(con, paste0("SELECT *", 
                                " FROM ", linksTableName)) # SIC only works for EVENT sublevel
  t = dbFetch(res)
  dbClearResult(res)
  print(t)
  
  
  # Disconnect from the database
  dbDisconnect(con)

}

# FOR DEVELOPMENT 
library('testthat') 
test_file('tests/testthat/test_autobuild.R')
