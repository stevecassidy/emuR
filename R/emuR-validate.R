## Validates the DBI representation of bundle 
##
validate_bundleDBI <- function(emuDBhandle, 
                               session, 
                               bundle){
  
  DBconfig = load_DBconfig(emuDBhandle)
  
  dbLevelDefs = list_levelDefinitions(emuDBhandle)
  
  # check that levels with same name are present
  levelNames <- DBI::dbGetQuery(emuDBhandle$connection, 
                                paste0("SELECT DISTINCT level ",
                                       "FROM items ",
                                       "WHERE db_uuid = '", emuDBhandle$UUID, "' ",
                                       " AND session = '", session, "' ",
                                       " AND bundle ='", bundle, "'"))$level
  
  levelDefNames = sapply(DBconfig$levelDefinitions, function(l) l$name)
  delta1 = setdiff(levelNames, levelDefNames)
  delta2 = setdiff(levelDefNames, levelNames)
  
  if(length(delta1) != 0 || length(delta2) != 0){
    if(length(delta1) != 0){
      return(list(type = 'ERROR',
                  message = paste('Following levels where found that do not ",
                                  "match any levelDefinition:', paste(delta1), ';',
                                  'in bundle:', bundle)))
    }else{
      warning("No items for levelDefinition where found for level:'", 
              paste(delta2), "';", "in bundle:'", bundle , "'")
    }
  }
  
  # check that levels have same types
  bundleLevels <- DBI::dbGetQuery(emuDBhandle$connection, paste0("SELECT DISTINCT ",
                                                                 " level AS name, ",
                                                                 " type ",
                                                                 "FROM items ",
                                                                 "WHERE db_uuid = '", emuDBhandle$UUID, "' ",
                                                                 " AND session = '", session, "' ",
                                                                 " AND bundle ='", bundle, "' "))
  
  joinedLevelDefs = bundleLevels %>% 
    dplyr::left_join(dbLevelDefs, by = "name") %>% 
    dplyr::select(.data$name, 
                  DBconfigType = .data$type.x, 
                  bundleType = .data$type.y)
  
  if(!all(joinedLevelDefs$DBconfigType == joinedLevelDefs$bundleType)){
    return(list(type = 'ERROR',
                message = paste0('There are level types that differ from those defined:\n', 
                                 paste(utils::capture.output(print(joinedLevelDefs)), 
                                       collapse = "\n"))))
  }  
  
  # validate sequence and overlaps in items of type SEGMENTS
  tmp <- DBI::dbGetQuery(emuDBhandle$connection, paste0("SELECT DISTINCT * ",
                                                        "FROM items ",
                                                        "WHERE session = '", session,"' ", 
                                                        " AND bundle ='", bundle, "' ",
                                                        " AND type = 'SEGMENT'"))
  
  #TODO: VALIDATE: SEQUENCE + OVERLAPS / LINKS'
  
  
  
  return(list(type = 'SUCCESS', 
              message = ''))
  
}




## FOR DEVELOPMENT
# library('testthat')
# test_file('tests/testthat/test_validate.R')
