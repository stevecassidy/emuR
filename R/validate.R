## Validates the list representation of bundle 
##
# validate.listFrom.bundle <- function(DBconfig, bundle){
#   
#   # check that levels with same name are present
#   levelNames = sapply(bundle$levels, function(l) l$name)
#   levelDefNames = sapply(DBconfig$levelDefinitions, function(l) l$name)
#   
#   delta1 = setdiff(levelNames, levelDefNames)
#   delta2 = setdiff(levelDefNames, levelNames)
#   
#   if(length(delta1) != 0 || length(delta2) != 0){
#     if(length(delta1) != 0){
#       return(list(type = 'ERROR',
#                   message = paste('Following levels where found that do not match any levelDefinition:', paste(delta1), ';',
#                                   'in bundle:', bundle$name)))
#     }else{
#       return(list(type = 'ERROR',
#                   message = paste('Following levelDefinition where not found:', paste(delta2), ';',
#                                   'in bundle:', bundle$name)))      
#     }
#   }
#   
#   # check that levels have same types
#   levelTypes = sapply(bundle$levels, function(l) l$type)
#   levelDefTypes = sapply(DBconfig$levelDefinitions, function(l) l$type)
#   
#   if(!all(levelTypes == levelDefTypes)){
#     return(list(type = 'ERROR',
#                 message = paste('Following level types differ from those defined:', paste(levelNames[levelTypes != levelDefTypes], collapse = ', '), ';',
#                                 'in bundle:', bundle$name)))
#   }  
#   
#   # validate sequence and overlaps
#   for(level in bundle[['levels']]){
#     levelType=level[['type']]
#     sp=-1
#     for(item in level[['items']]){
#       
#       if(levelType=='SEGMENT'){
#         start=item[['sampleStart']]
#         if(start<=sp){
#           return(list(type = 'ERROR',
#                       message = paste('Found sampleStart <= sampleStart + sampleDur of previous item in item with id:', item$id, ';',
#                                       'in level:', level$name, ';',
#                                       'of bundle:', bundle$name)))
#         }
#         dur=item[['sampleDur']]
#         if(dur<0){
#           return(list(type = 'ERROR', 
#                       message = paste('Found sampleDur value < 0 in item with id:', item$id, ';',
#                                       'in level:', level$name, ';',
#                                       'of bundle:', bundle$name)))
#         }
#         sp=start+dur
#       }else if(levelType=='EVENT'){
#         point=item[['samplePoint']]
#         if(point<=sp){
#           return(list(type = 'ERROR', 
#                       message = paste('Found samplePoint <= samplePoint value of previous item in item with id:', item$id, ';',
#                                       'in level:', level$name, ';',
#                                       'of bundle:', bundle$name)))
#         }
#         sp=point
#       }
#     }
#     
#     # check if links exist
#     #     if (length(links))
#     # check for cross links
#     for(link in bundle[['links']]){
#       # TODO
#     }
#     
#   }
#   return(list(type = 'SUCCESS', 
#               message = ''))
# }

# ## Validates the dataframe representation of bundle 
# ##
# validate.dfForm.bundle <- function(DBconfig, itemsDf, labelsDf, linksDf){
#   stop("NOT IMPLEMENTED YET")
# }

## Validates the DBI representation of bundle 
##
validateBundle.emuDB.DBI <- function(dbName, session, bundle, dbUUID=NULL){
  
  #########################
  # get dbObj
  dbUUID = get.emuDB.UUID(dbName = dbName, dbUUID = dbUUID)
  dbObj = .load.emuDB.DBI(uuid = dbUUID)
  
  
  # check that levels with same name are present
  levelNames <- dbGetQuery(emuDBs.con, paste0("SELECT DISTINCT level FROM items WHERE session = '", session,"' ",
                                              "AND bundle ='", bundle, "'"))$level
  
  levelDefNames = sapply(dbObj$DBconfig$levelDefinitions, function(l) l$name)
  delta1 = setdiff(levelNames, levelDefNames)
  delta2 = setdiff(levelDefNames, levelNames)
  
  if(length(delta1) != 0 || length(delta2) != 0){
    if(length(delta1) != 0){
      return(list(type = 'ERROR',
                  message = paste('Following levels where found that do not match any levelDefinition:', paste(delta1), ';',
                                  'in bundle:', bundle)))
    }else{
      return(list(type = 'ERROR',
                  message = paste('Following levelDefinition where not found:', paste(delta2), ';',
                                  'in bundle:', bundle)))      
    }
  }
  
  # check that levels have same types
  levelTypes <- dbGetQuery(emuDBs.con, paste0("SELECT DISTINCT level, type FROM items WHERE session = '", session,"' ",
                                              "AND bundle ='", bundle, "'"))$type
  
  levelDefTypes = sapply(dbObj$DBconfig$levelDefinitions, function(l) l$type)
  
  if(!all(levelTypes == levelDefTypes)){
    return(list(type = 'ERROR',
                message = paste('Following level types differ from those defined:', paste(levelNames[levelTypes != levelDefTypes], collapse = ', '), ';',
                                'in bundle:', bundle)))
  }  
  
  # validate sequence and overlaps in items of type SEGMENTS
  tmp <- dbGetQuery(emuDBs.con, paste0("SELECT DISTINCT * FROM items WHERE session = '", session,"' ", 
                                 "AND bundle ='", bundle, "' ",
                                 "AND type = 'SEGMENT'"))
  
  #TODO: VALIDATE: SEQUENCE + OVERLAPS / LINKS'
  
  
  
  return(list(type = 'SUCCESS', 
              message = ''))
  
}




## FOR DEVELOPMENT
# library('testthat')
# test_file('tests/testthat/test_validate.R')
