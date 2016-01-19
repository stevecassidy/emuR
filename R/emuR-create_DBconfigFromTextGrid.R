require(RSQLite)

## Create emuDB DBconfig object from a TextGrid file
## 
## @param tgPath path to TextGrid file
## @param dbName name of the database
## @param basePath project base path
## @param tierNames character vector containing names of tiers to extract and convert. If NULL (the default) all
## tiers are converted.
## @return object of class emuDB.schema.db
## @import stringr uuid wrassp RSQLite
## @keywords internal
## 
create_DBconfigFromTextGrid = function(tgPath, dbName, basePath, tierNames = NULL){
  
  ####################
  # check parameters
  
  if(is.null(tgPath)) {
    stop("Argument tgPath (path to TextGrid file) must not be NULL\n")
  }
  
  if(is.null(dbName)) {
    stop("Argument dbName (name of new DB) must not be NULL\n")
  }
  
  #
  ####################
  
  # create tmp dbHandle
  dbHandle = emuDBhandle(dbName, basePath, UUIDgenerate(), connectionPath = ":memory:")
  
  # parse TextGrid  
  parse_TextGridDBI(dbHandle, tgPath, 20000, bundle = "tmpBundleName", session = "0000") # sampleRate doesn't matter!! -> hardcoded
  
  # remove unwanted levels
  if(!is.null(tierNames)){
    
    condStr = paste0("level!='", paste0(tierNames, collapse = paste0("' AND ", " level!='")), "'")
    # delete items
    dbGetQuery(dbHandle$connection, paste0("DELETE FROM items WHERE db_uuid='", dbHandle$UUID, "' AND ", condStr))
    
    # delete labels
    dbGetQuery(dbHandle$connection, paste0("DELETE FROM labels ", 
                                           "WHERE db_uuid='", dbHandle$UUID, "' AND itemID NOT IN (SELECT itemID FROM items)"))
  }
  
  levels <- dbGetQuery(dbHandle$connection, paste0("SELECT DISTINCT level, type FROM items WHERE db_uuid='", dbHandle$UUID, "'"))
  
  # create level definitions
  levelDefinitions = list()
  
  # generate defaultLvlOrder
  defaultLvlOrder=list()
  levIdx = 1  
  for(lineIdx in 1:dim(levels)[1]){
    lev = levels[lineIdx,]
    if(lev$type == 'SEGMENT' || lev$type == 'EVENT'){
      defaultLvlOrder[[length(defaultLvlOrder)+1L]]=lev$level
    }else{
      stop('Found levelDefinition that is not of type SEGMENT|EVENT while parsing TextGrid...this should not occur!')
    }
    # add new leveDef.
    levelDefinitions[[levIdx]] = list(name = lev$level, 
                                      type = lev$type, 
                                      attributeDefinitions = list(list(name = lev$level, type = "STRING")))
    levIdx = levIdx + 1
  }
  
  
  # create signalCanvas config
  sc = list(order = c("OSCI","SPEC"), 
            assign = list(), 
            contourLims = list())
  
  # create perspective
  defPersp = list(name = 'default', 
                  signalCanvases = sc, 
                  levelCanvases = list(order = defaultLvlOrder), 
                  twoDimCanvases = list(order = list()))
  # create EMUwebAppConfig 
  waCfg = list(perspectives = list(defPersp),
               activeButtons = list(saveBundle = TRUE,
                                    showHierarchy = TRUE))
  
  
  
  # generate full schema list
  dbSchema = list(name = dbName,
                  UUID = UUIDgenerate(),
                  mediafileExtension = 'wav',
                  ssffTrackDefinitions = list(),
                  levelDefinitions = levelDefinitions,
                  linkDefinitions = list(),
                  EMUwebAppConfig = waCfg)
  
  
  return(dbSchema)
}

# FOR DEVELOPMENT
# library('testthat')
# test_file('tests/testthat/test_aaa_initData.R')
# test_file('tests/testthat/test_emuR-create_DBconfigFromTextGrid.R')
