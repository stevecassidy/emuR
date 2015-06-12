require(RSQLite)

##' Create emuDB DBconfig object from a TextGrid file
##' 
##' @param tgPath path to TextGrid file
##' @param dbName name of the database
##' @param tierNames character vector containing names of tiers to extract and convert. If NULL (the default) all
##' tiers are converted.
##' @return object of class emuDB.schema.db
##' @author Raphael Winkelmann
##' @import stringr uuid wrassp RSQLite
##' @keywords emuDB database schema Emu TextGrid
## 
create.DBconfig.from.TextGrid = function(tgPath, dbName, tierNames=NULL){
  
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
  
  # create tmp db 
  dbConfig=create.schema.databaseDefinition(name=dbName,mediafileExtension = 'wav')
  db=create.database(name=dbName,basePath=file.path(tempdir(), dbName),DBconfig = dbConfig)
  .store.emuDB.DBI(get_emuDBcon(), database = db)
  
  # parse TextGrid  
  
  parse.textgrid(tgPath, 20000, dbName=dbName, bundle="tmpBundleName", session="0000") # sampleRate doesn't matter!! -> hardcoded
  
  # remove unwanted levels
  if(!is.null(tierNames)){
    
    condStr = paste0("level!='", paste0(tierNames, collapse = paste0("' AND ", " level!='")), "'")
    # delete items
    dbSendQuery(get_emuDBcon(), paste0("DELETE FROM items WHERE db_uuid='", dbConfig$UUID, "' AND ", condStr))
    
    # delete labels
    dbSendQuery(get_emuDBcon(), paste0("DELETE FROM labels ", 
                                   "WHERE db_uuid='", dbConfig$UUID, "' AND itemID NOT IN (SELECT itemID FROM items)"))
  }
  
  levels <- dbGetQuery(get_emuDBcon(), paste0("SELECT DISTINCT level, type FROM items WHERE db_uuid='", dbConfig$UUID, "'"))
  
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
                                      attributeDefinitions = list(create.schema.attributeDefinition(lev$level)))
    levIdx = levIdx + 1
  }
  
  
  # create signalCanvas config
  sc = create.EMUwebAppConfig.signalCanvas(order = c("OSCI","SPEC"), 
                                           assign = list(), 
                                           contourLims = list())
  
  # create perspective
  defPersp = create.EMUwebAppConfig.perspective(name = 'default', 
                                                signalCanvases = sc, 
                                                levelCanvases = list(order = defaultLvlOrder), 
                                                twoDimCanvases = list(order = list()))
  # create EMUwebAppConfig 
  waCfg = create.EMUwebAppConfig(perspectives=list(defPersp))
  
  
  
  # generate full schema list
  dbSchema = create.schema.databaseDefinition(name = dbName,
                                              UUID = UUIDgenerate(),
                                              mediafileBasePathPattern = '',
                                              mediafileExtension = 'wav',
                                              ssffTrackDefinitions = list(),
                                              levelDefinitions = levelDefinitions,
                                              linkDefinitions = list(),
                                              EMUwebAppConfig = waCfg,
                                              annotationDescriptors = list(),
                                              tracks = list(),
                                              flags=list());
  
  # hardcoded maxNumberOfLabels (always 1 in TextGrids)
  dbSchema$maxNumberOfLabels = 1
  
  # purge tmp DB 
  UUID = get_emuDB_UUID(dbName = dbName)
  .purge.emuDB(UUID)
  
  
  return(dbSchema)
}

# FOR DEVELOPMENT
# library('testthat')
# test_file('tests/testthat/test_create.DBconfig.from.TextGrid.R')
