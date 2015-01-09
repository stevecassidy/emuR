require(RSQLite)

##' Create emuDB DBconfig object from a TextGrid file
##' 
##' @param tgPath path to TextGrid file
##' @param dbName name of the database
##' @return object of class emuDB.schema.db
##' @author Raphael Winkelmann
##' @import stringr uuid wrassp RSQLite
##' @keywords emuDB database schema Emu TextGrid
## 
create.DBconfig.from.TextGrid = function(tgPath, dbName){
  
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
  
  # parse TextGrid
  itemsTableName = "emuR_emuDB_items_tmp"
  
  labelsTableName ="emuR_emuDB_labels_tmp"
  
  linksTableName = "emuR_emuDB_links_tmp"
  
  # Create an ephemeral in-memory RSQLite database
  con <- RSQLite::dbConnect(RSQLite::SQLite(), ":memory:")
  
  initialize_database_tables(con, itemsTableName, labelsTableName, linksTableName)
  
  
  parse.textgrid(tgPath, 20000, db='ae', bundle="msajc003", session="0000", conn = con, itemsTableName=itemsTableName, labelsTableName=labelsTableName) # sampleRate doesn't matter!! -> hardcoded
  #   tgAnnot = parse.textgrid(tgPath, 44100) # sampleRate hardcoded because it does not matter
  
  res <- RSQLite::dbSendQuery(con, paste0("SELECT DISTINCT level, type FROM ", itemsTableName))
  levels = RSQLite::dbFetch(res)
  RSQLite::dbClearResult(res)
  
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
      stop('what?')
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
  
  # Disconnect from the database
  RSQLite::dbDisconnect(con)
  return(dbSchema)
}

# FOR DEVELOPMENT
# library('testthat')
# test_file('tests/testthat/test_create.DBconfig.from.TextGrid.R')
