##' Create emuDB database schema object from a TextGrid file
##' 
##' @param tgPath path to TextGrid file
##' @param dbName name of the database
##' @return object of class emuDB.schema.db
##' @author Raphael Winkelmann
##' @import stringr uuid wrassp
##' @keywords emuDB database schema Emu TextGrid
## 
create.database.schema.from.TextGrid = function(tgPath, dbName){

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
  tgAnnot = parse.textgrid(tgPath, 44100) # sampleRate hardcoded because it does not matter
  
  # generate defaultLvlOrder
  defaultLvlOrder=list()
  for(lev in tgAnnot){
    if(lev$type == 'SEGMENT' || lev$type == 'EVENT'){
      defaultLvlOrder[[length(defaultLvlOrder)+1L]]=lev$name
    }
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
                                            ssffTracks = list(),
                                            levelDefinitions = list(),
                                            linkDefinitions = list(),
                                            EMUwebAppConfig = waCfg,
                                            annotationDescriptors = list(),
                                            tracks = list(),
                                            flags=list());
  
  # hardcoded maxNumberOfLabels (always 1 in TextGrids)
  dbSchema$maxNumberOfLabels = 1
  
  return(dbSchema)
}

# FOR DEVELOPMENT
tgPath = "/Library/Frameworks/R.framework/Versions/3.1/Resources/library/emuR/extdata/legacy_emu/DBs//ae/labels/msajc003.TextGrid"
schemaFromTg = create.database.schema.from.TextGrid(tgPath, 'test12')
# print(schemaFromTg)