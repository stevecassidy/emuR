require(RSQLite)

##' Convert a TextGridCollection (.wav & .TextGrid files) to a emuDB
##' 
##' Converts a TextGridCollection to a emuDB by first generating a file pair list 
##' containing the paths to the .wav & .TextGrid (default extentions) files with the same base
##' name. It then generates a emuDB DBconfig from the first TextGrid in this file pair list. 
##' After this it converts all file pairs to the new format.
##' 
##' @param path2rootDir path to root directory (CAUTION: think of DB size and search space!) 
##' @param dbName name given to newly generated emuDB
##' @param targetDir directory where to save newly generated emuDB
##' @param tgExt extention of TextGrid files (default=TextGrid meaning filesnames of the form baseName.TextGrid)
##' @param audioExt extention of audio files (default=wav meaning filesnames of the form baseName.wav).
##' @param tierNames character vector containing names of tiers to extract and convert. If NULL (the default) all
##' tiers are converted.
##' @param showProgress show progress bar flag
##' @seealso create.filePairList
##' @export
##' @author Raphael Winkelmann
##' 
convert.TextGridCollection.to.emuDB <- function(path2rootDir, dbName, 
                                                targetDir, tgExt = 'TextGrid', 
                                                audioExt = 'wav', tierNames = NULL, 
                                                showProgress = TRUE){
  # normalize paths
  path2rootDir = suppressWarnings(normalizePath(path2rootDir))
  targetDir = suppressWarnings(normalizePath(targetDir))
  
  # check if path2rootDir exists
  if(!file.exists(path2rootDir)){
    stop("path2rootDir does not exist!")
  }
  
  # check if targetDir exists
  if(!file.exists(targetDir)){
    stop("targetDir does not exist!")
  }
  
  # check if target dir already exists
  if(file.exists(file.path(targetDir, dbName))){
    stop('The directory ', file.path(targetDir, dbName), ' already exists. Can not generate new emuDB if directory called ', dbName, ' already exists!')
  }
  
  # gernerate file pail list
  fpl = create.filePairList(path2rootDir, path2rootDir, audioExt, tgExt)
  
  progress = 0
  
  if(showProgress){
    cat("INFO: Loading TextGridCollection containing", length(fpl[,1]), "file pairs...\n")
    pb = txtProgressBar(min = 0, max = length(fpl[,1]), initial = progress, style=3)
    setTxtProgressBar(pb, progress)
  }
  
  # gereate schema from first TextGrid in fpl
  schema = create.DBconfig.from.TextGrid(fpl[1,2], dbName, tierNames)
  # set transient values
  schema=.update.transient.schema.values(schema)
  # create db object
  db=create.database(name = schema[['name']],basePath = normalizePath(targetDir),DBconfig = schema)
  
  .initialize.DBI.database()
  dbsDf=dbGetQuery(emuDBs.con,paste0("SELECT * FROM emuDB WHERE uuid='",schema[['UUID']],"'"))
  if(nrow(dbsDf)>0){
    stop("EmuDB '",dbsDf[1,'name'],"', UUID: '",dbsDf[1,'uuid'],"' already loaded!")
  }
  
  .store.emuDB.DBI(db)
  
  # get dbObj
  dbUUID = get.emuDB.UUID(dbName = dbName, dbUUID = NULL)
  dbObj = .load.emuDB.DBI(uuid = dbUUID)
  
  
  # allBundles object to hold bundles without levels and links
  allBundles = list()
  
  # create session entry
  dbGetQuery(emuDBs.con, paste0("INSERT INTO session VALUES('", dbUUID, "', '0000')"))
  
  # loop through fpl
  for(i in 1:dim(fpl)[1]){
    
    # get sampleRate of audio file
    asspObj = read.AsspDataObj(fpl[i,1])
    
    # create bundle name
    bndlName = gsub('^_', '', gsub(.Platform$file.sep, '_', gsub(normalizePath(path2rootDir),'',file_path_sans_ext(normalizePath(fpl[i,1])))))
    
    # create bundle entry
    dbGetQuery(emuDBs.con, paste0("INSERT INTO bundle VALUES('", dbUUID, "', '0000', '", bndlName, "', '", basename(fpl[i,1]), "', ", attributes(asspObj)$sampleRate, ",'", fpl[i,1], "')"))
    
    # create track entry
    dbGetQuery(emuDBs.con, paste0("INSERT INTO track VALUES('", dbUUID, "', '0000', '", bndlName, "', '", fpl[i,1], "')"))
    
    # parse TextGrid
    parse.textgrid(fpl[i,2], attributes(asspObj)$sampleRate, dbName=dbName, bundle=bndlName, session="0000")
    
    # remove unwanted levels
    if(!is.null(tierNames)){
      
      condStr = paste0("level!='", paste0(tierNames, collapse = paste0("' AND ", " level!='")), "'")

      # delete items
      dbSendQuery(emuDBs.con, paste0("DELETE FROM items WHERE ", "db_uuid='", dbUUID, "' AND ", condStr))
      
      # delete labels
      dbSendQuery(emuDBs.con, paste0("DELETE FROM labels", 
                                     " WHERE ", "db_uuid='", dbUUID, "' AND itemID NOT IN (SELECT itemID FROM items)"))
    }
    
    # validate bundle
    valRes = validateBundle.emuDB.DBI(dbName, session='0000', bundle=bndlName)
    
    if(valRes$type != 'SUCCESS'){
      stop('Parsed TextGrid did not pass validator! The validator message is: ', valRes$message)
    }
    
    
    # update pb
    if(showProgress){
      setTxtProgressBar(pb, i)
    }
    
  }
  
  # store newly generated emuDB
  if(showProgress){
    cat('\n') # hack to have newline after pb
  }
  
  
  # store
  store.emuDB(dbName, targetDir, showProgress = showProgress)
  
  # purge tmp emuDB
  .purge.emuDB(dbUUID)
  
}

# FOR DEVELOPMENT
# library('testthat')
# test_file('tests/testthat/test_convert.TextGridCollection.R')
