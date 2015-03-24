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
  dbd = create.DBconfig.from.TextGrid(fpl[1,2], dbName, tierNames)
  
  
  # create empty database
  db = create.database(name = dbd$name, basePath = '', DBconfig = dbd)
  
  itemsTableName = "emuR_emuDB_items_tmp"
  
  labelsTableName ="emuR_emuDB_labels_tmp"
  
  linksTableName = "emuR_emuDB_links_tmp"
  
  
  # Create an ephemeral in-memory RSQLite database
  con <- dbConnect(RSQLite::SQLite(), ":memory:")
  
  initialize_database_tables(con, itemsTableName, labelsTableName, linksTableName)
  
  
  # allBundles object to hold bundles without levels and links
  allBundles = list()
  
  # loop through fpl
  for(i in 1:dim(fpl)[1]){
    
    # get sampleRate of audio file
    asspObj = read.AsspDataObj(fpl[i,1])
    
    # create bundle name
    #     bndlName = basename(file_path_sans_ext(fpl[i,2]))
    bndlName = gsub('^_', '', gsub(.Platform$file.sep, '_', gsub(normalizePath(path2rootDir),'',file_path_sans_ext(normalizePath(fpl[i,1])))))
    
    # parse TextGrid
    parse.textgrid(fpl[i,2], attributes(asspObj)$sampleRate, db='ae', bundle=bndlName, session="0000", 
                   conn = con, itemsTableName=itemsTableName, labelsTableName=labelsTableName)
    
    # remove unwanted levels
    if(!is.null(tierNames)){
      delete_unwanted_levels_from_database_tables(con, itemsTableName, labelsTableName, linksTableName, tierNames)
    }
    
    # validate bundle
    valRes = validate.sqlTableRep.bundle(dbd, bndlName, conn = con, itemsTableName = itemsTableName, 
                                         labelsTableName = labelsTableName, linksTableName=linksTableName)
    
    if(valRes$type != 'SUCCESS'){
      stop('Parsed TextGrid did not pass validator! The validator message is: ', valRes$message)
    }
    
    # create bundle and append
    allBundles[[bndlName]] = create.bundle(name = bndlName,
                                           sessionName = '0000',
                                           annotates = paste0(basename(file_path_sans_ext(fpl[i,2])),'.', audioExt),
                                           sampleRate = attr(asspObj, 'sampleRate'),
                                           levels = list(),
                                           signalpaths = list(unname(fpl[i,1])),
                                           mediaFilePath = unname(fpl[i,1]),
                                           links = list())
    
    
    # update pb
    if(showProgress){
      setTxtProgressBar(pb, i)
    }
    
  }
  
  # extract dataframes and assign them to db Obj
  db[['items']]=dbReadTable(con, itemsTableName)
  db[['labels']]=dbReadTable(con, labelsTableName)
  db[['links']]=dbReadTable(con, linksTableName)
  
  # create dummy container with bundles
  db[['sessions']][['0000']]=emuDB.session(name='0000', bundles=allBundles)
  
  # store newly generated emuDB
  if(showProgress){
    cat('\n') # hack to have newline after pb
  }
  
  # Disconnect from the database
  dbDisconnect(con)
  
  # store
  store.emuDB(db, targetDir, showProgress = showProgress)
  
  
}

# FOR DEVELOPMENT
# library('testthat')
# test_file('tests/testthat/test_convert.TextGridCollection.R')
