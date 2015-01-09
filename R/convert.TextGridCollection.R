require(RSQLite)

##' Convert a TextGridCollection (.TextGrid + .wav) to emuDB
##' 
##' Converts a TextGridCollection to a emuDB by first generating a file pair list 
##' containing the paths to .TextGrid + .wav (default extentions) files with the same base
##' name. It then generates a emuDB DBconfig from the first TextGrid in this file pair list. 
##' After this it converts all file pairs to the new format.
##' 
##' @param path2rootDir path to root directory (CAUTION: think of DB size and search space!) 
##' @param dbName name given to newly generated emuDB
##' @param targetDir directory where to save newly generated emuDB
##' @param tgExt extention of TextGrid files (default=TextGrid meaning filesnames of the form baseName.TextGrid)
##' @param audioExt extention of audio files (default=wav meaning filesnames of the form baseName.wav).
##' @param showProgress show progress bar flag
##' @export
##' @author Raphael Winkelmann
##' 
convert.TextGridCollection.to.emuDB <- function(path2rootDir, dbName, 
                                                targetDir, tgExt = 'TextGrid', 
                                                audioExt = 'wav', showProgress = TRUE){
  
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
  dbd = create.DBconfig.from.TextGrid(fpl[1,2], dbName)
  
  
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
    
    # extract bundle name
    bndlName = basename(file_path_sans_ext(fpl[i,2]))
    
    # parse TextGrid
    parse.textgrid(fpl[i,2], attributes(asspObj)$sampleRate, db='ae', bundle=bndlName, session="0000", 
                   conn = con, itemsTableName=itemsTableName, labelsTableName=labelsTableName)
    
    # validate bundle
    valRes = validate.sqlTableRep.bundle(dbd, bndlName, conn = con, itemsTableName = itemsTableName, 
                                         labelsTableName = labelsTableName, linksTableName=linksTableName)
    
    if(valRes$type != 'SUCCESS'){
      stop('Parsed TextGrid did not pass validator! The validator message is: ', valRes$message)
    }
    
    # create bundle and append
    allBundles[[bndlName]] = create.bundle(name = bndlName,
                                           annotates = paste0('0000_ses/', bndlName, '_bndl/', bndlName, '.', audioExt),
                                           sampleRate = attr(asspObj,'sampleRate'),
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

##' Initialize empty database tables 
##' 
##' Initialize empty database tables that in the sql database 
##' specified by the conn argument. The tables are items, labels, links
##' 
##' @param conn sql DB connection
##' @param itemsTableName SQL table name of items table
##' @param labelTableName SQL table name of label table
##' @author Raphael Winkelmann
##'
initialize_database_tables <- function(conn, itemsTableName, labelTableName, linksTableName){
  
  
  # initialize empty tables (items, labels, links)
  items = data.frame(id=character(), session=character(), bundle=character(), level=character(),
                     itemID=integer(), type=character(), seqIdx=integer(), sampleRate=numeric(), 
                     samplePoint=integer(), sampleStart=integer(), sampleDur=integer(), label=character(), stringsAsFactors=FALSE)
  
  RSQLite::dbWriteTable(conn, "emuR_emuDB_items_tmp", items)
  
  labels = data.frame(itemID=character(), session=character(), bundle=character(),
                      labelIdx=integer(), name=character(), label=character(), stringsAsFactors=FALSE)
  
  RSQLite::dbWriteTable(conn, "emuR_emuDB_labels_tmp", labels)
  
  links = data.frame(session=character(), bundle=character(), fromID=integer(),
                     toID=integer(), label=character(), stringsAsFactors=FALSE)
  
  RSQLite::dbWriteTable(conn, "emuR_emuDB_links_tmp", links)
  
}

# FOR DEVELOPMENT
# library('testthat')
# test_file('tests/testthat/test_convert.TextGridCollection.R')
