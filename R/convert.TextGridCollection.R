require(RSQLite)

##' Convert a TextGridCollection (.wav & .TextGrid files) to emuDB
##' 
##' Converts a TextGridCollection to a emuDB by first generating a file pair list 
##' containing the paths to the .wav & .TextGrid files (default extentions) with the same base
##' name. The function then generates a emuDB configurations based on the first TextGrid in this list which specifies 
##' the allowed level names and types in the new emuDB. After this it converts all file pairs to the new format
##' checking whether they comply to the newly generated database configuration.
##' 
##' @param dir path to directory containing TextGridCollection
##' @param dbName name given to emuDB
##' @param targetDir directory where to save emuDB
##' @param tgExt extension of TextGrid files (default=TextGrid meaning file names of the form baseName.TextGrid)
##' @param audioExt extension of audio files (default=wav meaning file names of the form baseName.wav)
##' @param tierNames character vector containing names of tiers to extract and convert. If NULL (the default) all
##' tiers are converted.
##' @param verbose display infos & show progress bar
##' @import tools
##' @export
##' @examples 
##' \dontrun{
##' 
##' ##########################################################
##' # prerequisite: directory containing .wav & .TextGrid files 
##' 
##' # convert TextGridCollection and store 
##' # new emuDB in folder provided by tempdir()
##' convert_TextGridCollection_to_emuDB(dir = "/path/to/directory/", 
##'                                     dbName = "myTGcolDB", 
##'                                     targetDir = tempdir())
##' 
##' 
##' # same as above but this time only convert 
##' # the information stored in the "Syllable" and "Phonetic" tiers
##' convert_TextGridCollection_to_emuDB(dir = "/path/to/directory/", 
##'                                     dbName = "myTGcolDB", 
##'                                     targetDir = tempdir(),
##'                                     tierNames = c("Syllable", "Phonetic"))
##'
##'} 
convert_TextGridCollection_to_emuDB <- function(dir, dbName, 
                                                targetDir, tgExt = 'TextGrid', 
                                                audioExt = 'wav', tierNames = NULL, 
                                                verbose = TRUE){
  # normalize paths
  dir = suppressWarnings(normalizePath(dir))
  targetDir = suppressWarnings(normalizePath(targetDir))
  
  # check if dir exists
  if(!file.exists(dir)){
    stop("dir does not exist!")
  }
  
  # create
  if(!file.exists(targetDir)){
    res=dir.create(targetDir,recursive = TRUE)
    if(!res){
      stop("Could not create target directory: ",targetDir," !\n")
    }
  }
  
  basePath=file.path(targetDir, dbName)
  # check if base path dir already exists
  if(file.exists(basePath)){
    stop('The directory ', basePath, ' already exists. Can not generate new emuDB if directory called ', dbName, ' already exists!')
  }else{
    res=dir.create(basePath)
    if(!res){
      stop("Could not create base directory: ",basePath," !\n")
    }
  }
  
  # generate file pair list
  fpl = create_filePairList(dir, dir, audioExt, tgExt)
  
  progress = 0
  
  if(verbose){
    cat("INFO: Loading TextGridCollection containing", length(fpl[,1]), "file pairs...\n")
    pb = txtProgressBar(min = 0, max = length(fpl[,1]), initial = progress, style=3)
    setTxtProgressBar(pb, progress)
  }
  
  # gereate schema from first TextGrid in fpl
  schema = create.DBconfig.from.TextGrid(fpl[1,2], dbName, basePath,tierNames)
  # set transient values
  schema=.update.transient.schema.values(schema)
  
  # create db object
  db=create.database(name = schema[['name']],basePath = normalizePath(basePath),DBconfig = schema)
  
  # set editable + showHierarchy
  schema$EMUwebAppConfig$activeButtons=list(saveBundle=TRUE,
                                           showHierarchy=TRUE)
  sdbUUID=schema[['UUID']]
  add_emuDBhandle(dbName,basePath,sdbUUID)
  dbsDf=dbGetQuery(get_emuDBcon(sdbUUID),paste0("SELECT * FROM emuDB WHERE uuid='",sdbUUID,"'"))
  if(nrow(dbsDf)>0){
    stop("EmuDB '",dbsDf[1,'name'],"', UUID: '",dbsDf[1,'uuid'],"' already loaded!")
  }
  
  # store to tmp DBI
  .store.emuDB.DBI(get_emuDBcon(sdbUUID), db)
  
  # store db schema file
  .store.DBconfig(get_emuDBcon(sdbUUID), basePath,schema)
  
  # get dbObj
  dbUUID = get_emuDB_UUID(dbName = dbName, dbUUID = NULL)
  
  # allBundles object to hold bundles without levels and links
  allBundles = list()
  
  # create session entry
  dbGetQuery(get_emuDBcon(dbUUID), paste0("INSERT INTO session VALUES('", dbUUID, "', '0000')"))
  
  
  # loop through fpl
  for(i in 1:dim(fpl)[1]){
    
    # create session name
    sesName = gsub('^_', '', gsub(.Platform$file.sep, '_', gsub(normalizePath(dir, winslash = .Platform$file.sep),'',dirname(normalizePath(fpl[i,1], winslash = .Platform$file.sep)))))
    
    # session file path
    if(sesName == ""){
      sfp=file.path(basePath,paste0("0000",session.suffix))
    }else{
      sfp=file.path(basePath,paste0(sesName,session.suffix))
    }
    if(!dir.exists(sfp)){
      res=dir.create(sfp)
      if(!res){
        # purge tmp emuDB
        purge_emuDB(dbUUID=dbUUID,interactive=F)
        stop("Could not create session directory: ",sfp," !\n")
      }
    }
    
    # media file
    mfPath=fpl[i,1]
    mfBn=basename(mfPath)
    
    # get sampleRate of audio file
    asspObj = read.AsspDataObj(mfPath)
    sampleRate=attributes(asspObj)$sampleRate
    # create bundle name
    bndlName = file_path_sans_ext(basename(fpl[i,1]))
    
    # create bundle entry
    dbGetQuery(get_emuDBcon(dbUUID), paste0("INSERT INTO bundle VALUES('", dbUUID, "', '0000', '", bndlName, "', '", mfBn, "', ", sampleRate, ", 'NULL')"))
    #b=create.bundle(bndlName,sessionName = '0000',annotates=basename(fpl[i,1]),sampleRate = sampleRate)
    
    
    ## create track entry
    #dbGetQuery(get_emuDBcon(), paste0("INSERT INTO track VALUES('", dbUUID, "', '0000', '", bndlName, "', '", fpl[i,1], "')"))
    
    # parse TextGrid
    parse.textgrid(fpl[i,2], sampleRate, dbName=dbName, bundle=bndlName, session="0000")
    
    # remove unwanted levels
    if(!is.null(tierNames)){
      
      condStr = paste0("level!='", paste0(tierNames, collapse = paste0("' AND ", " level!='")), "'")
      
      # delete items
      dbSendQuery(get_emuDBcon(dbUUID), paste0("DELETE FROM items WHERE ", "db_uuid='", dbUUID, "' AND ", condStr))
      
      # delete labels
      dbSendQuery(get_emuDBcon(dbUUID), paste0("DELETE FROM labels", 
                                         " WHERE ", "db_uuid='", dbUUID, "' AND itemID NOT IN (SELECT itemID FROM items)"))
    }
    
    # validate bundle
    valRes = validateBundle.emuDB.DBI(dbName, session='0000', bundle=bndlName)
    
    if(valRes$type != 'SUCCESS'){
      stop('Parsed TextGrid did not pass validator! The validator message is: ', valRes$message)
    }
    b=get.bundle(sessionName='0000',bundleName=bndlName,dbUUID=dbUUID)
    bDir=paste0(b[['name']],bundle.dir.suffix)
    bfp=file.path(sfp,bDir)
    res=dir.create(bfp)
    if(!res){
      # purge tmp emuDB
      purge_emuDB(dbUUID=dbUUID,interactive = F)
      stop("Could not create bundle directory ",bfp," !\n")
    }
    pFilter=emuR.persist.filters.bundle
    bp=marshal.for.persistence(b,pFilter)
    
    # store media file
    newMfPath=file.path(bfp,mfBn)
    if(file.exists(mfPath)){
      file.copy(from=mfPath,to=newMfPath)
    }else{
      stop("Media file :'",mfPath,"' does not exist!")
    }
    
    # and metadata (annotations)
    ban=str_c(b[['name']],bundle.annotation.suffix,'.json')
    baJSONPath=file.path(bfp,ban)
    pbpJSON=jsonlite::toJSON(bp,auto_unbox=TRUE,force=TRUE,pretty=TRUE)
    writeLines(pbpJSON,baJSONPath)
    
    # update pb
    if(verbose){
      setTxtProgressBar(pb, i)
    }
    
  }
  
  # store newly generated emuDB
  if(verbose){
    cat('\n') # hack to have newline after pb
  }
  
  # purge tmp emuDB
  purge_emuDB(dbUUID=dbUUID,interactive=F)
  
}

# FOR DEVELOPMENT
# library('testthat')
# test_file('tests/testthat/test_convert.TextGridCollection.R')
