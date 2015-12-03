##' Import media files to emuDB
##' @description Import media files to emuDB
##' @param dbName name of emuDB
##' @param dir directory containing mediafiles or session directories
##' @param targetSessionName name of session in which to create the new bundles 
##' @param dbUUID optional UUID of emuDB
##' @param verbose display infos & show progress bar
##' @author Klaus Jaensch
##' @import stringr
##' @keywords emuDB database Emu
##' @export
##' @examples
##' \dontrun{
##' ## Add mediafiles from directory
##' 
##'  import_mediaFiles('myEmuDB',dir="/data/mymedia/")
##' 
##' }
import_mediaFiles<-function(dbName,dir,targetSessionName='0000',dbUUID=NULL, verbose=TRUE){
  db=.load.emuDB.DBI(uuid = dbUUID,name=dbName)
  dbCfg=db[['DBconfig']]
  dbUUID=dbCfg['UUID']
  if(is.null(dbCfg[['mediafileExtension']])){
    pattern=NULL
    #stop("The DB has no media file extension defined.")
  }else{
    pattern=paste0('.*[.]',dbCfg[['mediafileExtension']],'$')
  }
  mfList=list.files(dir,pattern=pattern)
  if(length(mfList)>0){
    # create session dir and session list object if required
    sessDir=file.path(db[['basePath']],paste0(targetSessionName,session.suffix))
    if(!file.exists(sessDir)){
      dir.create(sessDir)
    }
    
    qSessSql=paste0("SELECT * FROM session WHERE db_uuid='",dbUUID,"' AND name='",targetSessionName,"'")
    sessDf<-dbGetQuery(get_emuDBcon(dbUUID),qSessSql)
    if(nrow(sessDf)==0){
      .store.session.DBI(dbUUID = dbUUID,sessionName = targetSessionName)
    }
    
  }
  mediaAdded=FALSE
  
  progress = 0
  if(verbose){
    cat("INFO: Importing ", length(mfList), " media files...\n")
    pb = txtProgressBar(min = 0, max = length(mfList), initial = progress, style=3)
    setTxtProgressBar(pb, progress)
  }
  
  for(mf in mfList){
    mfFullPath=file.path(dir,mf)
    bundleName=sub('[.][^.]*$','',mf)
    
    bundleDir=file.path(sessDir,paste0(bundleName,bundle.dir.suffix))
    dir.create(bundleDir)
    newMediaFileFullPath=file.path(bundleDir,mf)
    file.copy(from = mfFullPath,to=newMediaFileFullPath)
    
    pfAssp=read.AsspDataObj(newMediaFileFullPath,0,4000)
    sampleRate=attr(pfAssp,'sampleRate')
    b=create.bundle(name = bundleName,sessionName = targetSessionName,annotates=mf,sampleRate=sampleRate)
    b[['session']]=targetSessionName
    .store.bundle.DBI(database = db,bundle=b)
    #db[['sessions']][[targetSessionName]][['bundles']][[bundleName]]=b
    # TODO generate empty annotation and SSFFtracks if required
    annoAdded=FALSE
    b[['levels']]=list()
    
    for(ld in dbCfg[['levelDefinitions']]){
      
      b[['levels']][[ld[['name']]]]=create.bundle.level(name=ld[['name']],type = ld[['type']],items = list())
      
      ## TODO TEST only
      #labelAttrs=list(list(name=ld[['name']],value='Test Huhu!'))
      #b[['levels']][[ld[['name']]]][['items']][[1]]=create.interval.item(id = 1,sampleStart = 0,sampleDur = 50000,labels = labelAttrs )
      #db=move.bundle.levels.to.data.frame(db,b)
      annoAdded=TRUE
    }
    if(annoAdded){
      store.bundle.annotation(bundle = b,dbUUID = dbUUID)
    }
    mediaAdded=TRUE
    
    # update pb
    progress = progress + 1
    if(verbose){
      setTxtProgressBar(pb, progress)
    }
    
  }
  
  perspectives=dbCfg[['EMUwebAppConfig']][['perspectives']]
  # create an EMUwebapp default perspective if media has been added 
  if(mediaAdded & (is.null(perspectives) | length(perspectives)==0)){
    sc=create.EMUwebAppConfig.signalCanvas(order=c("OSCI","SPEC"),assign=list(),contourLims=list())
    defPersp=create.EMUwebAppConfig.perspective(name='default',signalCanvases=sc,levelCanvases=list(order=list()),twoDimCanvases=list(order=list()))
    db[['DBconfig']][['EMUwebAppConfig']][['perspectives']]=list(defPersp)
    #.store.DBconfig.DBI(DBconfig = db[['DBconfig']])
    .store.schema(db = db)
  }
  return(invisible(NULL))
}




###################################################
# CRUD operations for files


##' Add files to emuDB
##' 
##' Add files to bundles of specified session of emuDB.
##' The files that are found in "dir" that have the extension 
##' "fileExtension" will be copied into the the according bundle
##' folder that have the same basename as the according file. For 
##' more information on the structural elements of an emuDB 
##' see \code{vignette{emuDB}}.
##' 
##' @param dbName name of loaded emuDB
##' @param dir directory containing files to be added
##' @param fileExtension file extension of file to be added
##' @param targetSessionName name of sessions containing 
##' bundles that the files will be added to
##' @param dbUUID optional UUID of loaded emuDB
##' @export
##' @keywords emuDB database Emu 
##' @examples 
##' \dontrun{
##' 
##' ##################################
##' # prerequisite: loaded "ae" emuDB 
##' # (see ?load_emuDB for more information)
##' 
##' # specify path to folder containing the following
##' # files we wish to add to: 
##' # msajc003.zcr, msajc010.zcr, msajc012.zcr, msajc015.zcr, 
##' # msajc022.zcr, msajc023.zcr and msajc057.zcr 
##' path2dir = "/path/to/dir/"
##' 
##' # add the files to session "0000" of "ae" emuDB
##' add_files(dbName = "ae",
##'           dir = path2dir,
##'           fileExtension = "zcr",
##'           targetSessionName = "0000")
##' 
##' }
add_files <- function(dbName, dir, fileExtension, targetSessionName='0000', dbUUID=NULL){
  
  dbUUID = get_emuDB_UUID(dbName = dbName, dbUUID = dbUUID)
  dbObj = .load.emuDB.DBI(name = dbName, uuid = dbUUID)
  
  # get all basePath + bundles
  dbHandle=get_emuDBhandle(dbUUID)
  bp=dbHandle$basePath
  
  bndls = list_bundles(dbName, session = targetSessionName, dbUUID=dbUUID)
  
  sourcePaths = list.files(dir, pattern = paste0(fileExtension, '$'), full.names = T)
  
  destDirs = file.path(bp, paste0(bndls$session, '_ses'), paste0(bndls$name, '_bndl'))
  
  # copy files
  for (i in 1:length(sourcePaths)){
    cbn = basename(file_path_sans_ext(sourcePaths[i]))
    cbndl = bndls[bndls$name == cbn, ]
    # check that only one bundle folder
    if(nrow(cbndl) != 1){
      stop(paste0("more or less then one bundle found that matches the base name of the file '", sourcePaths[i], "'"))
    }
    
    destDir = file.path(bp, paste0(cbndl$session, '_ses'), paste0(cbndl$name, '_bndl'))
    file.copy(sourcePaths[i], destDirs[i])
  }
}

##' List files of emuDB
##' 
##' @param dbName name of loaded emuDB
##' @param sessionPattern A (glob) pattern matching sessions to be searched from the database
##' @param bundlePattern A (glob) pattern matching bundles to be searched from the database
##' @param dbUUID optional UUID of loaded emuDB
##' @author Raphael Winkelmann
##' @export
##' @keywords emuDB database Emu 
list_files <- function(dbName,
                       sessionPattern = "*",
                       bundlePattern = "*",
                       dbUUID = NULL){
  dbUUID = get_emuDB_UUID(dbName = dbName, dbUUID = dbUUID)
  dbObj = .load.emuDB.DBI(name = dbName, uuid = dbUUID)
  
  # get all basePath + bundles
  dbHandle=get_emuDBhandle(dbUUID)
  bp=dbHandle$basePath
  
  bndls = list_bundles(dbName, dbUUID = dbUUID)
  
  df = data.frame(session = character(), 
                  bundle = character(),
                  file = character(),
                  stringsAsFactors = F)
  # get files for each bundle
  for(i in 1:nrow(bndls)){
    
    fps = list.files(file.path(bp, paste0(bndls[i,]$session, "_ses"), paste0(bndls[i,]$name, "_bndl")))
    df = rbind(df, data.frame(session = rep(bndls[i,]$session, length(fps)), 
                              bundle = rep(bndls[i,]$name, length(fps)), 
                              file = fps,
                              stringsAsFactors = F))  
  }
  
  # filter for patterns
  df = df[grepl(glob2rx(sessionPattern), df$session) & grepl(glob2rx(bundlePattern), df$bundle),]
  
  return(df)
  
}

modify_files <- function(){
  stop('not implemented yet')
}

remove_files <- function(){
  stop('not implemented yet')
}


#########################
# FOR DEVELOPMENT
# library('testthat')
# test_file('tests/testthat/test_database.files.R')

