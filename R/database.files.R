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
  if(is.null(dbCfg[['mediaFileExtension']])){
    pattern=NULL
    #stop("The DB has no media file extension defined.")
  }else{
    pattern=paste0('.*[.]',dbCfg[['mediaFileExtension']],'$')
  }
  mfList=list.files(dir,pattern=pattern)
  if(length(mfList)>0){
    # create session dir and session list object if required
    sessDir=file.path(db[['basePath']],paste0(targetSessionName,session.suffix))
    if(!file.exists(sessDir)){
      dir.create(sessDir)
    }
    
    qSessSql=paste0("SELECT * FROM session WHERE db_uuid='",dbUUID,"' AND name='",targetSessionName,"'")
    sessDf<-dbGetQuery(getEmuDBcon(),qSessSql)
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
    b=create.bundle(name = bundleName,sessionName = targetSessionName,mediaFilePath = newMediaFileFullPath,annotates=mf,sampleRate=sampleRate)
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


##' Add files to bundles
##' 
##' Matches the basenames of all files found in path2rootDir
##' with the according mediafiles contained in the session
##' folder specified and copies the files into the according
##' _bndl folders.
##' 
##' @param path2rootDir path to directory containing files that should be added
##' @param path2sessionDir path to session directory containing the destination bundles
##' @param fileExt file extention of the files that should be added to the bundles
##' @param mediafileExtension file extention of media files of emuDB (default='wav')
##' @export
##' @author Raphael Winkelmann
##'
add_files_to_bundles <- function(path2rootDir, path2sessionDir, 
                                 fileExt, mediafileExtension = 'wav'){
  
  # gernerate file pail list
  fpl = create_filePairList(path2sessionDir, path2rootDir, mediafileExtension, fileExt)
  
  # extract sourcePaths and destDirs
  sourcePaths = fpl[,2]
  destDirs = dirname(fpl[,1])
  
  # copy files
  for (i in 1:length(sourcePaths)){
    file.copy(sourcePaths[i], destDirs[i])
  }
} 


#########################
# FOR DEVELOPMENT
# library('testthat')
# test_file('tests/testthat/test_database.files.R')

