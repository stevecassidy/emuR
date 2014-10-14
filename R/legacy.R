build.legacy.bundle.list<-function(parsedEmuPath,currentPath=NULL,fileSuffixPattern,bundleList=list()){
  if(length(parsedEmuPath)==0){
    #fileGlobPatt=paste0("*",fileSuffix)
    #fileRegexPatt=glob2rx(fileGlobPatt)
    fileRegexPattern=paste0('^.*',fileSuffixPattern)
    fileList = list.files(currentPath, pattern=fileRegexPattern, recursive=F, full.names=F)
    if(length(fileList)==0){
      return(NULL)
    }else{
      # TODO convert to bundle names
      res=list()
      for(f in fileList){
        # remove file suffix for bundle name
        bundleName=gsub(x = f,pattern=fileSuffixPattern,replacement='')
        res[[length(res)+1]]=bundleName
      }
      return(res)
    }
  }else{
    p1=parsedEmuPath[[1]]
    if(length(parsedEmuPath)==1){
      restPath=list()
    }else{
      restPath=parsedEmuPath[2:length(parsedEmuPath)]
    }
    if(p1[['pattern']]){
      dirs=list.dirs(currentPath,full.names=FALSE,recursive=FALSE)
      bll=list()
      for(dir in dirs){
        dirPatt=gsub('*','.*',p1[['dir']],fixed=TRUE)
        if(grepl(dirPatt,dir)){
          newPath=file.path(currentPath,dir)
          bl=build.legacy.bundle.list(restPath,newPath,fileSuffixPattern,bundleList=bundleList)
          # prepend dir to list
          bl=lapply(bl,function(x,s) return(c(s,x)),dir)
          bll=c(bll,bl)
        }
       
      }
      return(bll)
    }else{
      if(is.null(currentPath)){
        newPath=p1[['dir']]
      }else{
        newPath=file.path(currentPath,p1[['dir']])
      }
      
      bll=build.legacy.bundle.list(restPath,newPath,fileSuffixPattern,bundleList=bundleList)
      return(bll)
    }
  }
  
}

convert.legacy.bundle.id<-function(bundleId){
  # takes character vector of legacy globpattern dirs and bundle name
  # and converts to session and bundle
  # example: bundleID: "BLOCK10","SES1000","foo_42" -> "BLOCK10_SES1000","foo_42"
  
  bundleIdLen=length(bundleId)
  globPatternCount=bundleIdLen-1
  
  if(globPatternCount>0){
    # collapse globpattern matches to one session ID
    s=paste(bundleId[1:globPatternCount],collapse='_')
    
  }else{
    # no glob patterns, put all bundles to dummy session
    s='0000'
    
  }
  return(c(s,bundleId[bundleIdLen]))
}

convert.legacy.bundle.list.to.sessions<-function(bl){
  sessions=list()
  
  if(!is.null(bl) & length(bl)>0){
    globPatternCount=length(bl[[1]])-1
    
    if(globPatternCount>0){
      createSessName=function(x,toCol){
        return(paste(x[1:toCol],collapse='_'))
      }
      sesssNonUnique=lapply(bl,createSessName,toCol=globPatternCount)
      sessions=unique(sesssNonUnique)
    }else{
      # no glob patterns, put all bundles to dummy session
      sessions[['0000']]=list(bundles=bl)
    }
  }
  return(sessions)
}

get.legacy.emu.bundles=function(basePath,pathPattern,primaryFileSuffixPattern=NULL){
  if(is.relative.file.path(pathPattern)){
    absPathPattern=file.path(basePath,pathPattern)
  }else{
    absPathPattern=pathPattern
  }
  emuParsedPathPattern=parse.emuTrackPath(absPathPattern)
  bl=build.legacy.bundle.list(emuParsedPathPattern[['dirs']],fileSuffixPattern=primaryFileSuffixPattern)
  return(bl)
}

get.legacy.file.path=function(basePath,emuPath,bundleId,fileExtension){
  if(is.relative.file.path(emuPath)){
    absPathPattern=file.path(basePath,emuPath)
  }else{
    absPathPattern=emuPath
  }
  pp=parse.emuTrackPath(absEmuTrackPath = absPathPattern)
  path=NULL
  bIdIdx=1
  for(pdl in pp[['dirs']]){
   
    if(pdl[['pattern']]){
      # substitute
      dir=bundleId[bIdIdx]
      bIdIdx=bIdIdx+1
    }else{
      dir=pdl[['dir']]
    }
    if(is.null(path)){
      path=dir
    }else{
      path=file.path(path,dir)
    }
  }
  filename=paste0(bundleId[bIdIdx],'.',fileExtension)
  return(file.path(path,filename))
}