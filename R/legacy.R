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

convert.legacy.bundle.id<-function(legacybundleID){
  # takes character vector of legacy globpattern dirs and bundle name
  # and converts to session and bundle
  # examples: 
  # legacybundleID: "BLOCK10","SES1000","foo_42" -> "BLOCK10_SES1000","foo_42"
  # legacybundleID: "SES1000","foo_42" -> "SES1000","foo_42"
  # legacybundleID: "foo_42" -> "0000","foo_42"
  
  legacybundleIDLen=length(legacybundleID)
  globPatternCount=legacybundleIDLen-1
  
  if(globPatternCount>0){
    # collapse globpattern matches to one session ID
    s=paste(legacybundleID[1:globPatternCount],collapse='_')
    
  }else{
    # no glob patterns, put all bundles to dummy session
    s='0000'
    
  }
  return(c(s,legacybundleID[legacybundleIDLen]))
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


convert.emuTrackPath <- function(absEmuTrackPath){
  # Emu track path may have asterisks for pattern matching e.g.
  # wav E:/KielCorpusRead/*/*/*/*
  # to use this pattern with R regex we have to convert to regular expression
  epSpl=strsplit(absEmuTrackPath,'/')[[1]]
  pp=c()
  cDir=NULL
  lastIsAsterisk=FALSE
  for(epDir in epSpl){
    #if(epDir!='*'){
    if(!grepl('*',epDir)){
      # Bug this condition block is never reached!!
      # The regex substitution is done is list.trackdirs
      lastIsAsterisk=FALSE
      #if(epDir==''){
      #  cDir='/'
      #}else{
      if(epDir!=''){
        if(is.null(cDir)){
          cDir=epDir
        }else{
          sep='/'
          if(length(gl <- grep('/$', cDir))){
            sep=''
          }
          cDir=paste(cDir,epDir,sep=sep)
        }
      }else{
        cDir='/'
      }
    }else{
      lastIsAsterisk=TRUE
      
      pp=c(pp,cDir,epDir)
      cDir=NULL
    }
  }
  if(!lastIsAsterisk){
    pp=c(pp,cDir)
  }
  return(pp)
}

parse.emuTrackPath <- function(absEmuTrackPath){
  # Emu track path may have asterisks for pattern matching e.g.
  # wav E:/KielCorpusRead/*/*/*/*
  # to use this pattern with R regex we have to convert to regular expression
  epSpl=strsplit(absEmuTrackPath,'/+')[[1]]
  topo=list()
  wildcardDirLevelCount=0
  pp=list()
  cDir=NULL
  lastIsAsterisk=FALSE
  for(epDir in epSpl){
    #if(epDir!='*'){
    if(!grepl('[*]',epDir)){
      lastIsAsterisk=FALSE
      #if(epDir==''){
      #  cDir='/'
      #}else{
      if(epDir!=''){
        if(is.null(cDir)){
          cDir=epDir
        }else{
          sep='/'
          if(length(gl <- grep('/$', cDir))){
            sep=''
          }
          cDir=paste(cDir,epDir,sep=sep)
        }
      }else{
        cDir='/'
      }
    }else{
      lastIsAsterisk=TRUE
      
      if(!is.null(cDir)){
        pp[[length(pp)+1]]=list(dir=cDir,pattern=FALSE)
      } 
      cDir=NULL
      
      #epDirRegexPattern=
      pp[[length(pp)+1]]=list(dir=epDir,pattern=TRUE)
      wildcardDirLevelCount=wildcardDirLevelCount+1
      
      
    }
  }
  if(!lastIsAsterisk){
    #pp=c(pp,cDir)
    pp[[length(pp)+1]]=list(dir=cDir,pattern=FALSE)
  }
  topo[['dirs']]=pp
  topo[['patternCount']]=wildcardDirLevelCount
  
  return(topo)
}

## List directories to search for track files
## @description emu track pathes syntax allows asterisk wildcard pattern for path. This function goes through the directory hierarchy and returns list with all directories matching the pattern 
## @param emuPath Emu path specification (may contain asterisk wildcards) type character
## @param parsedEmuPathPattern character vector containing the parsed segments of the path. Each segment is a dierctory or a wildacrd asterisk.
## @return character vector of absolute path directories 
## @author Klaus Jaensch
## @keywords emuDB bundle Emu
## 
list.trackdirs<-function(emuPath=NULL,parsedEmuPathPattern=NULL){
  if(is.null(parsedEmuPathPattern)){
    if(is.null(emuPath)){
      stop("At least one of the parameters emuPath or parsedEmuPathPattern is required.")
    }
    parsedEmuPathPattern=convert.emuTrackPath(emuPath)
  }
  cDir=NULL
  dirLevels=length(parsedEmuPathPattern)
  res=c()
  for(i in 1:dirLevels){
    ettp=parsedEmuPathPattern[i]
    lastLevel=(i==dirLevels)
    if(!grepl('[*]',ettp)){
      if(is.null(cDir)){
        cDir=ettp
      }else{
        cDir=file.path(cDir,ettp)
      }
      if(lastLevel){
        return(cDir)
      }
    }else{
      
      dirs=list.dirs(cDir,recursive=FALSE)
      for(dir in dirs){
        dirPatt=gsub('*','.*',ettp,fixed=TRUE)
        if(grepl(dirPatt,dir)){
          newPattern=c(dir)
          if(!lastLevel){
            for(j in (i+1L):dirLevels){
              newPattern=c(newPattern,parsedEmuPathPattern[j])
            }
          }
          wcRes=list.trackdirs(parsedEmuPathPattern=newPattern)
          res=c(res,wcRes)
        }
      }
      return(res)
    }
  }
  return(cDir)
}

list.emuTemplatePathes<-function(){
  # check if path is set
  emuTemplatePath=Sys.getenv('EMU_TEMPLATE_PATH')
  if(is.null(emuTemplatePath) | ''==emuTemplatePath){
    emuConfFile=NULL
    homePath=Sys.getenv('HOME')
    if(!is.null(homePath) & '' != homePath){
      emuConfFile=file.path(homePath,'.emu','emu-conf')
      if(!file.exists(emuConfFile)){
        emuConfFile=file.path(homePath,'.emu','Emu','emu-conf')
      }
    }
    osInfo=Sys.info()
    isWindos=FALSE
    if(!is.null(osInfo)){
      isWindos=('Windows'==osInfo[['sysname']])
    }
    if(isWindos & (is.null(emuConfFile) | !file.exists(emuConfFile))){
      # Windows 7
      userProfile=Sys.getenv('USERPROFILE')
      emuConfFile=file.path(userProfile,'.emu','Emu','emu-conf')
      #cat("emu conf",emuConfFile,"\n")
    }
    
    if(!is.null(emuConfFile) & file.exists(emuConfFile)){
      
      lc = try(readLines(emuConfFile,warn=FALSE))
      if(class(lc) == "try-error") {
        stop("Cannot read ",emuConfFile)
      }
      for(l in lc){
        
        kv=parse.line.to.key.value(l)
        if(!is.null(kv)){
          if(kv[1]=='#EMU_TEMPLATE_PATH'){
            etpSpl=strsplit(kv[2],.Platform[['path.sep']])
            return(etpSpl[[1]])
            
          }
        }
      }
    }
    
  }
}

##' List known database names from legacy EMU installation
##' @return character vector with database names
##' @author Klaus Jaensch
##' @seealso \code{\link{list.legacy.emu.databases}} \code{\link{convert.legacyEmuDB.by.name.to.emuDB}} 
##' @export
##' @keywords database legacy Emu
##' @examples
##' \dontrun{
##' ## List legacy EMU known database names
##' 
##' list.legacy.emu.database.names()
##' 
##' }
##' 
list.legacy.emu.database.names<-function(){
  return(names(list.legacy.emu.databases()))
}

##' List known databases from legacy EMU installation
##' Reads EMU_TEMPLATE_PATH variable from environment or from file ${HOME}/.emu/emu-conf or ${USERPROFILE}/.emu/Emu/emu-conf
##' and searches for *.tlp template files in this path. The basename of the template file is the database name.
##' @return named list with pathes to database template files. The names of the list are the database names
##' @author Klaus Jaensch
##' @seealso \code{\link{list.legacy.emu.database.names}}
##' @export
##' @keywords database legacy Emu
##' @examples
##' \dontrun{
##' ## List legacy EMU known databases
##' 
##' list.legacy.emu.databases()
##' 
##' }
##' 

list.legacy.emu.databases<-function(){
  lEmuDbs=list()
  templPathes=list.emuTemplatePathes()
  if(!is.null(templPathes)){
    for(templPath in templPathes){
      tplFiles=list.files(templPath,'.*[.][tT][pP][lL]$')
      for(tplFile in tplFiles){
        tplBasename = basename(tplFile)
        dbName=gsub("[.][tT][pP][lL]$","",tplBasename)
        lEmuDbs[[dbName]]=file.path(templPath,tplFile)
      }
    }
  }
  return(lEmuDbs)
}

list.file.matching.emu.path.pattern=function(basePath,pathPattern,filePattern=NULL){
  if(is.relative.file.path(pathPattern)){
    absPathPattern=file.path(basePath,pathPattern)
  }else{
    absPathPattern=pathPattern
  }
  dirList=list.trackdirs(absPathPattern)
  fileList=c()
  for(dir in dirList){
    pFileList = list.files(dir, pattern=filePattern, recursive=T, full.names=T)
    fileList=c(fileList,pFileList)
  }
  return(fileList)
}

find.file.in.emu.path.pattern=function(emuPathPattern,fileName,basePath=NULL){
  if(is.relative.file.path(emuPathPattern)){
    absPathPattern=file.path(basePath,emuPathPattern)
  }else{
    absPathPattern=emuPathPattern
  }
  dirList=list.trackdirs(absPathPattern)
  for(dir in dirList){
    tfp=paste0(dir,'/',fileName)
    if(file.exists(tfp)){
      return(tfp)
    }
  }
  return(NULL)
}

get.legacy.file.path=function(basePath,emuPath,legacybundleID,fileExtension){
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
      dir=legacybundleID[bIdIdx]
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
  filename=paste0(legacybundleID[length(legacybundleID)],'.',fileExtension)
  return(file.path(path,filename))
}

## @import stringr wrassp
load.annotation.for.legacy.bundle=function(schema,legacyBundleID,basePath=NULL,encoding=NULL){
  
  newBundleId=convert.legacy.bundle.id(legacyBundleID)
  bundleName=newBundleId[2]
  sessionName=newBundleId[1]
  # determine samplerate
  # fallback is primary file
  sampleRateReferenceFile=NULL
  sampleTrackFile=NULL
  if(!is.null(schema[['mediafileBasePathPattern']]) && ! is.null(schema[['mediafileExtension']])){
    # use samples track to determine sample rate
    
    ## resolve wildcards
    #sampleRateReferenceFile=find.file.in.emu.path.pattern(emuPathPattern=schema[['mediafileBasePathPattern']],fileName=sampleTrackFile,basePath)
    sampleRateReferenceFile=get.legacy.file.path(basePath,emuPath=schema[['mediafileBasePathPattern']],legacyBundleID,fileExtension=schema[['mediafileExtension']])
  }
  if(is.null(sampleRateReferenceFile)){
    stop("Could not determine media sample rate of bundle ID ",paste(legacyBundleID,collapse='_'),"\n")
  }else{
    # TODO ASSP does not return good error messages if an IO error (not exist, permission dnied ,etc...) occurs
    # TODO test file access first
    #cat("Determine sample rate from file: ",sampleRateReferenceFile,"\n")
    pfAssp=read.AsspDataObj(sampleRateReferenceFile,0,4000)
    sampleRate=attr(pfAssp,'sampleRate')
  }
  
  # create signal paths list
  
  signalpaths=list() 
  for(tr in schema[['tracks']]){
    #cat("Track: ",tr$name," ",tr$fileExtension,"\n")
    #sigFilename=str_c(uttCode,'.',tr[['fileExtension']])
    #sFile=find.file.in.emu.path.pattern(tr[['basePath']],sigFilename,basePath)
    sFile=get.legacy.file.path(basePath=basePath,emuPath=tr[['basePath']],legacyBundleID,fileExtension=tr[['fileExtension']])
    if(!is.null(sFile)){
      signalpaths[[length(signalpaths)+1L]]=sFile
    }
  }
  idCnt=0
  levels=list()
  links=list()
  
  #  ESPS label files first
  for(ad in schema[['annotationDescriptors']]){
    extension=ad[['extension']]
    #cat("Anno ext: ",extension,ad$basePath,"\n")
    annoBasePath=NULL
    if(is.null(ad[['basePath']])){
      #annoBasePath=schema$
      # TODO use same as primary track
    }else{
      annoBasePath=ad[['basePath']]
      # Emu: assume that files reside in this directory (no recursive search)
      
      #annoFilename=str_c(uttCode,'.',extension)
      #annoPath=find.file.in.emu.path.pattern(annoBasePath,annoFilename,basePath)
      annoPath=get.legacy.file.path(basePath=basePath,emuPath=ad[['basePath']],legacyBundleID,fileExtension=extension)
      if(!is.null(annoPath)){
        #cat("Anno: ",annoPath,"\n")
        if(extension!='hlb'){
          # parse lab file
          if(file.exists(annoPath)){
            labTier=parse.esps.label.file(labFilePath=annoPath,tierName=ad[['name']],tierType=ad[['type']],encoding=encoding,sampleRate=sampleRate,idCnt=idCnt)
            if(!is.null(labTier)){
              levels[[labTier[['name']]]] <- labTier
              labTierItemCnt=length(labTier[['items']])
              idCnt=idCnt+labTierItemCnt
            }
          }else{
            # warning ??
          }
        }
      }
    }
    
  }
  # now hlb file
  for(ad in schema[['annotationDescriptors']]){
    extension=ad[['extension']]
    #cat("Anno ext: ",extension,ad$basePath,"\n")
    #annoBasePath=NULL
    if(is.null(ad[['basePath']])){
      #annoBasePath=schema$
      # TODO use same as primary track
    }else{
      annoBasePathEmu=ad[['basePath']]
      # resolve wildcards
      #annoFilename=str_c(uttCode,'.',extension)
      #annoPath=find.file.in.emu.path.pattern(annoBasePathEmu,annoFilename,basePath)
      #if(!is.null(annoPath)){
      #cat("Anno: ",annoPath,"\n")
      if(extension=='hlb'){
        #cat("Parse hlb file:",annoPath,"\n")
        hlbFilePath=get.legacy.file.path(basePath=basePath,emuPath=annoBasePathEmu,legacyBundleID,fileExtension=extension)
        if(file.exists(hlbFilePath)){
          hlbParseResult=parse.hlb.file(hlbFilePath=annoPath,levelDefinitions=schema[['levelDefinitions']],levels=levels,encoding=encoding);
          hlbTiers=hlbParseResult[['hlbTiers']]
          links=hlbParseResult[['links']]
          # sort levels
          lIdx=0
          sortedLevels=list()
          
          for( ld in schema[['levelDefinitions']]){
            lIdx=lIdx+1L
            for(hlbTier in hlbTiers){
              if(ld[['name']]==hlbTier[['name']]){
                sortedLevels[[hlbTier[['name']]]] <- hlbTier
                break;
              }
            }
          }
          levels=sortedLevels
        }else{
          #cat("Warning: HLB file: ",hlbFilePath," does not exist!\n")
        }
      }
      #}
    }
    
  }
  
  bundleSampleRate=NULL
  for(l in levels){
    lvlSr=l[['sampleRate']]
    if(is.null(bundleSampleRate)){
      if(!is.null(lvlSr)){
        bundleSampleRate=lvlSr
      }
      
    }else{
      if(!is.null(lvlSr) && lvlSr!=bundleSampleRate){
        cat("WARNING: Levels have different sample rates!\n")
      }
    }
  }
  #annotates=paste0('0000_ses/',uttCode,bundle.dir.suffix,'/',sampleTrackFile)
  
  sampleTrackFile=paste0(bundleName,'.',schema[['mediafileExtension']]) 
  #annotates=paste0(sessionName,session.suffix,'/',newBundleId[2],bundle.dir.suffix,'/',sampleTrackFile)
  # bug #19
  annotates=paste0(sampleTrackFile)
  bundle=create.bundle(name=bundleName,sessionName=sessionName,legacyBundleID=legacyBundleID,annotates=annotates,sampleRate=bundleSampleRate,levels=levels,signalpaths=signalpaths,mediaFilePath=sampleRateReferenceFile,links=links)
  return(bundle)
}

remove.redundant.links<-function(database,links){
  # Legacy EMU and query functions link collections contain links for each possible connection between levels
  # We consider links that do not follow link definition constraints as redundant and therefore we remove them from the
  # link data model
  #
  # build SQL query from link definitions
  items=database[['items']]
  sqlQuery="SELECT l.* FROM items f,items t,links l WHERE f.bundle=t.bundle AND l.bundle=f.bundle AND f.session=t.session AND l.session=f.session AND f.itemID=l.fromID AND t.itemID=l.toID AND ("
  ldCnt=length(database[['DBconfig']][['linkDefinitions']])
  for(i in 1:ldCnt){
    ld=database[['DBconfig']][['linkDefinitions']][[i]]
    sqlQuery=paste0(sqlQuery,'(f.level=\'',ld[['superlevelName']],'\' AND t.level=\'',ld[['sublevelName']],'\')')
    if(i<ldCnt){
      sqlQuery=paste0(sqlQuery,' OR ')
    }
  }
  sqlQuery=paste0(sqlQuery,')')
  #cat(sqlQuery,"\n")
  return(sqldf(sqlQuery))
}

remove.database.redundant.links<-function(database){
  # Legacy EMU and query functions link collections contain links for each possible connection between levels
  # We consider links that do not follow link definition constraints as redundant and therefore we remove them from the
  # link data model
  #
  # build SQL query from link definitions
  items=database[['items']]
  links=database[['links']]
  linksCnt=nrow(links)
  if(linksCnt>0){
    sqlQuery="SELECT l.* FROM items f,items t,links l WHERE f.bundle=t.bundle AND l.bundle=f.bundle AND f.session=t.session AND l.session=f.session AND f.itemID=l.fromID AND t.itemID=l.toID AND ("
    ldCnt=length(database[['DBconfig']][['linkDefinitions']])
    if(ldCnt>0){
      for(i in 1:ldCnt){
        ld=database[['DBconfig']][['linkDefinitions']][[i]]
        sqlQuery=paste0(sqlQuery,'(f.level=\'',ld[['superlevelName']],'\' AND t.level=\'',ld[['sublevelName']],'\')')
        if(i<ldCnt){
          sqlQuery=paste0(sqlQuery,' OR ')
        }
      }
      sqlQuery=paste0(sqlQuery,')')
      #cat(sqlQuery,"\n")
      database[['links']]=sqldf(sqlQuery)
    }
  }
  return(database)
}

## Load legacy EMU database by name
## 
## @param dbName legacy EMU database name
## @param verboseLevel integer setting the verbosity level
## @param showProgress show progress bar
## @param encodign optional encoding of input files
## @return object of class emuDB
## @author Klaus Jaensch
## @import stringr wrassp
## 
load.database.from.legacy.emu.by.name=function(dbName,verboseLevel=0,showProgress=TRUE,encoding=NULL){
  
  emuDbsList=list.legacy.emu.databases()
  emuTplPath=emuDbsList[[dbName]]
  if(is.null(emuTplPath)){
    stop("Legacy EMU database '",dbName,"' could not be found.")
  }
  return(load.database.from.legacy.emu(emuTplPath=emuTplPath,verboseLevel=verboseLevel,showProgress=showProgress,encoding=encoding))
}


## Load legacy EMU database from EMU template (.tpl) file
## 
## @param emuTplPath EMU template file path
## @param verboseLevel integer setting the verbosity level
## @param showProgress show progress bar
## @param encoding optionla encoding if input files
## @return object of class emuDB
## @author Klaus Jaensch
## @import stringr wrassp
## 
load.database.from.legacy.emu=function(emuTplPath,verboseLevel=0,showProgress=TRUE,encoding=NULL){
  progress=0
  
  dbd=load.database.schema.from.emu.template(emuTplPath,encoding=encoding)
  if(verboseLevel>0){
    cat("Loaded database schema.\n")
  }
  progress=progress+1L
  
  tplBaseDir=NULL
  tplBaseDir=dirname(emuTplPath)
  
  db=create.database(name=dbd[['name']],basePath=tplBaseDir,DBconfig=dbd)
  db=initialize.database.dataframes(db)
  
  schema=db[['DBconfig']]
  # load primary track file list first
  # and find samples track to get sample rate
  primaryFileList = NULL
  
  primaryBasePath=NULL
  primaryFileExtension=NULL
  
  #pattern='*'
  primaryFileSuffixPattern=NULL
  # find primary and sample track paths
  for(tr in schema[['tracks']]){
    #cat("Track: ",tr$name," ",tr$fileExtension,"\n")
    if(tr[['fileExtension']]==schema[['flags']][['PrimaryExtension']]){
      
      primaryFileExtension=tr[['fileExtension']]
      primaryBasePath=tr[['basePath']]
      #if(!is.null(tr[['unitSuffix']])){
      #  pattern=str_c(pattern,tr[['unitSuffix']])
      #}
    }
  }
  
  if(is.null(primaryFileExtension)){
    for(ad in schema[['annotationDescriptors']]){
      if(ad[['extension']]==schema[['flags']][['PrimaryExtension']]){
        primaryFileExtension=ad[['extension']]
        primaryBasePath=ad[['basePath']]
        break
      }
    }
    
  }
  #pattern=str_c(pattern,'[.]',primaryFileExtension)
  primaryFileSuffixPattern=paste0('[.]',primaryFileExtension,'$')
  #primaryFileList=list.file.matching.emu.path.pattern(db[['basePath']],primaryBasePath,filePattern=pattern)
  legacyBundleIDsList=get.legacy.emu.bundles(db[['basePath']],primaryBasePath,primaryFileSuffixPattern)
  
  
  bundlesCount=length(legacyBundleIDsList)
  #utts=vector(mode='list',length=bundlesCount)
  us=1:bundlesCount
  if(showProgress){
    cat("INFO: Loading legacy EMU database containing",bundlesCount,"bundles...\n")
    pb=txtProgressBar(min=0,max=bundlesCount+7,initial=progress,style=3)
    
    setTxtProgressBar(pb,progress)
  }
  #uttNames=c()
  
  db[['itemsIdx']]=0L
  db[['labelsIdx']]=0L
  db[['linksIdx']]=0L
  
  for(ui in us){
    legacyBundleID=legacyBundleIDsList[[ui]]
    newBundleId=convert.legacy.bundle.id(legacyBundleID)
    sessionName=newBundleId[1]
    bundleName=newBundleId[2]
    if(is.null(db[['sessions']][[sessionName]])){
      # create session if needed
      db[['sessions']][[sessionName]]=list(name=sessionName,bundles=list())
      
    }
    ptrFilePath=get.legacy.file.path(db[['basePath']],primaryBasePath,legacyBundleID,primaryFileExtension)
    #ptrFilePath=primaryFileList[ui]
    #cat("Primary track file path: ",ptrFilePath,"\n")
    
    ptrFileBasename=basename(ptrFilePath)
    #cat("Ext: ",primaryTrackFileExtension,"\n")
    cutLen=str_length(primaryFileExtension)+1L
    cutPos=str_length(ptrFileBasename)-cutLen
    #cat("Cut: ",ptrFileBasename,cutLen,cutPos,"\n")
    #uttCode=substr(ptrFileBasename,1,cutPos)
    bundle=load.annotation.for.legacy.bundle(schema,legacyBundleID,db[['basePath']],encoding=encoding)
    
    # "inlining" of append.bundle.to.tmp.list improves performance for very large databases
    # (db object is not copied for each call)
    schema=db[['DBconfig']]
    maxLbls=db[['DBconfig']][['maxNumberOfLabels']]
    bName=bundle[['name']]
    for(lvl in bundle[['levels']]){
      
      seqIdx=as.integer(0)
      for(it in lvl[['items']]){
        db[['itemsIdx']]=db[['itemsIdx']]+1L
        row=db[['itemsIdx']]
        
        itemsVectorSize=length(db[['items']][['bundle']])
        if(row>itemsVectorSize){
          colnms=names(db[['items']])
          for(colNm in colnms){
            colClass=class(db[['items']][[colNm]])
            if(colClass=='character'){
              db[['items']][[colNm]]=c(db[['items']][[colNm]],character(vector.increment))
            }else if(colClass=='integer'){
              db[['items']][[colNm]]=c(db[['items']][[colNm]],integer(vector.increment))
            }else if(colClass=='numeric'){
              db[['items']][[colNm]]=c(db[['items']][[colNm]],numeric(vector.increment))
            }else{
              stop('Unsupported column class ',colClass,' of column ',colNm,'\n')
            }
          }
          if(verboseLevel>10){
            cat("Incremented items\n")
          }
        }
        seqIdx=seqIdx+as.integer(1)
        
        db[['items']][['session']][row]=sessionName
        db[['items']][['bundle']][row]=bName
        itemId=it[['id']]
        if(is.null(itemId)){
          # for instance aetobi has no .hlb files and therefore no links and item ids
          id=paste(db[['name']],sessionName,bName,sep='_')
          itemId=NA
        }else{
          id=paste(db[['name']],sessionName,bName,it['id'],sep='_')
        }
        db[['items']][['id']][row]=id
        db[['items']][['itemID']][row]=itemId
        db[['items']][['level']][row]=lvl[['name']]
        db[['items']][['type']][row]=lvl[['type']]
        if(!is.null(bundle[['sampleRate']])){
          db[['items']][['sampleRate']][row]=bundle[['sampleRate']]
        }else{
          db[['items']][['sampleRate']][row]=NA
        }
        db[['items']][['seqIdx']][row]=seqIdx
        sp=it[['samplePoint']]
        if(!is.null(sp)){
          db[['items']][['samplePoint']][row]=sp
        }else{
          db[['items']][['samplePoint']][row]=NA
        }
        ss=it[['sampleStart']]
        if(!is.null(ss)){
          db[['items']][['sampleStart']][row]=ss
        }else{
          db[['items']][['sampleStart']][row]=NA
        }
        sd=it[['sampleDur']]
        if(!is.null(sd)){
          db[['items']][['sampleDur']][row]=sd
        }else{
          db[['items']][['sampleDur']][row]=NA
        }
        
        lbls=it[['labels']]
        lblsLen=length(lbls)
        lbl0=it[['labels']][[1]][['value']]
        if(is.null(lbl0)){
          db[['items']][['label']][row]=''
        }else{
          db[['items']][['label']][row]=lbl0
        }
        for(i in 1:maxLbls){
          rLbl=''
          if(lblsLen>=i){
            lbl=lbls[[i]]
            if(!is.null(lbl)){
              db[['labelsIdx']]=db[['labelsIdx']]+1L
              lrow=db[['labelsIdx']]
              labelsVectorSize=length(db[['labels']][['bundle']])
              if(lrow>labelsVectorSize){
                colnms=names(db[['labels']])
                for(colNm in colnms){
                  colClass=class(db[['labels']][[colNm]])
                  if(colClass=='character'){
                    db[['labels']][[colNm]]=c(db[['labels']][[colNm]],character(vector.increment))
                  }else if(colClass=='integer'){
                    db[['labels']][[colNm]]=c(db[['labels']][[colNm]],integer(vector.increment))
                  }else{
                    stop('Unsupported column class ',colClass,' of column ',colNm,'\n')
                  }
                }
                if(verboseLevel>10){
                  cat("Incremented labels\n")
                }
              }
              rLbl=lbl[['value']]
              if(is.null(rLbl)){
                rLbl=''
              }
              db[['labels']][['itemID']][lrow]=id
              db[['labels']][['session']][lrow]=sessionName
              db[['labels']][['bundle']][lrow]=bName
              db[['labels']][['labelIdx']][lrow]=i-1L
              db[['labels']][['name']][lrow]=lbl[['name']]
              db[['labels']][['label']][lrow]=rLbl
              
            }
          }
        } 
        
      }
    }
    
    for(lk in bundle[['links']]){
      db[['linksIdx']]=db[['linksIdx']]+1L
      row=db[['linksIdx']]
      linksVectorSize=length(db[['links']][['bundle']])
      if(row>linksVectorSize){
        colnms=names(db[['links']])
        for(colNm in colnms){
          colClass=class(db[['links']][[colNm]])
          if(colClass=='character'){
            db[['links']][[colNm]]=c(db[['links']][[colNm]],character(vector.increment))
          }else if(colClass=='integer'){
            db[['links']][[colNm]]=c(db[['links']][[colNm]],integer(vector.increment))
          }else{
            stop('Unsupported column class ',colClass,' of column ',colNm,'\n')
          }
        }
        if(verboseLevel>10){
          cat("Incremented links\n")
        }
      }
      db[['links']][['session']][row]=sessionName
      db[['links']][['bundle']][row]=bName
      db[['links']][['fromID']][row]=lk[['fromID']]
      db[['links']][['toID']][row]=lk[['toID']]
      lbl=lk[['label']]
      if(is.null(lbl)){
        db[['links']][['label']][row]=NA
      }else{
        db[['links']][['label']][row]=lbl
      }
    }
    
    bundle[['levels']]=NULL
    bundle[['links']]=NULL
    
    bName=bundle[['name']]
    
    #utts[[uttCode]]=bundle
    db[['sessions']][[sessionName]][['bundles']][[bName]]=bundle
    
    if(verboseLevel>5){
      cat("Loaded bundle ",bName,"(",ui," of ",bundlesCount,")\n")
    }
    progress=progress+1L
    if(showProgress){
      setTxtProgressBar(pb,progress)
    }
  }
  itemsIdx=db[['itemsIdx']]
  
  db[['items']]=data.frame(id=db[['items']][['id']][0:itemsIdx],session=db[['items']][['session']][0:itemsIdx],bundle=db[['items']][['bundle']][0:itemsIdx],level=db[['items']][['level']][0:itemsIdx],itemID=db[['items']][['itemID']][1:itemsIdx],type=db[['items']][['type']][1:itemsIdx],seqIdx=db[['items']][['seqIdx']][1:itemsIdx],sampleRate=db[['items']][['sampleRate']][1:itemsIdx],samplePoint=db[['items']][['samplePoint']][1:itemsIdx],sampleStart=db[['items']][['sampleStart']][1:itemsIdx],sampleDur=db[['items']][['sampleDur']][1:itemsIdx],label=db[['items']][['label']][1:itemsIdx],stringsAsFactors=FALSE)
  #tmpDf=data.frame(db[['items']],stringsAsFactors = FALSE)
  #db[['items']]=tmpDf[1:itemsIdx,]
  progress=progress+1L
  if(showProgress){
    setTxtProgressBar(pb,progress)
  }
  
  labelsIdx=db[['labelsIdx']]
  db[['labels']]=data.frame(itemID=db[['labels']][['itemID']][0:labelsIdx],session=db[['labels']][['session']][0:labelsIdx],bundle=db[['labels']][['bundle']][0:labelsIdx],labelIdx=db[['labels']][['labelIdx']][0:labelsIdx],name=db[['labels']][['name']][1:labelsIdx],label=db[['labels']][['label']][1:labelsIdx],stringsAsFactors=FALSE)
  #tmpDf=data.frame(db[['labels']],stringsAsFactors = FALSE)
  #db[['labels']]=tmpDf[1:labelsIdx,]
  #db[['links']]=data.frame(bundle=bundle_l[1:lli],fromID=fromID_l[1:lli],toID=toID_l[1:lli],label=label_l[1:lli])
  progress=progress+1L
  if(showProgress){
    setTxtProgressBar(pb,progress)
  }
  
  linksIdx=db[['linksIdx']]
  db[['links']]=data.frame(session=db[['links']][['session']][0:linksIdx],bundle=db[['links']][['bundle']][0:linksIdx],fromID=db[['links']][['fromID']][0:linksIdx],toID=db[['links']][['toID']][0:linksIdx],label=db[['links']][['label']][0:linksIdx],stringsAsFactors=FALSE)
  progress=progress+1L
  if(showProgress){
    setTxtProgressBar(pb,progress)
  }
  if(verboseLevel>3){
    cat('Loaded',itemsIdx,'items',labelsIdx,'labels',linksIdx,'links\n')
  }
  #db[['links']]=db[['links']][1:linksIdx,]
  #tmpDf=data.frame(db[['links']],stringsAsFactors = FALSE)
  #db[['links']]=tmpDf[1:linksIdx,]
  #tmpDf=NULL
  
  ## Emu does not divide utterances in sessions
  ## we create a dummy container session to satisfy new db data model
  ## (db: list of sessions, session: list of bundles (utterances))
  #containerSession=emuDB.session(name='0000',bundles=utts)
  ##db$sessions[[1]]$bundles = utts
  #db[['sessions']][['0000']]=containerSession
  
  if(verboseLevel>3){
    cat('Removing redundant links...\n')
  }
  db=remove.database.redundant.links(db)
  if(verboseLevel>3){
    cat('Removed redundant links.\n')
  }
  progress=progress+1L
  if(showProgress){
    setTxtProgressBar(pb,progress)
  }
  if(verboseLevel>3){
    cat('Build redundant links...\n')
  }
  redLinks=build.redundant.links.all(db)
  if(verboseLevel>3){
    cat('Build redundant links.\n')
  }
  progress=progress+1L
  if(showProgress){
    setTxtProgressBar(pb,progress)
  }
  if(verboseLevel>3){
    cat('Extending links table...\n')
  }
  db[['linksExt']]=calculate.postions.of.links(db[['items']],redLinks)
  #db[['linksExt']]=calculate.postions.of.links(db[['items']],db[['links']])
  if(verboseLevel>3){
    cat('Links table extended.\n')
  }
  progress=progress+1L
  if(showProgress){
    setTxtProgressBar(pb,progress)
    cat("\n")
  }
  
  if(verboseLevel>0){
    cat("Loaded database bundles.")
  }
  
  return(db)
}


##' @title Convert legacy EMU database and store it in new format
##' @description If the legacy database template file could be found the database metadata and annoations are loaded. If load is successfull a new directory with the name of the database is created in the \code{targetDirectory}.
##' @details Information of the legacy Emu template file is transferred to [dbname]_DBconfig.json file. Legacy Emu utterances are reorganized in sessions and bundles. 
##' Media files (e.g. wav files) are copied, SSFF track files are rewritten. Annotations in Emu hierarchy (.hlb) files and ESPS label files are converted to a [bundleName]_annot.json file per bundle (utterance).
##' Please note that only those files get copied, which are referenced by the template file. Additional files in the legacy database directories are ignored. The legacy Emu database is not modified.
##' 
##' options is a list of key value pairs:
##' rewriteSSFFTracks if TRUE rewrite SSF tracks instead of file copy to get rid of big endian encoded SSFF files (SPARC), default: TRUE
##' ignoreMissingSSFFTrackFiles if TRUE missing SSFF track files are ignored, default: TRUE
##' sourceFileTextEncoding encoding of legacy database text files (template, label and hlb files) :default NULL (usess encoding of operating system platform)
##' 
##' @param emuTplPath EMU template file path
##' @param targetDir target directory
##' @param options list of options
##' @param verbose be verbose
##' @author Klaus Jaensch
##' @seealso \code{\link{load.emuDB}}
##' @export
##' @keywords emuDB database schema Emu
##' @examples
##' \dontrun{
##' ## Convert legacy EMU database specified by EMU 
##' ## template file /homes/mylogin/ae/ae.tpl to directory /homes/mylogin/EMUnew/ae
##'
##' convert.legacyEmuDB.to.emuDB("/homes/mylogin/ae/ae.tpl","/homes/mylogin/EMUnew/ae")
##'
##' }
##' 
convert.legacyEmuDB.to.emuDB <- function(emuTplPath,targetDir,options=NULL,verbose=TRUE){
 
  # default options
  # ignore missing SSFF track files
  # rewrite SSFF track files
  # encoding : platform 
  mergedOptions=list(sourceFileTextEncoding=NULL,ignoreMissingSSFFTrackFiles=TRUE,rewriteSSFFTracks=TRUE)
  if(!is.null(options)){
    for(opt in names(options)){
        mergedOptions[[opt]]=options[[opt]]
    }
  }
  
  # pre check target dir
  if(file.exists(targetDir)){
    tdInfo=file.info(targetDir)
    if(!tdInfo[['isdir']]){
      stop(targetDir," exists and is not a directory.")
    }
  }
  
  # load database schema and metadata to get db name
  dbConfig=load.database.schema.from.emu.template(emuTplPath,encoding=mergedOptions[['sourceFileTextEncoding']])
  # database dir
  pp=file.path(targetDir,dbConfig[['name']])
  
  # check existence of database dir
  if(file.exists(pp)){
    stop("Database storage dir ",pp," already exists.")
  }

  # load legacy Emu db
  db=load.database.from.legacy.emu(emuTplPath,showProgress=verbose)
  
  # store loaded database 
  store.emuDB(db,targetDir,options=mergedOptions,showProgress=verbose)
  
}

##' Convert legacy EMU database and store it in new format
##' If the legacy database could be found it is loaded. If load is successfull a new directory with the name of the database is created in the \code{targetDirectory}
##' Loading by name only works if database was used with legacy EMU. Use the function \code{\link{convert.legacyEmuDB.to.emuDB}} otherwise.
##'
##' options is a list of key value pairs:
##' rewriteSSFFTracks if TRUE rewrite SSF tracks instead of file copy to get rid of big endian encoded SSFF files (SPARC), default: TRUE
##' ignoreMissingSSFFTrackFiles if TRUE missing SSFF track files are ignored, default: TRUE
##' ##' sourceFileTextEncoding encoding of legacy database text files (template, label and hlb files) :default NULL (usess encoding of operating system platform)
##' 
##' @param dbName legacy EMU database name
##' @param targetDir target directory
##' @param options list of options
##' @param verbose be verbose
##' @author Klaus Jaensch
##' @seealso \code{\link{convert.legacyEmuDB.to.emuDB}} \code{\link{load.emuDB}} 
##' @export
##' @keywords emuDB database schema Emu
##' @examples
##' \dontrun{
##' ## Load database "ae", convert and save in new format to directory /homes/mylogin/EMUnew/
##' 
##' convert.legacyEmuDB.by.name.to.emuDB("ae","/homes/mylogin/EMUnew/")
##' 
##' }
##' 
convert.legacyEmuDB.by.name.to.emuDB <- function(dbName,targetDir,options=NULL,verbose=TRUE){
  
  # default options
  # ignore missing SSFF track files
  # rewrite SSFF track files
  # encoding : platform 
  mergedOptions=list(sourceFileTextEncoding=NULL,ignoreMissingSSFFTrackFiles=TRUE,rewriteSSFFTracks=TRUE)
  if(!is.null(options)){
    for(opt in names(options)){
      mergedOptions[[opt]]=options[[opt]]
    }
  }
  
  # pre check target dir
  if(file.exists(targetDir)){
    tdInfo=file.info(targetDir)
    if(!tdInfo[['isdir']]){
      stop(targetDir," exists and is not a directory.")
    }
  }
  
  # database dir
  pp=file.path(targetDir,dbName)
  
  # check existence of database dir
  if(file.exists(pp)){
    stop("Database storage dir ",pp," already exists.")
  }
  
  # load database schema and metadata
  db=load.database.from.legacy.emu.by.name(dbName,showProgress=verbose,encoding=mergedOptions[['sourceFileTextEncoding']])
  
  # save in new format
  store.emuDB(db,targetDir,options=mergedOptions,showProgress=verbose)
  #activeButtons=list(saveBundle=TRUE)
 
}
