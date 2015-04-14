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
##' @description Reads EMU_TEMPLATE_PATH variable from environment or from file ${HOME}/.emu/emu-conf or ${USERPROFILE}/.emu/Emu/emu-conf
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
  # set sample rate even if no annotation levels exist
  # Bug fix #20
  if(is.null(bundleSampleRate)){
    bundleSampleRate=sampleRate
  }
  
  #annotates=paste0('0000_ses/',uttCode,bundle.dir.suffix,'/',sampleTrackFile)
  
  sampleTrackFile=paste0(bundleName,'.',schema[['mediafileExtension']]) 
  #annotates=paste0(sessionName,session.suffix,'/',newBundleId[2],bundle.dir.suffix,'/',sampleTrackFile)
  # bug #19
  annotates=paste0(sampleTrackFile)
  bundle=create.bundle(name=bundleName,sessionName=sessionName,legacyBundleID=legacyBundleID,annotates=annotates,sampleRate=bundleSampleRate,levels=levels,signalpaths=signalpaths,mediaFilePath=sampleRateReferenceFile,links=links)
  return(bundle)
}

# remove.redundant.bundle.links<-function(bundle){
#   # Legacy EMU and query functions link collections contain links for each possible connection between levels
#   # We consider links that do not follow link definition constraints as redundant and therefore we remove them from the
#   # link data model
#   #
#   # build SQL query from link definitions
#   
#   move
#   items=database[['items']]
#   sqlQuery="SELECT l.* FROM items f,items t,links l WHERE f.bundle=t.bundle AND l.bundle=f.bundle AND f.session=t.session AND l.session=f.session AND f.itemID=l.fromID AND t.itemID=l.toID AND ("
#   ldCnt=length(database[['DBconfig']][['linkDefinitions']])
#   for(i in 1:ldCnt){
#     ld=database[['DBconfig']][['linkDefinitions']][[i]]
#     sqlQuery=paste0(sqlQuery,'(f.level=\'',ld[['superlevelName']],'\' AND t.level=\'',ld[['sublevelName']],'\')')
#     if(i<ldCnt){
#       sqlQuery=paste0(sqlQuery,' OR ')
#     }
#   }
#   sqlQuery=paste0(sqlQuery,')')
#   #cat(sqlQuery,"\n")
#   return(sqldf(sqlQuery))
# }

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
  
  # Could not find a way to directly delte the redundant links. SQLite DELETE commands functionality is limited.
  # use a temp table for now
  res<-dbSendQuery(emuDBs.con,'DELETE FROM linksTmp')
  dbClearResult(res)
    sqlQuery="INSERT INTO linksTmp SELECT l.* FROM items f,items t, links l WHERE f.bundle=t.bundle AND l.bundle=f.bundle AND f.session=t.session AND l.session=f.session AND f.itemID=l.fromID AND t.itemID=l.toID AND ("
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
      #database[['links']]=sqldf(sqlQuery)
      res<-dbSendQuery(emuDBs.con,sqlQuery)
      dbClearResult(res)
      res<-dbSendQuery(emuDBs.con,'DELETE FROM links')
      dbClearResult(res)
      res<-dbSendQuery(emuDBs.con,'INSERT INTO links SELECT * FROM linksTmp')
      dbClearResult(res)
      res<-dbSendQuery(emuDBs.con,'DELETE FROM linksTmp')
      dbClearResult(res)
    } 
  
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

build.hashed.link.defs<-function(linkDefinitions){
  
  # build link definitions hashed by super level name
  linkDefsHashed=list()
  for(ld in linkDefinitions){
    supLvlNm=ld[['superlevelName']]
    #if(is.null(linkDefsHashed[[supLvlNm]])){
    #  # set
    #  linkDefsHashed[[supLvlNm]]=ld[['sublevelName']]
    #}else{
      # append
      linkDefsHashed[[supLvlNm]]=c(linkDefsHashed[[supLvlNm]],ld[['sublevelName']])
    #}
    
  }
  return(linkDefsHashed)
}

remove.redundant.bundle.links<-function(linkDefsHashed,bundle){
  lvls=bundle[['levels']]
  itemsHashed=list()
  for(lvl in lvls){
    for(it in lvl[['items']]){
      itemsHashed[[it[['id']]+1]]=lvl[['name']]
    }
  }
  legacyLinks=bundle[['links']]
  
  # new link list without redundant links
  links=list()
  
  for(legLk in legacyLinks){
    fromLvl=itemsHashed[[legLk[['fromID']]+1]]
    toLvl=itemsHashed[[legLk[['toID']]+1]]
    
    subLvls=linkDefsHashed[[fromLvl]]
    for(subLvl in subLvls){
      if(subLvl==toLvl){
        links[[length(links)+1]]=legLk
      }
    }
  }
  # overwrite
  bundle[['links']]=links
  return(bundle)
  
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
  
  initialize.DBI.database()
  .store.emuDB.DBI(db)
  
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
  linkDefsHashed=build.hashed.link.defs(dbConfig[['linkDefinitions']])
  for(ui in us){
    legacyBundleID=legacyBundleIDsList[[ui]]
    newBundleId=convert.legacy.bundle.id(legacyBundleID)
    sessionName=newBundleId[1]
    bundleName=newBundleId[2]
    if(is.null(db[['sessions']][[sessionName]])){
      # create session if needed
      db[['sessions']][[sessionName]]=list(name=sessionName,bundles=list())
      .store.session.DBI(dbd[['UUID']],sessionName)
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
   
    schema=db[['DBconfig']]
    maxLbls=db[['DBconfig']][['maxNumberOfLabels']]
    bundle[['db_UUID']]=schema[['UUID']]
    .store.bundle.DBI(db,bundle)
    .store.bundle.annot.DBI(schema[['UUID']],bundle)
    
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
  
  if(verboseLevel>3){
    cat('Removing redundant links...\n')
  }
  remove.database.redundant.links(db)
  if(verboseLevel>3){
    cat('Removed redundant links.\n')
  }
  progress=progress+1L
  if(showProgress){
    setTxtProgressBar(pb,progress)
  }
  
  db[['items']]=dbReadTable(emuDBs.con,'items')
 
  progress=progress+1L
  if(showProgress){
    setTxtProgressBar(pb,progress)
  }
  
  db[['labels']]=dbReadTable(emuDBs.con,'labels')
  progress=progress+1L
  if(showProgress){
    setTxtProgressBar(pb,progress)
  }
  
  db[['links']]=dbReadTable(emuDBs.con,'links')
  progress=progress+1L
  if(showProgress){
    setTxtProgressBar(pb,progress)
  }
  if(verboseLevel>3){
    cat('Loaded',itemsIdx,'items',labelsIdx,'labels',linksIdx,'links\n')
  }
  
  ## Emu does not divide utterances in sessions
  ## we create a dummy container session to satisfy new db data model
  ## (db: list of sessions, session: list of bundles (utterances))
  #containerSession=emuDB.session(name='0000',bundles=utts)
  ##db$sessions[[1]]$bundles = utts
  #db[['sessions']][['0000']]=containerSession
  
 
  if(verboseLevel>3){
    cat('Build redundant links...\n')
  }
  build.redundant.links.all(db)
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
  calculate.postions.of.links()
  db[['linksExt']]=dbReadTable(emuDBs.con,'linksExt')
  if(verboseLevel>3){
    cat('Links table extended.\n')
  }
  progress=progress+1L
  if(showProgress){
    setTxtProgressBar(pb,progress)
    cat("\n")
  }
  
  #destroy.DBI.database()
  if(verboseLevel>0){
    cat("Loaded database bundles.")
  }
  
  
  return()
}


##' @title Convert legacy EMU database and store it in new format
##' @description Converts an existing legacy EMU database to the new EMU format. Conversion is done in two steps. In the first step the legacy database configuration and the annotations are loaded.
##' In the second step the target directory for the new database structure is created, the configuration and annoation files are stored in the new format and the signal files are copied.
##' @details The database will be stored if the legacy database template file could be found. The database metadata and annoations are loaded. If loading is successfull, a new directory with the name of the database is created in the \code{targetDirectory}. \code{targetDirectory} and its parent directories will be created if the do not exist. If the new database directory already exists the function stops wit an error.
##' 
##' Information of the legacy Emu template file is transferred to [dbname]_DBconfig.json file. Some of the flags of the legacy EMU template files are ignored (lines of syntax: "set [flagName] [flagValue]", known ignored flag names are: 'LabelTracks','SpectrogramWhiteLevel','HierarchyViewLevels','SignalViewLevels'). 
##' Legacy Emu utterances are reorganized in sessions and bundles. Session structure depends on wilcard path pattern of primary track. At least one default session with name '0000' will be created.
##' Media files (e.g. wav files) are copied, SSFF track files are rewritten (read/write with ASSP (wrassp) library) by default. Annotations in Emu hierarchy (.hlb) files and ESPS label files are converted to a [bundleName]_annot.json file per bundle (utterance).
##' Please note that only those files get copied, which are referenced by the template file. Additional files in the legacy database directories are ignored. The legacy Emu database is not modified.
##' 
##' options is a list of key value pairs:
##' 
##' \code{rewriteSSFFTracks} if \code{TRUE} rewrite SSF tracks instead of file copy to get rid of big endian encoded SSFF files (SPARC), default: \code{TRUE}
##' 
##' \code{ignoreMissingSSFFTrackFiles} if \code{TRUE} missing SSFF track files are ignored, default: \code{TRUE}
##' 
##' \code{sourceFileTextEncoding} encoding of legacy database text files (template, label and hlb files), possible values: NULL, "latin1", "UTF-8" "bytes" or "unknown" :default \code{NULL} (uses encoding of operating system platform)
##' 
##' \code{symbolicLinkSignalFiles} if \code{TRUE} signal files are symbolic linked instead of copied. Implies: \code{rewriteSSFFTracks=FALSE}, Default: \code{FALSE}
##' 
##' @param emuTplPath EMU template file path
##' @param targetDir target directory
##' @param dbUUID optional UUID of emuDB
##' @param options list of options (see details)
##' @param verbose be verbose
##' @author Klaus Jaensch
##' @seealso \code{\link{load.emuDB}}
##' @export
##' @keywords emuDB database schema Emu
##' @examples
##' \dontrun{
##' ## Convert legacy EMU database specified by EMU 
##' ## template file /homes/mylogin/ae/ae.tpl to directory /homes/mylogin/EMUnew/
##' ## and load it afterwards
##'
##' convert.legacyEmuDB.to.emuDB("/homes/mylogin/ae/ae.tpl","/homes/mylogin/EMUnew/")
##' ae=load.emuDB("/homes/mylogin/EMUnew/ae")
##'
##' ## Convert database "ae" and do not rewrite SSFF tracks 
##' 
##' convert.legacyEmuDB.by.name.to.emuDB(
##' "ae",
##' "/homes/mylogin/EMUnew/",
##' options=list(rewriteSSFFTracks=FALSE)
##' )
##' 
##' }
##' 
convert.legacyEmuDB.to.emuDB <- function(emuTplPath,targetDir,dbUUID=NULL,options=NULL,verbose=TRUE){
  progress=0
  # default options
  # ignore missing SSFF track files
  # rewrite SSFF track files
  # encoding : platform 
  mergedOptions=list(sourceFileTextEncoding=NULL,ignoreMissingSSFFTrackFiles=TRUE,rewriteSSFFTracks=TRUE,symbolicLinkSignalFiles=FALSE)
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
  legacyBasePath=dirname(emuTplPath)
  # load database schema and metadata to get db name
  dbConfig=load.database.schema.from.emu.template(emuTplPath,dbUUID=dbUUID,encoding=mergedOptions[['sourceFileTextEncoding']])
  # database dir
  pp=file.path(targetDir,dbConfig[['name']])
  
  # check existence of database dir
  if(file.exists(pp)){
    stop("Database storage dir ",pp," already exists.")
  }


  progress=progress+1L
  
  tplBaseDir=NULL
  tplBaseDir=dirname(emuTplPath)
  
  # create target dir
  dir.create(targetDir)
  
  # create database dir in targetdir
  dir.create(pp)
 
  # get UUID
  dbUUID=dbConfig[['UUID']]
  
  # set user editable
  dbConfig[['EMUwebAppConfig']][['activeButtons']]=list(saveBundle=TRUE)
  
  # store db schema file
  .store.DBconfig(pp,dbConfig)
  progress=progress+1L
  
  # load primary track file list first
  # and find samples track to get sample rate
  primaryFileList = NULL
  
  primaryBasePath=NULL
  primaryFileExtension=NULL
  
  #pattern='*'
  primaryFileSuffixPattern=NULL
  # find primary and sample track paths
  for(tr in dbConfig[['tracks']]){
    #cat("Track: ",tr$name," ",tr$fileExtension,"\n")
    if(tr[['fileExtension']]==dbConfig[['flags']][['PrimaryExtension']]){
      
      primaryFileExtension=tr[['fileExtension']]
      primaryBasePath=tr[['basePath']]
      #if(!is.null(tr[['unitSuffix']])){
      #  pattern=str_c(pattern,tr[['unitSuffix']])
      #}
    }
  }
  
  if(is.null(primaryFileExtension)){
    for(ad in dbConfig[['annotationDescriptors']]){
      if(ad[['extension']]==dbConfig[['flags']][['PrimaryExtension']]){
        primaryFileExtension=ad[['extension']]
        primaryBasePath=ad[['basePath']]
        break
      }
    }
    
  }
  #pattern=str_c(pattern,'[.]',primaryFileExtension)
  primaryFileSuffixPattern=paste0('[.]',primaryFileExtension,'$')
  #primaryFileList=list.file.matching.emu.path.pattern(db[['basePath']],primaryBasePath,filePattern=pattern)
  legacyBundleIDsList=get.legacy.emu.bundles(legacyBasePath,primaryBasePath,primaryFileSuffixPattern)
  
  
  bundlesCount=length(legacyBundleIDsList)
  #utts=vector(mode='list',length=bundlesCount)
  us=1:bundlesCount
  if(verbose){
    cat("INFO: Loading legacy EMU database containing",bundlesCount,"bundles...\n")
    pb=txtProgressBar(min=0,max=bundlesCount+7,initial=progress,style=3)
    
    setTxtProgressBar(pb,progress)
  }
  linkDefsHashed=build.hashed.link.defs(dbConfig[['linkDefinitions']])
  for(ui in us){
    legacyBundleID=legacyBundleIDsList[[ui]]
    newBundleId=convert.legacy.bundle.id(legacyBundleID)
    sessionName=newBundleId[1]
    bundleName=newBundleId[2]
    sDir=paste0(sessionName,session.suffix)
    sfp=file.path(pp,sDir)
    #if(is.null(db[['sessions']][[sessionName]])){
    if(!file.exists(sfp)){
      # create session if needed
      #db[['sessions']][[sessionName]]=list(name=sessionName,bundles=list())
      #.store.session.DBI(dbd[['UUID']],sessionName)
       
        #cat(targetDir,s$name,sfp,"\n")
        dir.create(sfp)
    }
    ptrFilePath=get.legacy.file.path(legacyBasePath,primaryBasePath,legacyBundleID,primaryFileExtension)
    #ptrFilePath=primaryFileList[ui]
    #cat("Primary track file path: ",ptrFilePath,"\n")
    
    ptrFileBasename=basename(ptrFilePath)
    #cat("Ext: ",primaryTrackFileExtension,"\n")
    cutLen=str_length(primaryFileExtension)+1L
    cutPos=str_length(ptrFileBasename)-cutLen
    #cat("Cut: ",ptrFileBasename,cutLen,cutPos,"\n")
    #uttCode=substr(ptrFileBasename,1,cutPos)
    bundle=load.annotation.for.legacy.bundle(dbConfig,legacyBundleID,legacyBasePath,encoding=mergedOptions[['sourceFileTextEncoding']])
    bundle=remove.redundant.bundle.links(linkDefsHashed,bundle)
    #maxLbls=db[['DBconfig']][['maxNumberOfLabels']]
    #bundle[['db_UUID']]=dbConfig[['UUID']]
    #.store.bundle.DBI(db,bundle)
    #.store.bundle.annot.DBI(db,bundle)
  
  bDir=paste0(bundle[['name']],bundle.dir.suffix)
  bfp=file.path(sfp,bDir)
  dir.create(bfp)
  pFilter=emuR.persist.filters.bundle
  bp=marshal.for.persistence(bundle,pFilter)
  
  # metadata (annotations)
  ban=str_c(bundle[['name']],bundle.annotation.suffix,'.json')
  baJSONPath=file.path(bfp,ban)
  pbpJSON=jsonlite::toJSON(bp,auto_unbox=TRUE,force=TRUE,pretty=TRUE)
  writeLines(pbpJSON,baJSONPath)
  
  
  for(sf in bundle[['signalpaths']]){
    #cat("Signalpath: ",sf,"\n")
    bn=basename(sf)
    nsfp=file.path(bfp,bn)
    # check if SSFF type
    isSSFFFile=FALSE
    for(ssffTrDef in dbConfig[['ssffTrackDefinitions']]){
      ssffTrFileExt=ssffTrDef[['fileExtension']]
      fileExtPatt=paste0('[.]',ssffTrFileExt,'$')
      if(length(grep(fileExtPatt,sf))==1){
        isSSFFFile=TRUE
        break
      }
    }
    if(file.exists(sf)){
      if(mergedOptions[['symbolicLinkSignalFiles']]){
        file.symlink(from=sf,to=nsfp)
      }else if(mergedOptions[['rewriteSSFFTracks']] && isSSFFFile){
        # is SSFF track
        # read/write instead of copy to get rid of big endian encoded SSFF files (SPARC)
        pfAssp=read.AsspDataObj(sf)
        write.AsspDataObj(pfAssp,nsfp)
        #cat("Rewritten SSFF: ",sf," to ",nsfp,"\n")
      }else{
        # media file (likely a wav file)
        file.copy(from=sf,to=nsfp)
        #cat("Copied: ",sf," to ",nsfp,"\n")
      }
    }else{
      if(!mergedOptions[['ignoreMissingSSFFTrackFiles']]){
        stop("SSFF track file :'",sf,"' does not exist!")
      }
    }
  }
    bundle[['levels']]=NULL
    bundle[['links']]=NULL
    
    bName=bundle[['name']]
    
    #utts[[uttCode]]=bundle
    #db[['sessions']][[sessionName]][['bundles']][[bName]]=bundle
    
  }
    progress=progress+1L
    if(verbose){
      setTxtProgressBar(pb,progress)
    }

  
  
}

##' Convert legacy EMU database and store it in new format
##' @description Converts an existing legacy EMU database to the new EMU format. Conversion is done in two steps. In the first step the legacy database configuration and the annotations are loaded.
##' In the second step the target directory for the new database structure is created, the configuration and annoation files are stored in the new format and the signal files are copied.
##' @details This function first tries to get the path of the legacy template file from the given database name using \code{\link{list.legacy.emu.databases}}.
##' If the database could be found the function \code{\link{convert.legacyEmuDB.to.emuDB}} is called.
##' 
##' @param dbName legacy EMU database name
##' @param targetDir target directory
##' @param options list of options (see details of \code{\link{convert.legacyEmuDB.to.emuDB}})
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
  emuDbsList=list.legacy.emu.databases()
  emuTplPath=emuDbsList[[dbName]]
  if(is.null(emuTplPath)){
    stop("Legacy EMU database '",dbName,"' could not be found.")
  }
  convert.legacyEmuDB.to.emuDB(emuTplPath = emuTplPath,targetDir = targetDir,options = options,verbose = verbose)
  
 
}
