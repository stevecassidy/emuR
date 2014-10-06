require(stringr)
require(uuid)
require(wrassp)
#require(data.table)


# constants

session.suffix='_ses'
bundle.dir.suffix='_bndl'
bundle.annotation.suffix='_annot'
database.schema.suffix='_DBconfig.json'

vector.increment=100000

create.schema.track <- function(basePath=NULL,name,columnName=name,unitSuffix=NULL,extension=NULL,hasDeepStructure=FALSE){
  o <- list(basePath=basePath,name=name,columnName=columnName,unitSuffix=unitSuffix,fileExtension=extension)
  class(o) <- 'emuDB.schema.track'
  invisible(o)
}

create.schema.annotationDescriptor <- function(name=NULL,basePath=NULL,extension=NULL,type=NULL,timeFactor=NULL,levelDefinitions=NULL){
  o <- list(name=name,basePath=basePath,extension=extension,type=type,timeFactor=timeFactor,levelDefinitions=levelDefinitions)
  class(o) <- 'emuDB.schema.annotationDescriptor'
  invisible(o)
}

## Create emuDB attribute definition object
## 
## @param name name of the level
## @param type level type (ITEM,EVENT,SEGMENT)
## @return object of class emuDB.schema.attributeDefinition
## @author Klaus Jaensch
## @keywords emuDB attribute level Emu
## 
create.schema.attributeDefinition <- function(name,type='string'){
  o <- list(name=name,type=type)
  class(o) <- c('create.schema.attributeDefinition','list')
  invisible(o)
}

## Create emuDB level definition object
## 
## @param name name of the level
## @param type level type (ITEM,EVENT,SEGMENT)
## @param attributeDefinitions list of attribute definitions
## @return object of class emuDB.levelDefinition
## @author Klaus Jaensch
## @keywords emuDB level Emu
## 
create.schema.levelDefinition <- function(name,type=NULL,attributeDefinitions=NULL){
  if(is.null(attributeDefinitions)){
    defAttrDef=create.schema.attributeDefinition(name=name)
    attributeDefinitions=list(defAttrDef)
  }
  o <- list(name=name,type=type,attributeDefinitions=attributeDefinitions)
  class(o) <- 'emuDB.schema.levelDefinition'
  invisible(o)
}

## Create emuDB link definition object
## Represents the hierarchical information of EMU hlb files
## 
## @param name name of the link (optional)
## @param type link type ("ONE_TO_ONE", "ONE_TO_MANY", "MANY_TO_MANY")
## @param superlevelName name of the super level (link from)
## @param sublevelName name of the sublevel (link to)
## @return object of class emuDB.schema.linkDefinition
## @author Klaus Jaensch
## @keywords emuDB database schema link Emu
## 
create.schema.linkDefinition <- function(name=NULL,type,superlevelName,sublevelName){
  o <- list(name=name,type=type,superlevelName=superlevelName,sublevelName=sublevelName)
  class(o) <- 'emuDB.schema.linkDefinition'
  invisible(o)
}

create.schema.databaseDefinition <- function(name,UUID,mediafileBasePathPattern,mediafileExtension,ssffTrackDefinitions,levelDefinitions,linkDefinitions,EMUwebAppConfig,annotationDescriptors,tracks,flags=NULL){
  o <- list(name=name,UUID=UUID,mediafileBasePathPattern=mediafileBasePathPattern,mediafileExtension=mediafileExtension,ssffTrackDefinitions=ssffTrackDefinitions,levelDefinitions=levelDefinitions,linkDefinitions=linkDefinitions,EMUwebAppConfig=EMUwebAppConfig,annotationDescriptors=annotationDescriptors,tracks=tracks,flags=flags)
  class(o) <- c('list','emuDB.schema.databaseDefinition')
  #rTypes=list(levelDefinitions=c('list','emuDB.schema.levelDefinition',linkDefinitions=c('list','emuDB.schema.linkDefinition')
  #attr(o,'ips.persist')<-list(rTypes=rTypes)
  invisible(o)
}




#emuDB.schema.EMUwebAppConfig.signalCanvases.assign <- function(spec,osci){
#  o <- list(SPEC=spec,OSCI=osci)
#  class(o) <- 'emuDB.EMUwebAppConfig.assign'
#  invisible(o)
#}
create.EMUwebAppConfig.signalCanvas <- function(order,assign,contourLims){
  o <- list(order=order,assign=assign,contourLims=contourLims)
  class(o) <- 'emuDB.EMUwebAppConfig.signalCanvas'
  invisible(o)
}
create.EMUwebAppConfig.levelCanvas <- function(order){
  o <- list(order=order)
  class(o) <- 'emuDB.EMUwebAppConfig.levelCanvas'
  invisible(o)
}
create.EMUwebAppConfig.perspective <- function(name,signalCanvases,levelCanvases,twoDimCanvases){
  o <- list(name=name,signalCanvases=signalCanvases,levelCanvases=levelCanvases,twoDimCanvases=twoDimCanvases)
  class(o) <- 'emuDB.EMUwebAppConfig.perspective'
  invisible(o)
}
create.EMUwebAppConfig <- function(perspectives){
  o <- list(perspectives=perspectives)
  class(o) <- 'emuDB.EMUwebAppConfig'
  invisible(o)
}


#setRefClass("emuDB",
#                     fields=list(name='character',basePath='character',schema='list',sessions='list',primaryextension='character',loadBundles='function',annotation='data.frame'),
#                     methods=list()
#                     )

create.database <- function(name=name,basePath=NULL,DBconfig,sessions=NULL,primaryExtension=NULL){
  o <- list(name=name,basePath=basePath,DBconfig=DBconfig,sessions=sessions,primaryExtension=primaryExtension)
  class(o) <- c('emuDB','list')
  invisible(o)
}

"as.emuDB"<-function(o,class){
  if(class=='emuDB'){
    return(o)
  }
  if(class=='list'){
    class(o) <- c('list')
    return(o)
  }
}


# Create emuDB bundle object
# @description A bundle typically contains media files and annoations of an utterance
# @param name name of the bundle
# @param annotates annotated signal file relative path
# @param sampleRate sample rate
# @param signalpaths pathes of signal files
# @param mediaFilePath path patter of samples track
# @param levels list of annotation levels
# @param links list of links containing the hierarchical information of the annotation levels
# @return object of class emuDB.bundle
# @author Klaus Jaensch
# @keywords emuDB bundle Emu
# 
create.bundle <- function(name,annotates=NULL,sampleRate,signalpaths=list(),mediaFilePath=NULL,levels=list(),links=list()){
  o <- list(name=name,annotates=annotates,sampleRate=sampleRate,signalpaths=signalpaths,mediaFilePath=mediaFilePath,files=signalpaths,levels=levels,links=links)
  return(as.bundle(o))
}

as.bundle <- function(bundleData){
  class(bundleData) <- 'emuDB.bundle'
  attr(bundleData,'ips.persist')<-list(typesJSON=list(levels='array'))
  invisible(bundleData)
}

remove.redundant.links<-function(database,links){
  # Legacy EMU and query functions link collections contain links for each possible connection between levels
  # We consider links that do not follow link definition constraints as redundant and therefore we remove them from the
  # link data model
  #
  # build SQL query from link definitions
  items=database[['items']]
  sqlQuery="SELECT l.* FROM items f,items t,links l WHERE f.bundle=t.bundle AND l.bundle=f.bundle AND f.bundleId=l.fromID AND t.bundleId=l.toID AND ("
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
  sqlQuery="SELECT l.* FROM items f,items t,links l WHERE f.bundle=t.bundle AND l.bundle=f.bundle AND f.bundleId=l.fromID AND t.bundleId=l.toID AND ("
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
  database[['links']]=sqldf(sqlQuery)
  return(database)
}

get.link.level.children<-function(DBconfig,superlevelName){
  subLds=list()
  for(ld in schema[['linkDefinitions']]){
    if(ld[['superlevelName']]==superlevelName){
      subLds[[length(subLds)+1L]]=ld
    }
  }
  return(subLds)
}

get.root.level.names<-function(schema){
  rlNames=character(0)
  for(lvlD in schema[['levelDefinitions']]){
    hasSuperLevel=FALSE
  for(ld in schema[['linkDefinitions']]){
    if(ld[['sublevelName']]==lvlD[['name']]){
        hasSuperLevel=TRUE
        break
    }
  }
  if(!hasSuperLevel){
    rlNames=c(rlNames,lvlD[['name']])
  }
  }
  return(rlNames)
}

get.level.names<-function(schema){
  lNames=character(0)
  for(lvlD in schema[['levelDefinitions']]){
    lNames=c(lNames,lvlD[['name']])
  }
  return(lNames)
}

get.attribute.names<-function(schema){
  aNames=character(0)
  for(lvlD in schema[['levelDefinitions']]){
    for(ad in lvlD[['attributeDefinitions']]){
      aNames=c(aNames,ad[['name']])
    }
    
  }
  return(aNames)
}


get.link.level.children.names<-function(schema,superlevelName){
  chNames=character(0)
  for(ld in schema[['linkDefinitions']]){
    if(ld[['superlevelName']]==superlevelName){
      chNames=c(chNames,ld[['sublevelName']])
    }
  }
  return(chNames)
}


build.level.partial.pathes<-function(schema,fromLevelName,toLevelName){
  pathes=list()
  chNames=get.link.level.children.names(schema,fromLevelName)
  if(length(chNames)==0){
    #pathes[[length(pathes)+1]]=c(levelName)
    
  }else{
    for(chName in chNames){
      if(chName==toLevelName){
        # terminated
        pathes[[length(pathes)+1L]]=c(fromLevelName,chName)
      }else{
        chPathes=build.level.partial.pathes(schema,chName,toLevelName)
        for(chPath in chPathes){
          pathes[[length(pathes)+1L]]=c(fromLevelName,chPath)
        }
      }
    }
  }
  return(pathes)
}

build.sublevel.pathes<-function(schema,levelName){
  pathes=list()
  chNames=get.link.level.children.names(schema,levelName)
  if(length(chNames)==0){
    pathes[[length(pathes)+1L]]=c(levelName)
  }else{
  for(chName in chNames){
  
    chPathes=build.sublevel.pathes(schema,chName)
    for(chPath in chPathes){
      pathes[[length(pathes)+1L]]=c(levelName,chPath)
    }
  }
  }
  return(pathes)
}

extract.link.targets.from.pathes<-function(pathes){
    mergedTargets=character(0)
    for(p in pathes){
      pLen=length(p)
      trgs=p[2:pLen]
      mergedTargets=c(mergedTargets,trgs)
    }
    uniqueTargets=unique(mergedTargets)
    return(uniqueTargets)
}

expand.link.path<-function(p){
  expPath=list()
  pLen=length(p)
  if(pLen==1){
    return(list())
  }
  expPath[[length(expPath)+1L]]=p
  expPath=c(expPath,expand.link.path(p[1:(pLen-1)]))
  return(expPath)
}

build.link.defs<-function(schema){
  extLds=list()
  for(ld in schema[['levelDefinitions']]){
    lName=ld[['name']]
     pathes=build.sublevel.pathes(schema,lName)
    for(p in pathes){
      extLds=c(extLds,expand.link.path(p))
    }
  }
 
  return(unique(extLds))
}


build.level.pathes<-function(schema){
    pathes=list()
  chNames=character(0)
  for(l in schema[['levelDefinitions']]){
    lPathes=build.sublevel.pathes(schema,l[['name']])
    pathes=c(pathes,lPathes)
  }
  
#   rlNames=get.root.level.names(schema)
#   for(rlName in rlNames){
#     rlPathes=build.sublevel.pathes(schema,rlName)
#     pathes=list(pathes,rlPathes)
#   }
  return(pathes)
}

build.ext.link.definitions<-function(schema){
  lds=list()
  pathes=build.level.pathes(schema)
  for(p in pathes){
    pLen=length(p)
    for(i in 1:pLen){
      ld=character(0)
      for(j in i:pLen){
        ld=c(ld,p[j])
      }
      lds[[length(lds)+1L]]=ld
    }
  }
  return(lds)
}

build.redundant.links.all<-function(database,bundleName=NULL){
  # Legacy EMU and query functions link collections contain links for each possible connection between levels
  # We consider links that do not follow link definition constraints as redundant and therefore we remove them from the
  # link data model
  #
 
  lfs=build.link.defs(database[['DBconfig']])
  maxLfLen=0
  for(lf in lfs){
    lfLen=length(lf)
    if(lfLen>maxLfLen){
      maxLfLen=lfLen
    }
  }
  return(build.redundant.links.for.pathes(database,lfs,bundleName) )
  
}

build.redundant.links<-function(database,fromLevel,toLevel){
  # Legacy EMU and query functions link collections contain links for each possible connection between levels
  # We consider links that do not follow link definition constraints as redundant and therefore we remove them from the
  # link data model. For queries we build links for particular start and end level.
  #
  
  lfs=build.level.partial.pathes(database[['DBconfig']],fromLevel,toLevel)
 
  return(build.redundant.links.for.pathes(database,lfs) )
}
  

build.redundant.links.for.pathes<-function(database,lfs,bundleName=NULL){
  maxLfLen=0
  for(lf in lfs){
    lfLen=length(lf)
    if(lfLen>maxLfLen){
      maxLfLen=lfLen
    }
  }
  items=database[['items']]
  links=database[['links']]
  sqlQuery="SELECT DISTINCT f.bundle,f.bundleId AS fromID,t.bundleId AS toID FROM items f,items t"
  sqlQuery=paste0(sqlQuery,' WHERE f.bundle=t.bundle AND ')
  if(!is.null(bundleName)){
     # only for one bundle
    sqlQuery=paste0(sqlQuery,"f.bundle='",bundleName,"' AND ")
  }
  #if(maxLfLen>2){
  #  for( ic in 2:(maxLfLen-1)){
  #    sqlQuery=paste0(sqlQuery,'i',ic,'.bundle=f.bundle AND ')
  #  }
  #}
  sqlQuery=paste0(sqlQuery,' (')
  ## TEST
  #lfs=list(c('Phoneme','Phonetic'))
  # build query for each partial path
  lfsLen=length(lfs)
  for(i in 1:lfsLen){
    lf=lfs[[i]]
    #cat("Path: ",lf,"\n")
    lfLen=length(lf)
    sLf=lf[1]
    eLf=lf[lfLen]
    sqlQuery=paste0(sqlQuery,"(f.level='",sLf,"' AND t.level='",eLf,"'" )
    sqlQuery=paste0(sqlQuery," AND EXISTS (SELECT l1.* FROM ")
    for(li in 1:(lfLen-1)){
      sqlQuery=paste0(sqlQuery,'links l',li)
      if(li<(lfLen-1)){
        sqlQuery=paste0(sqlQuery,',')
      }
    }
    if(lfLen>2){
      for(ii in 2:(lfLen-1)){
        sqlQuery=paste0(sqlQuery,',items i',ii)
      }
    }
    sqlQuery=paste0(sqlQuery," WHERE ")
    if(lfLen==2){
      sqlQuery=paste0(sqlQuery,"l1.bundle=f.bundle AND l1.bundle=t.bundle AND f.bundleID=l1.fromID AND t.bundleId=l1.toID")
      #cat(sLf,eLf,"\n")
    }else{
      # TODO start and end connection
      # from start to first in-between item 
      eLf=lf[2]
      #cat(sLf,eLf,"\n")
      sqlQuery=paste0(sqlQuery,"l1.bundle=f.bundle AND l1.bundle=i2.bundle AND f.bundleID=l1.fromID AND i2.bundleId=l1.toID AND f.level='",sLf,"' AND i2.level='",eLf,"' AND ")
      if(lfLen>3){
        for(j in 2:(lfLen-2)){
          sLf=lf[j]
          eLf=lf[j+1L] 
          #cat(sLf,eLf,"\n")
          sqlQuery=paste0(sqlQuery,"l",j,".bundle=i",j,".bundle AND l",j,".bundle=i",(j+1),".bundle AND i",j,".bundleID=l",j,".fromID AND i",(j+1L),".bundleId=l",j,".toID AND i",j,".level='",sLf,"' AND i",(j+1L),".level='",eLf,"' AND ")
        }
      }
      # from last in-between item to end item
      sLf=lf[(lfLen-1)]
      eLf=lf[lfLen]
      #cat(sLf,eLf,(lfLen-1),"\n")
      j=lfLen-1
      sqlQuery=paste0(sqlQuery,"l",j,".bundle=i",j,".bundle AND l",j,".bundle=t.bundle AND i",j,".bundleID=l",j,".fromID AND t.bundleId=l",j,".toID AND i",j,".level='",sLf,"' AND t.level='",eLf,"'")
    }
    sqlQuery=paste0(sqlQuery,"))")
    if(i<lfsLen){
      sqlQuery=paste0(sqlQuery," OR ")
    }
  }
  sqlQuery=paste0(sqlQuery,")")
  #cat(sqlQuery,"\n")
  nls=sqldf(sqlQuery)
  return(nls)
}

get.level.name.for.attribute<-function(db,attributeName){
  for(ld in db[['DBconfig']][['levelDefinitions']]){
    for(ad in ld[['attributeDefinitions']]){
      if(ad[['name']]==attributeName){
        return(ld[['name']])
      }
    }
  }
  return(NULL)
}

move.bundle.levels.to.data.frame <-function(db,bundle,replace=TRUE){
  # do not use this function to append multiple bundles (performance is bad)
  # growing data.frames with rbind() is unefficient

  DBconfig=db[['DBconfig']]
  row=1 
  #bdf=data.table(matrix(ncol=length(db[['DBconfig']][['itemColNames']]),nrow=0))
  # caclculate bundle items length
  itCount=0
  for(lvl in bundle[['levels']]){
    lvlItCount=length(lvl[['items']])
    itCount=itCount+lvlItCount
  }
  bdf=data.frame(id=character(itCount),bundle=character(itCount),level=character(itCount),bundleId=integer(itCount),type=character(itCount),seqIdx=integer(itCount),sampleRate=numeric(itCount),samplePoint=integer(itCount),sampleStart=integer(itCount),sampleDur=integer(itCount),label=character(itCount),stringsAsFactors=FALSE)
  #colnames(bdf)<-db[['DBconfig']][['itemColNames']]
  ldf=NULL
  lrow=1
  maxLbls=db[['DBconfig']][['maxNumberOfLabels']]
  lblColNames=c('itemID','bundle','labelIdx','name','label')
  ldf=data.frame(itemID=character(0),bundle=character(0),labelIdx=integer(0),name=character(0),label=character(0),stringsAsFactors=FALSE)
  colnames(ldf)<-lblColNames
  
  for(lvl in bundle[['levels']]){
    
    seqIdx=0L
    for(it in lvl[['items']]){
      
      seqIdx=seqIdx+1L
      bName=bundle[['name']]
      bdf[row,'bundle']=bName
      itemId=it[['id']]
      if(is.null(itemId)){
        # for instance aetobi has no .hlb files and tehrefore no links and item ids
        id=paste(db[['name']],bName,sep='_')
        itemId=NA
      }else{
        id=paste(db[['name']],bName,it['id'],sep='_')
      }
      bdf[row,'id']=id
      bdf[row,'bundleId']=itemId
      bdf[row,'level']=lvl[['name']]
      bdf[row,'type']=lvl[['type']]
      if(!is.null(bundle[['sampleRate']])){
        bdf[row,'sampleRate']=bundle[['sampleRate']]
      }else{
        bdf[row,'sampleRate']=NA
      }
      bdf[row,'seqIdx']=seqIdx
      sp=it[['samplePoint']]
      if(!is.null(sp)){
        bdf[row,'samplePoint']=sp
      }else{
        bdf[row,'samplePoint']=NA
      }
      ss=it[['sampleStart']]
      if(!is.null(ss)){
        bdf[row,'sampleStart']=ss
      }else{
        bdf[row,'sampleStart']=NA
      }
      sd=it[['sampleDur']]
      if(!is.null(sd)){
        bdf[row,'sampleDur']=sd
      }else{
        bdf[row,'sampleDur']=NA
      }
      # TODO get label column names from schema and set them as last columns
      lbls=it[['labels']]
      lblsLen=length(lbls)
      lbl0=it[['labels']][[1]][['value']]
      bdf[row,'label']=lbl0
      for(i in 1:maxLbls){
        rLbl=NA
        if(lblsLen>=i){
          lbl=lbls[[i]]
          if(!is.null(lbl)){
            rLbl=lbl[['value']]
            ldf[lrow,'itemID']=id
            ldf[lrow,'bundle']=bName
            ldf[lrow,'labelIdx']=i-1
            ldf[lrow,'name']=lbl[['name']]
            ldf[lrow,'label']=rLbl
            lrow=lrow+1L
          }
        }
      } 
      row=row+1L
    }
  }
  if(replace){
    # remove old bundle data
    otherBundlesSelector=db[['items']][['bundle']]!=bName
    db[['items']]=db[['items']][otherBundlesSelector,]
    
    otherBundlesSelector=db[['labels']][['bundle']]!=bName
    db[['labels']]=db[['labels']][otherBundlesSelector,]
    
  }
  
  db[['items']]=rbind(db[['items']],bdf)
  if(!is.null(ldf)){
    db[['labels']]=rbind(db[['labels']],ldf)
  }
  return(db)
  
}




append.bundle.to.tmp.list <-function(db,bundle){
 
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
        
      }
      seqIdx=seqIdx+as.integer(1)
      
      #db[['items']][row,'bundle']=bName
      db[['items']][['bundle']][row]=bName
      itemId=it[['id']]
      if(is.null(itemId)){
        # for instance aetobi has no .hlb files and therefore no links and item ids
        id=paste(db[['name']],bName,sep='_')
        itemId=NA
      }else{
        id=paste(db[['name']],bName,it['id'],sep='_')
      }
      db[['items']][['id']][row]=id
      db[['items']][['bundleId']][row]=itemId
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
        db[['items']][['samplePoint']][row]=as.integer(sp)
      }else{
        db[['items']][['samplePoint']][row]=NA
      }
      ss=it[['sampleStart']]
      if(!is.null(ss)){
        db[['items']][['sampleStart']][row]=as.integer(ss)
      }else{
        db[['items']][['sampleStart']][row]=NA
      }
      sd=it[['sampleDur']]
      if(!is.null(sd)){
        db[['items']][['sampleDur']][row]=as.integer(sd)
      }else{
        db[['items']][['sampleDur']][row]=NA
      }
      
      lbls=it[['labels']]
      lblsLen=length(lbls)
      lbl0=it[['labels']][[1]][['value']]
      db[['items']][['label']][row]=lbl0
      for(i in 1:maxLbls){
        rLbl=NA
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
            }
            rLbl=lbl[['value']]
            db[['labels']][['itemID']][lrow]=id
            db[['labels']][['bundle']][lrow]=bName
            db[['labels']][['labelIdx']][lrow]=as.integer(i-1)
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
      
    }
    db[['links']][['bundle']][row]=bName
    db[['links']][['fromID']][row]=as.integer(lk[['fromID']])
    db[['links']][['toID']][row]=as.integer(lk[['toID']])
    lbl=lk[['label']]
    if(is.null(lbl)){
      db[['links']][['label']][row]=NA
    }else{
      db[['links']][['label']][row]=lbl
    }
  }
  return(db)
}


append.bundle.to.tmp.list.by.ref <-function(dbWr,bundle){
  
  
  DBconfig=dbWr[['db']][['DBconfig']]
  maxLbls=dbWr[['db']][['DBconfig']][['maxNumberOfLabels']]
  bName=bundle[['name']]
  for(lvl in bundle[['levels']]){
    
    seqIdx=0L
    for(it in lvl[['items']]){
      dbWr[['db']][['itemsIdx']]=dbWr[['db']][['itemsIdx']]+1L
      row=dbWr[['db']][['itemsIdx']]
      itemsVectorSize=length(dbWr[['db']][['items']][['bundle']])
      if(row>itemsVectorSize){
        # TODO increase vectors
      }
      seqIdx=seqIdx+1L
      
      #dbWr[['db']][['items']][row,'bundle']=bName
      dbWr[['db']][['items']][['bundle']][row]=bName
      itemId=it[['id']]
      if(is.null(itemId)){
        # for instance aetobi has no .hlb files and therefore no links and item ids
        id=paste(dbWr[['db']][['name']],bName,sep='_')
        itemId=NA
      }else{
        id=paste(dbWr[['db']][['name']],bName,it['id'],sep='_')
      }
      dbWr[['db']][['items']][['id']][row]=id
      dbWr[['db']][['items']][['bundleId']][row]=itemId
      dbWr[['db']][['items']][['level']][row]=lvl[['name']]
      dbWr[['db']][['items']][['type']][row]=lvl[['type']]
      if(!is.null(bundle[['sampleRate']])){
        dbWr[['db']][['items']][['sampleRate']][row]=bundle[['sampleRate']]
      }else{
        dbWr[['db']][['items']][['sampleRate']][row]=NA
      }
      dbWr[['db']][['items']][['seqIdx']][row]=seqIdx
      sp=it[['samplePoint']]
      if(!is.null(sp)){
        dbWr[['db']][['items']][['samplePoint']][row]=as.integer(sp)
      }else{
        dbWr[['db']][['items']][['samplePoint']][row]=NA
      }
      ss=it[['sampleStart']]
      if(!is.null(ss)){
        dbWr[['db']][['items']][['sampleStart']][row]=as.integer(ss)
      }else{
        dbWr[['db']][['items']][['sampleStart']][row]=NA
      }
      sd=it[['sampleDur']]
      if(!is.null(sd)){
        dbWr[['db']][['items']][['sampleDur']][row]=as.integer(sd)
      }else{
        dbWr[['db']][['items']][['sampleDur']][row]=NA
      }
      
      lbls=it[['labels']]
      lblsLen=length(lbls)
      lbl0=it[['labels']][[1]][['value']]
      dbWr[['db']][['items']][['label']][row]=lbl0
      for(i in 1:maxLbls){
        rLbl=NA
        if(lblsLen>=i){
          lbl=lbls[[i]]
          if(!is.null(lbl)){
            dbWr[['db']][['labelsIdx']]=dbWr[['db']][['labelsIdx']]+1L
            lrow=dbWr[['db']][['labelsIdx']]
            rLbl=lbl[['value']]
            dbWr[['db']][['labels']][['itemID']][lrow]=id
            dbWr[['db']][['labels']][['bundle']][lrow]=bName
            dbWr[['db']][['labels']][['labelIdx']][lrow]=as.integer(i-1)
            dbWr[['db']][['labels']][['name']][lrow]=lbl[['name']]
            dbWr[['db']][['labels']][['label']][lrow]=rLbl
            
          }
        }
      } 
      
    }
  }
  
  for(lk in bundle[['links']]){
    dbWr[['db']][['linksIdx']]=dbWr[['db']][['linksIdx']]+1L
    row=dbWr[['db']][['linksIdx']]
    dbWr[['db']][['links']][['bundle']][row]=bName
    dbWr[['db']][['links']][['fromID']][row]=as.integer(lk[['fromID']])
    dbWr[['db']][['links']][['toID']][row]=as.integer(lk[['toID']])
    lbl=lk[['label']]
    if(is.null(lbl)){
      dbWr[['db']][['links']][['label']][row]=NA
    }else{
      dbWr[['db']][['links']][['label']][row]=lbl
    }
  }
  return()
}

get.bundle.levels.s3 <-function(db,bundleName){
  
  levelDefinitions=db[['DBconfig']][['levelDefinitions']]
  find.levelDefinition<-function(name){
    for(lvlDef in levelDefinitions){
      if(name == lvlDef[['name']]){
        return(lvlDef)
      }
    }
  }
  bundleSelector=db[['items']][['bundle']]==bundleName
  items=db[['items']][bundleSelector,]
  bundleSelector=db[['labels']][['bundle']]==bundleName
  bundleLabels=db[['labels']][bundleSelector,]
  
  nrows=nrow(items)
  cLvl=NULL
  lvlDef=NULL
  lvlItems=list()
  levels=list()
  if(nrows>0){
  for(r in 1:nrows){
    rLvl=items[r,'level']
      if(!is.null(cLvl) && cLvl[['name']]!=rLvl){
        cLvl[['items']]=lvlItems
        levels[[cLvl[['name']]]]=cLvl
        cLvl=NULL
      }
    
  if(is.null(cLvl)){
    
    lvlDef=find.levelDefinition(rLvl)
    lvlItems=list()
    sr=NULL
    srDf=items[r,'sampleRate']
    if(!is.na(srDf)){
      sr=srDf
    }
    cLvl=create.bundle.level(name=rLvl,type=items[r,'type'],sampleRate=sr,items=lvlItems)
  }
      id=items[r,'bundleId']
      type=items[r,'type']
    
      attrDefs=lvlDef[['attributeDefinitions']]
      attrDefsLen=length(attrDefs)
      
      gid=items[r,'id']
      itemLabelSelector=bundleLabels[['itemID']]==gid
      labelRows=bundleLabels[itemLabelSelector,]
      nLabelRows=nrow(labelRows)
       labels=list()
      for(j in 1:nLabelRows){
     
      
      lblNm=labelRows[j,'name']
      labels[[j]]=list(name=lblNm,value=labelRows[j,'label'])
    
      }
      
    
      if(type=='SEGMENT'){
        lvlItems[[length(lvlItems)+1L]]=create.interval.item(id=id,sampleStart=items[r,'sampleStart'],sampleDur=items[r,'sampleDur'],labels=labels)
      }else if(type=='EVENT'){
        lvlItems[[length(lvlItems)+1L]]=create.event.item(id=id,samplePoint=items[r,'samplePoint'],labels=labels)
      }else{
        lvlItems[[length(lvlItems)+1L]]=create.item(id=id,labels=labels)  
      }
  }
  # add last level
  cLvl[['items']]=lvlItems
  levels[[cLvl[['name']]]]=cLvl
  }
  return(levels)
}

convert.bundle.links.to.data.frame <-function(links){
  df=data.frame(stringsAsFactors=FALSE)
  row=0
  for(lk in links){
      row=row+1L
      df[row,'fromID']=lk[['fromID']]
      df[row,'toID']=lk[['toID']]
      lbl=lk[['label']]
      if(!is.null(lbl)){
        df[row,'label']=lbl
      }
  }
  invisible(df)
}

move.bundle.links.to.data.frame <-function(db,bundle,replace=TRUE){
  
  row=1
  bdf=data.frame(bundle=character(0),fromID=integer(0),toID=integer(0),label=character(0),stringsAsFactors=FALSE)
  bName=bundle[['name']]
  row=0
  for(lk in bundle[['links']]){
    row=row+1L 
    bdf[row,'bundle']=bName
    bdf[row,'fromID']=lk[['fromID']]
    bdf[row,'toID']=lk[['toID']]
    lbl=lk[['label']]
    if(!is.null(lbl)){
      bdf[row,'label']=lbl
    }else{
      bdf[row,'label']=NA
    }
  }
  if(replace){
    # remove old bundle data
    otherBundlesSelector=db[['links']][['bundle']]!=bName
    db[['links']]=db[['links']][otherBundlesSelector,]
    otherBundlesSelector=db[['linksExt']][['bundle']]!=bName
    db[['linksExt']]=db[['linksExt']][otherBundlesSelector,]
  }

  db[['links']]=rbind(db[['links']],bdf)
  redLinksBundle=build.redundant.links.all(database = db,bundleName=bName)
  #TODO  put level and links method together and use only items of the bundle
  redExtLinksBundle=calculate.postions.of.links(db[['items']],redLinksBundle)
  db[['linksExt']]=rbind(db[['linksExt']],redExtLinksBundle)
  
  return(db)
}




get.bundle.links.s3 <-function(db,bundleName){
  bundleSelector=db[['links']][['bundle']]==bundleName
  linksDf=db[['links']][bundleSelector,]
  nrows=nrow(linksDf)
  
  links=vector(mode='list',length=nrows)
  if(nrows>0){
    for(row in 1:nrows){
      
      links[[row]][['fromID']]=linksDf[row,'fromID']
      
      links[[row]][['toID']]=linksDf[row,'toID']
      lbl=linksDf[row,'label']
      if(!is.null(lbl) && !is.na(lbl)){
        links[[row]][['label']]=lbl
      }
    }
  }
  return(links)
}



convert.bundle.single.data.framed <- function(db,b,replace=TRUE){
  # convert levels to data.frame
  
  db=move.bundle.levels.to.data.frame(db,b,replace)
  #cat("move levels ",st,"\n")
  b[['levels']]=NULL
  
  # convert links
  db=move.bundle.links.to.data.frame(db,b,replace)
  #cat("move links ",st,"\n")
  b[['links']]=NULL
  return(db)
}

## Returns bundle as S3 object
## 
## @param db database
## @param bundleName name of bundle
## @return bundle in S3 format
## @author Klaus Jaensch
## @keywords emuDB database schema Emu bundle
## 
get.bundle <- function(db,bundleName){
  
  schema=db[['DBconfig']]
  bundle=get.bundle.stub(db,bundleName)
  if(!is.null(bundle)){
    bundle=get.bundle.s3(db,bundle)
  }
  return(bundle)
}

get.bundle.stub<-function(db,bundleName){
  sessCount=length(db[['sessions']])
  bundleStub=NULL
  for(s in 1:sessCount){
    sbNms=names(db[['sessions']][[s]][['bundles']])
    if(bundleName %in% sbNms){
      # this session contains requested bundle
      bundleStub=db[['sessions']][[s]][['bundles']][[bundleName]]
      break
    }
  }
  return(bundleStub)
}
## Converts bundle in data frame format to S3 format
## 
## @param db database
## @param bundle bundle stub
## @return bundle in S3 format
## @author Klaus Jaensch
## @keywords emuDB database DBconfig Emu bundle
## 
get.bundle.s3 <- function(db,bundle){
  
  schema=db[['DBconfig']]
  bName=bundle[['name']]
  # convert levels to s3
  bundle[['levels']]=get.bundle.levels.s3(db,bName)
  bundle[['itemsDataFrame']]=NULL
  # convert links
  bundle[['links']]=get.bundle.links.s3(db,bName)
  bundle[['linksDataFrame']]=NULL
  return(bundle)
}



emuDB.print.bundle <- function(utt){
  cat("code=",utt[['name']],"\n")
  cat("signalurls:\n")
  for(mf in utt[['signalpaths']]){
    print(mf) 
  }
  cat("levels:\n")
  for(a in utt[['levels']]){
    print(a) 
  }
}

emuDB.session <- function(name=NULL,path=NULL,bundles=NULL){
  o <- list(name=name,path=path,bundles=bundles)
  class(o) <- 'emuDB.session'
  invisible(o)
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

is.relative.file.path<-function(nativeFilePathStr,forRunningPlatform=FALSE){
  if(forRunningPlatform){
    if(.Platform[['OS.type']]=='unix'){
      if(.Platform[['file.sep']]==substr(nativeFilePathStr,1,1)){
        # UNIX: "/dir/file"
        # absolute path
        return(FALSE)
      }
    }else if(.Platform[['OS.type']]=='windows'){
      #See http://msdn.microsoft.com/en-us/library/windows/desktop/aa365247%28v=vs.85%29.aspx
      if(substr(nativeFilePathStr,1,2)=='\\'){
        # fully qualified MS UNC path (is this supported with R?): \\samba\bla
        return(FALSE)
      }else if(grepl('^[A-Z,a-z][:]',nativeFilePathStr)){
        # fully qualified drive path: C:\Users\bla
        return(FALSE)
      }else if(.Platform[['file.sep']]==substr(nativeFilePathStr,1,1)){
        # Windows: "\dir\file"
        # absolute path
        return(FALSE)
      }
    }
  }else{
    if(grepl('^[A-Z,a-z][:]',nativeFilePathStr)){
      return(FALSE)
    }
    if(grepl('^[\\]',nativeFilePathStr)){
      return(FALSE)
    }
    if(grepl('^/',nativeFilePathStr)){
      return(FALSE)
    }
    
  }
  return(TRUE)
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
  if(is.null(emuTemplatePath) || ''==emuTemplatePath){
    homePath=Sys.getenv('HOME')
    emuUserDir=file.path(homePath,'.emu')
   
    if(file.exists(emuUserDir)){
      emuConfFile=file.path(emuUserDir,'emu-conf')
      
      if(file.exists(emuConfFile)){
        
        lc = try(readLines(emuConfFile))
        if(class(lc) == "try-error") {
          stop("Cannot read ",emuConfFile)
        }
        for(l in lc){
          
          kv=parse.line.to.key.value(l)
          if(!is.null(kv)){
          if(kv[1]=='#EMU_TEMPLATE_PATH'){
            etpSpl=strsplit(kv[2],':')
            return(etpSpl[[1]])
          
          }
          }
        }
      }
    }
  }
}

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
dirList=list.trackdirs(pathPattern)
fileList=c()
for(dir in dirList){
  fqDir=dir
  if(is.relative.file.path(dir)){
    fqDir=file.path(basePath,dir)
  }
  pFileList = list.files(fqDir, pattern=filePattern, recursive=T, full.names=T)
  fileList=c(fileList,pFileList)
}
return(fileList)
}

find.file.in.emu.path.pattern=function(emuPathPattern,fileName,basePath=NULL){
  dirList=list.trackdirs(emuPathPattern)
  for(dir in dirList){
    fqDir=dir
    if(is.relative.file.path(dir)){
      fqDir=file.path(basePath,dir)
    }
    tfp=paste0(fqDir,'/',fileName)
    if(file.exists(tfp)){
     return(tfp)
    }
  }
  return(NULL)
}


initialize.database.dataframes<-function(db){
  
  baseColNms <- c('id','bundle','level','bundleId','type','seqIdx','sampleRate','samplePoint','sampleStart','sampleDur','label')
  maxLbls=db[['DBconfig']][['maxNumberOfLabels']]
  
  colNms=baseColNms
  db[['DBconfig']][['itemColNames']]=colNms
  


  # items 
  db[['items']]=list(id=character(vector.increment),bundle=character(vector.increment),level=character(vector.increment),bundleId=integer(vector.increment),type=character(vector.increment),seqIdx=integer(vector.increment),sampleRate=numeric(vector.increment),samplePoint=integer(vector.increment),sampleStart=integer(vector.increment),sampleDur=integer(vector.increment),label=character(vector.increment))
  #db[['items']]=matrix(nrow=itCount,ncol=length(colNms))w
  #colnames(db[['items']])=colNms
  db[['itemsIdx']]=0L
  # label table
  #lblColNms=c('itemID','bundle','labelIdx','name','label')
  #db[['labels']]=data.frame(matrix(ncol=length(lblColNms),nrow=0),stringsAsFactors=FALSE)
  db[['labels']]=list(itemID=character(vector.increment),bundle=character(vector.increment),labelIdx=integer(vector.increment),name=character(vector.increment),label=character(vector.increment))
  #db[['labels']]=matrix(nrow=vector.increment,ncol=length(lblColNms))
  #colnames(db[['labels']])=lblColNms
  db[['labelsIdx']]=0L
 
  #linkColNms=c('bundle','fromID','toID','label')
  db[['links']]=list(bundle=character(vector.increment),fromID=integer(vector.increment),toID=integer(vector.increment),label=character(vector.increment))
  #db[['links']]=matrix(nrow=itCount,ncol=length(linkColNms))
 #colnames(db[['links']])=linkColNms
  db[['linksIdx']]=0L

  #colnames(db[['labels']])<-lblColNms
  return(db)
}

## Create emuDB database schema object from EMU template (.tpl) file
## 
## @param tplPath EMU template file path
## @return object of class emuDB.schema.db
## @author Klaus Jaensch
## @import stringr uuid wrassp
## @keywords emuDB database schema Emu 
## 
load.database.schema.from.emu.template=function(tplPath){
  LEVEL_CMD='level'
  LABFILE_CMD='labfile'
  LABEL_CMD='label'
  SET_CMD='set'
  TRACK_CMD='track'
  PATH_CMD='path'
  LEGAL_CMD='legal'
  
  if(is.null(tplPath)) {
    stop("Argument tplPath (path to Emu template file) must not be NULL\n")
  }
  tplBasename = basename(tplPath)
  dbName=gsub("[.][tT][pP][lL]$","",tplBasename)
  # read
  tpl = try(readLines(tplPath))
  if(class(tpl) == "try-error") {
    stop("read tpl: cannot read from file ", tplPath)
  }

  tracks=list()
  flags=list()
  levelDefinitions=list()
  linkDefinitions=list()
  pathDescriptors=list()
  annotationDescriptors=list()
  hlbTierDescriptors=list()
  hlbAnnotationDescriptor=NULL;
  
  lineNr=0
  
  for(line in tpl){
    lineNr=lineNr+1L
    trimmedLine=str_trim(line)
    if(trimmedLine!=''){
      firstChar=substr(trimmedLine,1,1)
      if(firstChar!='!'){
        
        lineTokensLst=strsplit(trimmedLine,'[[:space:]]+')
        lineTokens=lineTokensLst[[1]]
        lineTokenCount=length(lineTokens)
        if(lineTokenCount>=1){
          command=lineTokens[1]
          
          if(command==LABFILE_CMD){
            tierName=lineTokens[2]
            # TODO are there any default values for this properties?
            extension=NULL
            type=NULL
            timeFactor=NULL
            for(tki in 3:length(lineTokens)){
              tk=lineTokens[[tki]]
              
              if(substr(tk,1,1)==':'){
                # property key
                key=substring(tk,2)
              }else{
                #property value
                if(is.null(key)){
                  stop("Emu template parser key/value error in ",lineNr,"\n")
                }
                val=tk
                if(key=='extension'){
                  extension=val
                }else if(key=='type'){
                  type=val
                }else if(key=='time-factor'){
                  timeFactor=val
                }
                #cat("Tier: ",tierName,key,val,"\n")
                # reset key
                key=NULL 
              }
            }
            ad=create.schema.annotationDescriptor(name=tierName,extension=extension,type=type,timeFactor=timeFactor)
            annotationDescriptors[[length(annotationDescriptors)+1L]] <- ad
            
            # lab file can reference hlb level
            replaced=FALSE
            tdLen=length(levelDefinitions)
            for(i in 1:tdLen){
              td=levelDefinitions[[i]]
              if(td[['name']]==tierName){
                # replace 
              
                levelDefinitions[[i]]=create.schema.levelDefinition(name=td[['name']],type=type,attributeDefinitions=td[['attributeDefinitions']]);
                replaced=TRUE
                break;
              }
            }
            if(!replaced){
              # append
              levelDefinitions[[length(levelDefinitions)+1L]]=create.schema.levelDefinition(name=tierName);
            }
          }else if(command==TRACK_CMD){
            
            name=lineTokens[2]
            extension=lineTokens[3]
            #cat("Command: ",command,name,'\n')
            track=create.schema.track(name=name,extension=extension)
            tracks[[length(tracks)+1L]] <- track
          }else if(command==SET_CMD){
            key=lineTokens[2]
            value=lineTokens[3]
            flags[[key]]=value
          }else if(command==PATH_CMD){
            annoKey=lineTokens[2]
            annoBasePath=lineTokens[3]
            pathDescr=list(basePath=annoBasePath,key=annoKey)
            pathDescriptors[[length(pathDescriptors)+1L]] <- pathDescr
            if(annoKey=='hlb'){
              # special meaning
              # hlb files are neither declared by tracks nor by labfile directive
              # add as annotationDescriptor
              ad=create.schema.annotationDescriptor(name=NULL,extension=annoKey,type='HLB')
              annotationDescriptors[[length(annotationDescriptors)+1L]] <- ad
            }
          }else if(command==LEVEL_CMD){
           
            levelTierName=lineTokens[2]
            
            if(lineTokenCount>=3){
              linkType="ONE_TO_MANY"
              if(lineTokenCount>=4){
                relationshipType=lineTokens[4]
                if(relationshipType=='many-to-many'){
                  linkType="MANY_TO_MANY"
                }
              }
              linkDefinition=create.schema.linkDefinition(type=linkType,superlevelName=lineTokens[3],sublevelName=levelTierName)
              linkDefinitions[[length(linkDefinitions)+1L]]=linkDefinition
            }
            tierDescr=create.schema.levelDefinition(name=levelTierName,type='ITEM');
            exists=FALSE
            for(lDef in levelDefinitions){
              if(lDef[['name']]==levelTierName){
                exists=TRUE
                break
              }
            }
            if(!exists){
              levelDefinitions[[length(levelDefinitions)+1L]]=tierDescr
            }
            
            # TODO constraints
          }else if(command==LABEL_CMD){
            
            levelTierName=lineTokens[2]
            labelNames=list(levelTierName)
            if(lineTokenCount!=3){
              stop("Expected label directive \"label levelName labelName\"")
            }
            
            for(i in 1:length(levelDefinitions)){
              td=levelDefinitions[[i]]
              if(td[['name']]==levelTierName){
                # replace
                attrDefs=levelDefinitions[[i]][['attributeDefinitions']]
                attrDefs[[length(attrDefs)+1L]]=create.schema.attributeDefinition(name=lineTokens[3])
                levelDefinitions[[i]]=create.schema.levelDefinition(name=levelTierName,type=td[['type']],attributeDefinitions=attrDefs);
                break
              }
            }
          }else if(command==LEGAL_CMD){
            if(lineTokenCount<=3){
              stop("Expected legal directive \"legal levelName className label1 label2 ... labeln\"")
            }
            levelName=lineTokens[2]
            labelClassName=lineTokens[3]
           
            classLabels=c()
            for(i in 4:lineTokenCount){
              classLabels=c(classLabels,lineTokens[i])
            }
            
            for(i in 1:length(levelDefinitions)){
              td=levelDefinitions[[i]]
              if(td[['name']]==levelName){
                td[['labelClasses']][[labelClassName]]=list(name=labelClassName,labels=classLabels)
                break
              }
            }
          }
        }
      }
    }
  }
  
  #pef=flags$PrimaryExtension
  tl=length(tracks)
  al=length(annotationDescriptors)
  
  for(pd in pathDescriptors){
    
    #isTrack=FALSE
    tss2=1:tl
    for(ti2 in tss2){
      
      if(tracks[[ti2]][['fileExtension']] == pd[['key']]){
        
        tracks[[ti2]][['basePath']]=pd[['basePath']]
        #cat("set track path for key:",pd$key,"\n")
        break
      }
    }
    
    as=1:al
    for(ai in as){
      if(annotationDescriptors[[ai]][['extension']] == pd[['key']]){
        
        annotationDescriptors[[ai]][['basePath']]=pd[['basePath']]
        #cat("set anno path for key:",pd$key,"\n")
        break
      }
      
    }
  }
 
  ssffTrackDefinitions=list()
  assign=list()
  mediafileBasePathPattern=NULL
  mediafileExtension=NULL
  for(tr in tracks){
    n=tr[['name']]
    e=tr[['fileExtension']]
    if(e==flags[['PrimaryExtension']]){
      primaryBasePath=tr[['basePath']]
    }
    if(n=='samples'){
      if(e!='wav'){
        cat("WARNING! Media file type with extension ",e," not supported by emuLVC.")
      }
      mediafileExtension=e
      mediafileBasePathPattern=tr[['basePath']]
    }else{
      #ssffTracks[[n]]=tr
      #array !
      ssffTrackDefinitions[[length(ssffTrackDefinitions)+1L]]=tr
      # default assign all to spectrum TODO
      
    }
  }

  # Generate UUID 
  # problem: the UUID will cahnge on every reload
  uuid=UUIDgenerate()
 
  # default perspective
  # assign all SSFF tracks to sonagram
  assign=list()
  contourLims=list()
  for(ssffTrack in ssffTrackDefinitions){
    #cat(ssffTrack$name,"\n")
    # TODO dirty workaround
    # detect formant tracks by number of channels
    if(ssffTrack[['name']] == 'fm'){
      #ssffTrack$name='FORMANTS'
      #cat('Assigned: ',ssffTrack$name,"\n")
      #assign[[length(assign)+1]]=list(signalCanvasName='SPEC',ssffTrackName='FORMANTS')
    }
  }
   
  sc=create.EMUwebAppConfig.signalCanvas(order=c("OSCI","SPEC"),assign=assign,contourLims=contourLims)
  
  defaultLvlOrder=list()
  for(ld in levelDefinitions){
   
    if(ld[['type']]=='SEGMENT' || ld[['type']]=='EVENT'){
    defaultLvlOrder[[length(defaultLvlOrder)+1L]]=ld[['name']]
    #cat(ld$name,"\n")
    }
  }
  
  defPersp=create.EMUwebAppConfig.perspective(name='default',signalCanvases=sc,levelCanvases=list(order=defaultLvlOrder),twoDimCanvases=list(order=list()))
  waCfg=create.EMUwebAppConfig(perspectives=list(defPersp))
  #waCfg$activeButtons=list(saveBundle=TRUE)
  dbSchema=create.schema.databaseDefinition(name=dbName,UUID=uuid,mediafileBasePathPattern=mediafileBasePathPattern,mediafileExtension=mediafileExtension,ssffTrackDefinitions=ssffTrackDefinitions,levelDefinitions=levelDefinitions,linkDefinitions=linkDefinitions,EMUwebAppConfig=waCfg,annotationDescriptors=annotationDescriptors,tracks=tracks,flags=flags);
  
  # get max label array size
  maxLbls=0
  for(lvlDef in levelDefinitions){
    attrCnt=length(lvlDef[['attributeDefinitions']])
    if(attrCnt > maxLbls){
      maxLbls=attrCnt
    }
  }
  dbSchema[['maxNumberOfLabels']]=maxLbls
  return(dbSchema)
}


## @import stringr wrassp
load.annotation.for.legacy.bundle=function(schema,uttCode,basePath=NULL){
  # determine samplerate
  # fallback is primary file
  sampleRateReferenceFile=NULL
  sampleTrackFile=NULL
  if(!is.null(schema[['mediafileBasePathPattern']]) && ! is.null(schema[['mediafileExtension']])){
    # use samples track to determine sample rate
    sampleTrackFile=paste0(uttCode,'.',schema[['mediafileExtension']]) 
    # resolve wildcards
    sampleRateReferenceFile=find.file.in.emu.path.pattern(emuPathPattern=schema[['mediafileBasePathPattern']],fileName=sampleTrackFile,basePath)
  }
  if(is.null(sampleRateReferenceFile)){
    stop("Could not determine media sample rate of bundle ",uttCode,"\n")
  }else{
  #cat("Determine sample rate from file: ",sampleRateReferenceFile,"\n")
  pfAssp=read.AsspDataObj(sampleRateReferenceFile,0,4000)
  sampleRate=attr(pfAssp,'sampleRate')
  }
  
  # create signal paths list
  
  signalpaths=list() 
  for(tr in schema[['tracks']]){
   #cat("Track: ",tr$name," ",tr$fileExtension,"\n")
    sigFilename=str_c(uttCode,'.',tr[['fileExtension']])
    sFile=find.file.in.emu.path.pattern(tr[['basePath']],sigFilename,basePath)
   
    if(!is.null(sFile)){
      signalpaths[[length(signalpaths)+1L]]=sFile
    }
  }
  
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
      
      annoFilename=str_c(uttCode,'.',extension)
      annoPath=find.file.in.emu.path.pattern(annoBasePath,annoFilename,basePath)
      if(!is.null(annoPath)){
        #cat("Anno: ",annoPath,"\n")
        if(extension!='hlb'){
          # parse lab file
          labTier=parse.esps.label.file(labFilePath=annoPath,tierName=ad[['name']],sampleRate=sampleRate)
          if(!is.null(labTier)){
            levels[[labTier[['name']]]] <- labTier
          }
        }
      }
    }
    
  }
  # now hlb file
  for(ad in schema[['annotationDescriptors']]){
    extension=ad[['extension']]
    #cat("Anno ext: ",extension,ad$basePath,"\n")
    annoBasePath=NULL
    if(is.null(ad[['basePath']])){
      #annoBasePath=schema$
      # TODO use same as primary track
    }else{
      annoBasePathEmu=ad[['basePath']]
      # resolve wildcards
      annoFilename=str_c(uttCode,'.',extension)
      annoPath=find.file.in.emu.path.pattern(annoBasePathEmu,annoFilename,basePath)
      if(!is.null(annoPath)){
        #cat("Anno: ",annoPath,"\n")
        if(extension=='hlb'){
          #cat("Parse hlb file:",annoPath,"\n")
          
          hlbParseResult=parse.hlb.file(hlbFilePath=annoPath,levelDefinitions=schema[['levelDefinitions']],levels=levels);
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
        }
      }
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
  annotates=paste0('0000_ses/',uttCode,bundle.dir.suffix,'/',sampleTrackFile)
  bundle=create.bundle(name=uttCode,annotates=annotates,sampleRate=bundleSampleRate,levels=levels,signalpaths=signalpaths,mediaFilePath=sampleRateReferenceFile,links=links)
  return(bundle)
}

## Load legacy EMU database by name
## 
## @param dbName legacy EMU database name
## @param verboseLevel integer setting the verbosity level
## @param showProgress show progress bar
## @return object of class emuDB
## @author Klaus Jaensch
## @import stringr wrassp
## 
load.database.from.legacy.emu.by.name=function(dbName,verboseLevel=0,showProgress=TRUE){

    emuDbsList=list.legacy.emu.databases()
    emuTplPath=emuDbsList[[dbName]]
    if(is.null(emuTplPath)){
      stop("Legacy EMU database '",dbName,"' could not be found.")
    }
    return(load.database.from.legacy.emu(emuTplPath=emuTplPath,verboseLevel=verboseLevel,showProgress=showProgress))
  }
  

## Load legacy EMU database from EMU template (.tpl) file
## 
## @param emuTplPath EMU template file path
## @param verboseLevel integer setting the verbosity level
## @param showProgress show progress bar
## @return object of class emuDB
## @author Klaus Jaensch
## @import stringr wrassp
## 
load.database.from.legacy.emu=function(emuTplPath,verboseLevel=0,showProgress=TRUE){
  progress=0
  tplBaseDir=NULL
  if(file.exists(emuTplPath)){
    tplFileinfo=file.info(emuTplPath)
    if(tplFileinfo[['isdir']]){
      stop('Expected EMU template file, but ',emuTplPath,' is a directory!')
    }
    tplBaseDir=dirname(emuTplPath)
  }else{
    stop('Template file "',emuTplPath,'" does not exist!');
  }

  dbd=load.database.schema.from.emu.template(emuTplPath)
  if(verboseLevel>0){
    cat("Loaded database schema.\n")
  }
  progress=progress+1L
  db=create.database(name=dbd[['name']],basePath=tplBaseDir,DBconfig=dbd)
  db=initialize.database.dataframes(db)
  
  schema=db[['DBconfig']]
  # load primary track file list first
  # and find samples track to get sample rate
  primaryFileList = NULL
  
  primaryBasePath=NULL
  primaryFileExtension=NULL
  
  pattern='*'
  
  # find primary and sample track paths
  for(tr in schema[['tracks']]){
    #cat("Track: ",tr$name," ",tr$fileExtension,"\n")
    if(tr[['fileExtension']]==schema[['flags']][['PrimaryExtension']]){
      
      primaryFileExtension=tr[['fileExtension']]
      primaryBasePath=tr[['basePath']]
      if(!is.null(tr[['unitSuffix']])){
        pattern=str_c(pattern,tr[['unitSuffix']])
      }
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
  pattern=str_c(pattern,'.',primaryFileExtension)
  
  primaryFileList=list.file.matching.emu.path.pattern(db[['basePath']],primaryBasePath,filePattern=pattern)
  
  # TODO load other tracks
  
  # build utterance list
  # TODO lazy loading of annotations/ASSP objects
  utts=list()
  bundlesCount=length(primaryFileList)
  #utts=vector(mode='list',length=bundlesCount)
  us=1:bundlesCount
  if(showProgress){
    cat("INFO: Loading legacy EMU database containing",bundlesCount,"bundles...\n")
    pb=txtProgressBar(min=0,max=bundlesCount+7,initial=progress,style=3)
    
    setTxtProgressBar(pb,progress)
  }
  uttNames=c()

  db[['itemsIdx']]=0L
  db[['labelsIdx']]=0L
  db[['linksIdx']]=0L
  
  for(ui in us){
    ptrFilePath=primaryFileList[ui]
    #cat("Primary track file path: ",ptrFilePath,"\n")
    
    ptrFileBasename=basename(ptrFilePath)
    #cat("Ext: ",primaryTrackFileExtension,"\n")
    cutLen=str_length(primaryFileExtension)+1L
    cutPos=str_length(ptrFileBasename)-cutLen
    #cat("Cut: ",ptrFileBasename,cutLen,cutPos,"\n")
    uttCode=substr(ptrFileBasename,1,cutPos)
    bundle=load.annotation.for.legacy.bundle(schema,uttCode,db[['basePath']])
    
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
        
        #db[['items']][row,'bundle']=bName
        db[['items']][['bundle']][row]=bName
        itemId=it[['id']]
        if(is.null(itemId)){
          # for instance aetobi has no .hlb files and therefore no links and item ids
          id=paste(db[['name']],bName,sep='_')
          itemId=NA
        }else{
          id=paste(db[['name']],bName,it['id'],sep='_')
        }
        db[['items']][['id']][row]=id
        db[['items']][['bundleId']][row]=itemId
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
        db[['items']][['label']][row]=lbl0
        for(i in 1:maxLbls){
          rLbl=NA
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
              db[['labels']][['itemID']][lrow]=id
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
    
    utts[[uttCode]]=bundle
    if(verboseLevel>5){
      cat("Loaded bundle ",uttCode,"(",ui," of ",bundlesCount,")\n")
    }
    progress=progress+1L
    if(showProgress){
      setTxtProgressBar(pb,progress)
    }
  }
  itemsIdx=db[['itemsIdx']]
  
  db[['items']]=data.frame(id=db[['items']][['id']][1:itemsIdx],bundle=db[['items']][['bundle']][1:itemsIdx],level=db[['items']][['level']][1:itemsIdx],bundleId=db[['items']][['bundleId']][1:itemsIdx],type=db[['items']][['type']][1:itemsIdx],seqIdx=db[['items']][['seqIdx']][1:itemsIdx],sampleRate=db[['items']][['sampleRate']][1:itemsIdx],samplePoint=db[['items']][['samplePoint']][1:itemsIdx],sampleStart=db[['items']][['sampleStart']][1:itemsIdx],sampleDur=db[['items']][['sampleDur']][1:itemsIdx],label=db[['items']][['label']][1:itemsIdx],stringsAsFactors=FALSE)
  #tmpDf=data.frame(db[['items']],stringsAsFactors = FALSE)
  #db[['items']]=tmpDf[1:itemsIdx,]
  progress=progress+1L
  if(showProgress){
    setTxtProgressBar(pb,progress)
  }
  
  labelsIdx=db[['labelsIdx']]
  db[['labels']]=data.frame(itemID=db[['labels']][['itemID']][1:labelsIdx],bundle=db[['labels']][['bundle']][1:labelsIdx],labelIdx=db[['labels']][['labelIdx']][1:labelsIdx],name=db[['labels']][['name']][1:labelsIdx],label=db[['labels']][['label']][1:labelsIdx],stringsAsFactors=FALSE)
  #tmpDf=data.frame(db[['labels']],stringsAsFactors = FALSE)
  #db[['labels']]=tmpDf[1:labelsIdx,]
  #db[['links']]=data.frame(bundle=bundle_l[1:lli],fromID=fromID_l[1:lli],toID=toID_l[1:lli],label=label_l[1:lli])
  progress=progress+1L
  if(showProgress){
    setTxtProgressBar(pb,progress)
  }
  
  linksIdx=db[['linksIdx']]
  db[['links']]=data.frame(bundle=db[['links']][['bundle']][1:linksIdx],fromID=db[['links']][['fromID']][1:linksIdx],toID=db[['links']][['toID']][1:linksIdx],label=db[['links']][['label']][1:linksIdx],stringsAsFactors=FALSE)
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

  # Emu does not divide utterances in sessions
  # we create a dummy container session to satisfy new db data model
  # (db: list of sessions, session: list of bundles (utterances))
  containerSession=emuDB.session(name='0000',bundles=utts)
  #db$sessions[[1]]$bundles = utts
  db[['sessions']][['0000']]=containerSession
  db=remove.database.redundant.links(db)
  progress=progress+1L
  if(showProgress){
    setTxtProgressBar(pb,progress)
  }
  redLinks=build.redundant.links.all(db)
  progress=progress+1L
  if(showProgress){
    setTxtProgressBar(pb,progress)
  }
  
  db[['linksExt']]=calculate.postions.of.links(db[['items']],redLinks)
  progress=progress+1L
  if(showProgress){
    setTxtProgressBar(pb,progress)
  }
  
  if(verboseLevel>0){
    cat("Loaded database bundles.")
  }
  
  return(db)
}
  


# emuR.database.fromDirectory=function(baseDir,blockDirPattern=NULL,sessionDirPattern=NULL,primaryUnitSuffix=NULL,primaryFileExtension="wav",name=NULL){
#  TODO !!
#   return(database) 
#  
# } 

query.segments <- function(db){
  ll=list()
  sl=list()
  el=list()
  ul=list()
  idx=1
  for(s in db[['sessions']]){
    #cat("Session ",s$code,"\n")
    for(b in s[['bundles']]){
      #cat("bundle: ",b$name,"\n")
      for(t in b[['levels']]){
        #cat("level: ",t$name,"\n")
        sampleRate=t[['sampleRate']]
        if(t[['name']]=='Phonetic'){
        for(i in t[['items']]){
          #cat("Item ",i$label,"\n")
          idx=length(ul)+1L
          ll[[idx]]=i[['label']]
          if(inherits(i,'emuDB.annotation.model.PointItem')){
            #cat("Event: ",i$label,i$samplePoint,"\n")
            sl[[idx]]=(i[['samplePoint']]*1000)/sampleRate
            el[[idx]]=0
            
          }else if(inherits(i,'emuDB.annotation.model.IntervalItem')){
            #cat("Interval: ",i$label,i$sampleStart,i$sampleDur,"\n")
            sl[[idx]]=(i[['sampleStart']]*1000)/sampleRate
            el[[idx]]=((i[['sampleStart']]+i[['sampleDur']])*1000)/sampleRate
          }else{
            #cat("Item: ",i$label,"\n")
            sl[[idx]]=0
            el[[idx]]=0
          }
         # test for Raphaels extractSegs version
          #ul[[idx]]=paste0("file:///homes/klausj/WORK/EmuDbs/ae/signals/",b$name,'.wav');
          ul[[idx]]=b[['name']]
        }
        }
      }
    }
    
  }
  
  segl=make.seglist(ll,sl,el,ul,NULL,NULL,NULL)
  return(segl)
}


# get.bundle <- function(db,uttName){
#   for(s in db[['sessions']]){
#     for(u in s[['bundles']]){
#       if(u[['name']]==uttName){
#         return(u);
#       }
#     }
#   }
#   return(NULL);
# }

extractTrackdata <- function(db=NULL,segmentList=NULL,trackName=NULL){
  schema=db[['DBconfig']]
  signalExt=NULL
  for(tr in schema[['tracks']]){
    if(tr[['name']]==trackName){
      signalExt=tr[['fileExtension']]
    }
  }
  signalExtPatt=paste0('[.]',signalExt,'$')
  currentUtt=''
  currentAsspObj=NULL
  utts=segmentList[['utts']]
  
  index <- matrix(ncol=2, nrow=length(utts))
  colnames(index) <- c("start","end")
  
  ftime <- matrix(ncol=2, nrow=length(utts))
  colnames(ftime) <- c("start","end")
  
  data <- NULL
  origFreq <- NULL
  
  #########################
  # LOOP OVER UTTS
  curIndexStart = 1
  for (i in 1:length(utts)){
   
    un=segmentList[['utts']][[i]]
    if(currentUtt!=un){
      #cat("Utt: ",un,"\n")
      u=get.bundle(db,un)
      for(sp in u[['signalpaths']]){ 
        if(length(grep(signalExtPatt,sp))==1){
          #cat("Signal path: ",sp,"\n")
          currentAsspObj=read.AsspDataObj(sp)
        }
      }
    }
    # we should have the corresponding (complete) ASSP data obj for the segment here
    completeData=currentAsspObj[[trackName]] 
    ncols=ncol(completeData) 
    if(is.null(data)){
      data <- matrix(ncol=ncols, nrow=0)
    }
    
    sampleRate=attr(currentAsspObj,"sampleRate")
    #cat("Cols: ",ncols,"\n")
    origFreq <- attr(currentAsspObj, "origFreq")
    
    curStart <- segmentList[['start']][i]
    curEnd <- segmentList[['end']][i]
  
    fSampleRateInMS <- (1/sampleRate)*1000
    fStartTime <- attr(currentAsspObj,"startTime")*1000
    #cat("Seq: ",fStartTime, curEnd, fSampleRateInMS,"\n")
    timeStampSeq <- seq(fStartTime, curEnd, fSampleRateInMS)
    ###########################################
    # search for first item larger than start time
    breakVal <- -1
    for (j in 1:length(timeStampSeq)){
      if (timeStampSeq[j] >= curStart){
        breakVal <- j
        break
      }
    }
    curStartDataIdx <- breakVal
    curEndDataIdx <- length(timeStampSeq)
    
    ####################
    # set index and ftime
    curIndexEnd <- curIndexStart+curEndDataIdx-curStartDataIdx
    index[i,] <- c(curIndexStart, curIndexEnd)
    ftime[i,] <- c(timeStampSeq[curStartDataIdx], timeStampSeq[curEndDataIdx])
    
    #############################
    # calculate size of and create new data matrix
    #tmpData <- eval(parse(text=paste("curDObj$",colName,sep="")))
    
    
    rowSeq <- seq(timeStampSeq[curStartDataIdx],timeStampSeq[curEndDataIdx], fSampleRateInMS) 
    curData <- matrix(ncol=ncol(completeData), nrow=length(rowSeq))
    colnames(curData) <- paste("T", 1:ncol(curData), sep="")
    rownames(curData) <- rowSeq
    curData[,] <- completeData[curStartDataIdx:curEndDataIdx,] 
    
    ##############################
    # Append to global data matrix app
    data <- rbind(data, curData)
    
    curIndexStart <- curIndexEnd+1
    
    curDObj = NULL
  }
  ########################################
  #convert data, index, ftime to trackdata
  FileExtAndtrackname=paste0(signalExt,':',trackName)
  myTrackData <- as.trackdata(data, index=index, ftime, FileExtAndtrackname)
  
  if(any(trackName %in% c("dft", "css", "lps", "cep"))){
    if(!is.null(origFreq)){
      attr(myTrackData[['data']], "fs") <- seq(0, origFreq/2, length=ncol(myTrackData[['data']]))
      class(myTrackData[['data']]) <- c(class(myTrackData[['data']]), "spectral")
    }else{
      stop("no origFreq entry in spectral data file!")
    }
  }
  
  #if(!is.null(OnTheFlyFunctionName)){
  #  close(pb)
  #}
  
  return(myTrackData)
}

emuR.persist.filters=list()
emuR.persist.filters[['bundle']]=list()
emuR.persist.filters[['bundle']][[1]]=c('files')
emuR.persist.filters[['bundle']][[2]]=c('signalpaths')
emuR.persist.filters[['bundle']][[3]]=c('mediaFilePath')
# TODO sampleRate required !!
emuR.persist.filters[['bundle']][[4]]=c('levels','*','sampleRate')

emuR.persist.filters[['DBconfig']]=list()
emuR.persist.filters[['DBconfig']][[1]]=c('annotationDescriptors')
emuR.persist.filters[['DBconfig']][[2]]=c('tracks')
emuR.persist.filters[['DBconfig']][[3]]=c('flags')
emuR.persist.filters[['DBconfig']][[4]]=c('ssffTrackDefinitions','basePath')
emuR.persist.filters[['DBconfig']][[5]]=c('mediafileBasePathPattern')
emuR.persist.filters[['DBconfig']][[6]]=c('maxNumberOfLabels')
emuR.persist.filters[['DBconfig']][[7]]=c('itemColNames')
emuR.persist.filters[['DBconfig']][[8]]=c('basePath')
emuR.persist.filters[['DBconfig']][[9]]=c('DBconfigPath')

marshal.for.persistence <- function(x, filter=NULL){
  if (is.list(x)) {
    
    cloneList=list()
    class(cloneList) <- class(x)
    nms=names(x)
    if(is.null(nms)){
      
      # pass through
      len=length(x)
      if(len>0){
        for (i in 1:len) {
          cloneList[[i]]=marshal.for.persistence(x[[i]],filter=filter);
        }
      }
    }else{
      if(any(duplicated(nms))){
        stop("Cannot handle list with duplicate names.")
      }
      # TODO if the value is null the key ist not in names
      
      persistAttr=attr(x,'ips.persist')
      refNms=list()
      refs=NULL
      typesJSON=NULL
      typesJSONnms=list()
      if(!is.null(persistAttr)){
        refs=persistAttr[['refs']]
        if(!is.null(refs)){
          refNms=names(refs)
        }
        typesJSON=persistAttr[['typesJSON']]
        if(!is.null(typesJSON)){
          typesJSONnms=names(typesJSON)
          #cat(typesJSONnms,"\n")
        }
      }
      childFilter=NULL
      for(n in nms){
        
        dropProp=FALSE
        if(!is.null(filter)){
          for(ps in filter){
            p=ps[1]
            if(n==p | p=='*'){
              lenP=length(ps)
              if(lenP>1){
                childSeq=2:lenP
                if(is.null(childFilter)){
                  childFilter=list()
                }
                childFilter[[length(childFilter)+1L]]=ps[childSeq]
                #cat("Added child filter item: ",ps[childSeq],"\n")
              }else{
                
                
                if(!dropProp && n==p){
                  # cat("Dropping property: ",n,"\n")
                  dropProp=TRUE
                }
              }
            }
          }
        }
        if(!dropProp){
          v=x[[n]]
          if(n %in% refNms){
            idFn=refs[[n]]
            if(is.null(idFn)){
              stop("No ID property given") 
            }
            # replace referenced object with ID
            
            # get target ID
            vPersistAttr=attr(v,'ips.persist')
            if(is.null(vPersistAttr)){
              stop("No persistence info for object to convert to ID")
            }
            vLocIdProp=vPersistAttr[['localId']]
            if(is.null(vLocIdProp)){
              stop("No ID property found.")
            }
            vLocId=v[[vLocIdProp]]
            #cat("ID field: ",idFn,vLocIdProp,vLocId,"\n")
            cloneList[[idFn]]=vLocId
          }else if(n %in% typesJSONnms){
            typeJSON=typesJSON[[n]]
            if(!is.null(typeJSON)){
              if(typeJSON=='array'){
                val=marshal.for.persistence(x[[n]],filter=childFilter)
                # convert to array by deletion of names attribute
                attr(val,'names') <-NULL
                cloneList[[n]]=val
                #cat("Array conversion of field ",n,"\n")
              }
            }
            
          }else{
            cloneList[[n]]=marshal.for.persistence(x[[n]],filter=childFilter)
          }
        }
      }
    }
    
    return(cloneList)
  }else{
    return(x);
  }
}

set.list.names <-function(list,nameProperty){
  elemNames=c()
  for(le in list){
    name=le[[nameProperty]]
    elemNames=c(elemNames,name)
  }
  names(list)<-elemNames
  return(list)
  
}

unmarshal.from.persistence <- function(x){
  # TODO !!
  return(x);
}


##' Convert legacy EMU database and store in new format to directory
##' 
##' @param emuTplPath EMU template file path
##' @param targetDir target directory
##' @param verbose be verbose
##' @author Klaus Jaensch
##' @seealso \code{\link{load.emuDB}}
##' @export
##' @keywords emuDB database schema Emu
##' @examples
##' \dontrun{
##' ## Convert legacy EMU database specified by EMU template file /homes/mylogin/ae/ae.tpl to directory /homes/mylogin/EMUnew/ae
##'
##' convert.legacyEmuDB.to.emuDB("/homes/mylogin/ae/ae.tpl","/homes/mylogin/EMUnew/ae")
##'
##' }
##' 
convert.legacyEmuDB.to.emuDB <- function(emuTplPath,targetDir,verbose=TRUE){
  # load database schema and metadata
  # lazy currently ignored
  db=load.database.from.legacy.emu(emuTplPath,showProgress=verbose)
  if(verbose){
    cat("\n")
  }
  store.database(db,targetDir,showProgress=verbose)
  #activeButtons=list(saveBundle=TRUE)
}

##' Convert legacy EMU database and store in new format to directory
##' Loading by name only works if database was used with legacy EMU.
##' @param dbName legacy EMU database name
##' @param targetDir target directory
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
convert.legacyEmuDB.by.name.to.emuDB <- function(dbName,targetDir,verbose=TRUE){
  # load database schema and metadata
  db=load.database.from.legacy.emu.by.name(dbName,showProgress=verbose)
  if(verbose){
    cat("\n")
  }
  # save in new format
  store.database(db,targetDir,showProgress=verbose)
  #activeButtons=list(saveBundle=TRUE)
}

## Store EMU database bundle to file
## 
## @param db EMU database (in R workspace)
## @param bundle the bundle to save
## @import rjson
## @keywords emuDB database Emu 
## 
store.bundle.annotation <- function(db,bundle){
  dbDir=db[['basePath']]
  # check target dir
  if(!file.exists(dbDir)){
   stop(dbDir," does not exist!")
  }
  bName=bundle[['name']]
  #cat("Store bundle ",bName,"\n")
  for(s in db[['sessions']]){
    sBundleNms=names(s[['bundles']])
    #cat(bName," in ",sBundleNms,"\n")
    if(bName %in% sBundleNms){
      # session found
      #cat("Store bundle in session ",s[['name']],"\n")
      
      #bundleDf=convert.bundle.data.framed(db[['DBconfig']],bundle)
      
      # insert in loaded db
      s[['bundles']][[bName]]=bundle
      
      # persist to filesystem
      # TODO error handling
      sessDirNm=paste0(s[['name']],session.suffix)
      sessPth=file.path(dbDir,sessDirNm)
      
      bDirNm=paste0(bName,bundle.dir.suffix)
      bndlPth=file.path(sessPth,bDirNm)
      
      bndlFileNm=paste0(bName,bundle.annotation.suffix,'.json')
      bndFilePth=file.path(bndlPth,bndlFileNm)
      pFilter=emuR.persist.filters[['bundle']]
      bp=marshal.for.persistence(bundle,pFilter)
      bpJSON=rjson::toJSON(bp)
      pbpJSON=jsonlite::prettify(bpJSON)
      writeLines(pbpJSON,bndFilePth)
      db=move.bundle.levels.to.data.frame(db=db,bundle=bundle,replace=TRUE)

      db=move.bundle.links.to.data.frame(db=db,bundle=bundle,replace=TRUE)
    }
  }
  return(db)
}



##' Store EMU database to directory
##' 
##' @param db EMU database (in R workspace)
##' @param targetDir target directory
##' @param rewriteSSFFTracks if TRUE rewrite SSF tracks instead of file copy to get rid of big endian encoded SSFF files (SPARC)
##' @param showProgress show progress bar
##' @author Klaus Jaensch
##' @import stringr uuid rjson
##' @export
##' @keywords emuDB database Emu
##' @seealso  \code{\link{load.emuDB}}
##' @examples
##' \dontrun{
##' # Store database object ae to directory /homes/mylogin/EMUnew/
##' 
##' store.database(ae,"/homes/mylogin/EMUnew/")
##' 
##' }

store.database <- function(db,targetDir,rewriteSSFFTracks=TRUE,showProgress=TRUE){
  progress=0
  # check target dir
  if(file.exists(targetDir)){
    tdInfo=file.info(targetDir)
    if(!tdInfo[['isdir']]){
      stop(targetDir," exists and is not a directory.")
    }
  }else{
    # create target dir
    dir.create(targetDir)
  }
  # create database dir in targetdir
  pp=file.path(targetDir,db[['name']])
  # check existence
  if(file.exists(pp)){
    stop(pp," already exists.")
  }
 
  dir.create(pp)
  
  # set editable
  db[['DBconfig']][['EMUwebAppConfig']][['activeButtons']]=list(saveBundle=TRUE)

  # store db schema file
  dbCfgNm=paste0(db[['name']],database.schema.suffix)
  dbCfgPath=file.path(pp,dbCfgNm)
  
  persistFilter=emuR.persist.filters[['DBconfig']]
  sp=marshal.for.persistence(db[['DBconfig']],persistFilter)
  sJSON=rjson::toJSON(sp)
  psJSON=jsonlite::prettify(sJSON)
  writeLines(psJSON,dbCfgPath)
  progress=progress+1L
  
  bundleCount=0
  if(showProgress){
    # calculate bundle count
    for(s in db[['sessions']]){
        sessBundleCount=length(s[['bundles']])
        bundleCount=bundleCount+sessBundleCount
    }
    cat("INFO: Storing EMU database containing",bundleCount,"bundles...\n")
    pb=txtProgressBar(min=0,max=bundleCount+1L,style=3)
    setTxtProgressBar(pb,progress)
  }
  
  # convert sessions
  for(s in db[['sessions']]){
    #cat(targetDir,s$name,"\n")
    sDir=paste0(s[['name']],session.suffix)
    sfp=file.path(pp,sDir)
    #cat(targetDir,s$name,sfp,"\n")
    dir.create(sfp)
    # bundles
    bnms=names(s[['bundles']])
    for(bn in bnms){
      b=NULL
      bdf=s[['bundles']][[bn]]
      if(is.null(bdf)){
        bdf=list(name=bn)
      }
        #b=load.annotation.for.bundle(db[['DBconfig']],bn,db[['basePath']])
      #}else{
        #b=convert.bundle.s3(db[['DBconfig']],bdf)
        b=get.bundle.s3(db=db,bundle=bdf)
      #}
      bDir=paste0(b[['name']],bundle.dir.suffix)
      bfp=file.path(sfp,bDir)
      dir.create(bfp)
      pFilter=emuR.persist.filters[['bundle']]
      bp=marshal.for.persistence(b,pFilter)
  
      for(sf in b[['signalpaths']]){
        #cat("Signalpath: ",sf,"\n")
        bn=basename(sf)
        nsfp=file.path(bfp,bn)
        # check if SSFF type
        isSSFFFile=FALSE
        for(ssffTrDef in db[['DBconfig']][['ssffTrackDefinitions']]){
          ssffTrFileExt=ssffTrDef[['fileExtension']]
          fileExtPatt=paste0('[.]',ssffTrFileExt,'$')
          if(length(grep(fileExtPatt,sf))==1){
            isSSFFFile=TRUE
            break
          }
        }
        if(rewriteSSFFTracks && isSSFFFile){
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
      }
      
      # and metadata (annotations)
      ban=str_c(b[['name']],bundle.annotation.suffix,'.json')
      baJSONPath=file.path(bfp,ban)
      bpJSON=rjson::toJSON(bp)
      pbpJSON=jsonlite::prettify(bpJSON)   
      writeLines(pbpJSON,baJSONPath)
      
      progress=progress+1L
      if(showProgress){
        setTxtProgressBar(pb,progress)
      }
    }
     
  }
  
}

calculate.postions.of.links<-function(items,links){
  # for all position related functions we need to calculate the sequence indices of dominated items grouped to one dominance item 
  # Extend links table with sequence index of the targeted (dominated) item
  #links2=sqldf("SELECT k.*,i.seqIdx FROM links k,items i WHERE i.bundle=k.bundle AND k.toID=i.bundleId")
  links2=sqldf("SELECT k.*,i.seqIdx,i.level AS toLevel,i.type FROM links k,items i WHERE i.bundle=k.bundle AND k.toID=i.bundleId")
  # extend links table with relative sequence index
  links3=sqldf("SELECT k.*,k.seqIdx-(SELECT MIN(m.seqIdx) FROM links2 m WHERE m.fromID=k.fromID AND m.bundle=k.bundle AND k.toLevel=m.toLevel GROUP BY m.bundle,m.fromID,m.toLevel) AS toSeqIdx FROM links2 k")
  # Add length of dominance group sequence
  links4=sqldf("SELECT k.*,(SELECT MAX(m.seqIdx)-MIN(m.seqIdx)+1 FROM links3 m WHERE m.fromID=k.fromID AND m.bundle=k.bundle AND k.toLevel=m.toLevel GROUP BY m.bundle,m.fromID,m.toLevel) AS toSeqLen FROM links3 k")
  return(links4)
  #}
}

##' Load EMU database
##' 
##' @param databaseDir directory of the EMU database
##' @param verbose be verbose
##' @return object of class emuDB
##' @author Klaus Jaensch
##' @import rjson
##' @export
##' @keywords emuDB database schema Emu 
##' @examples
##' \dontrun{
##' ## Load database 'ae' in directory /homes/mylogin/EMUnew/ae
##' 
##' ae=load.emuDB("ae","/homes/mylogin/EMUnew/ae")
##' 
##' }
load.emuDB <- function(databaseDir,verbose=TRUE){
  progress=0
 
  db=list()
  class(db)<-'emuDB'
  # check database dir
  if(!file.exists(databaseDir)){
    stop("Database dir ",databaseDir," does not exist!")
  }
    dbDirInfo=file.info(databaseDir)
    if(!dbDirInfo[['isdir']]){
      stop(databaseDir," exists, but is not a directory.")
    }
 
 
  # load db schema file
  dbCfgPattern=paste0('.*',database.schema.suffix,'$')
  dbCfgFiles=list.files(path=databaseDir,dbCfgPattern)
  dbCfgFileCount=length(dbCfgFiles)
  if(dbCfgFileCount==0){
    stop("Could not find global DB config JSON file (regex pattern: ",dbCfgPattern,") in ",databaseDir)
  }
  if(dbCfgFileCount>1){
    stop("Found multiple global DB config JSON files (regex pattern: ",dbCfgPattern,") in ",databaseDir)
  }
  
  dbCfgPath=file.path(databaseDir,dbCfgFiles[[1]])
  if(!file.exists(dbCfgPath)){
    stop("Could not find database info file: ",dbCfgPath,"\n")
  }
  # with warn=TRUE and some files
  # R complains about incomplete (last) line
  # See https://stat.ethz.ch/pipermail/r-help/2006-July/108654.html
  dbCfgJSONLns=readLines(dbCfgPath,warn=FALSE)
  dbCfgJSON=paste(dbCfgJSONLns,collapse='')
  dbCfgPersisted=rjson::fromJSON(dbCfgJSON)
  # Quick and dirty workaround for rjson bug with one-element arrays:
  # assumes excatly one perspective!!
  orderCv=dbCfgPersisted[['EMUwebAppConfig']][['perspectives']][[1]][['levelCanvases']][['order']]
  orderL=list()
  for(orderC in orderCv){
    orderL[[length(orderL)+1L]]=orderC
    #cat("Order: ",orderC,"\n")
  }
  dbCfgPersisted[['EMUwebAppConfig']][['perspectives']][[1]][['levelCanvases']][['order']]=orderL
  
  schema=unmarshal.from.persistence(dbCfgPersisted)
  # get max label array size
  maxLbls=0
  for(lvlDef in schema[['levelDefinitions']]){
    attrCnt=length(lvlDef[['attributeDefinitions']])
    if(attrCnt > maxLbls){
      maxLbls=attrCnt
    }
  }
  schema[['maxNumberOfLabels']]=maxLbls
  db[['DBconfig']]=schema
  db[['name']]=schema[['name']]
  db[['basePath']]=databaseDir
  if(verbose){
    cat("INFO: Loading EMU database from ",databaseDir,"...\n")
  }
  db=initialize.database.dataframes(db)
  
  sessions=list()
  # sessions
  sessPattern=paste0('^[0-9]{4}',session.suffix,'$')
  sessDirs=dir(databaseDir,pattern=sessPattern)
 
  if(verbose){
    # calculate bundle count
    bundleCount=0
    for(sd in sessDirs){
      absSd=file.path(databaseDir,sd)
      bundleDirs=dir(absSd,pattern=paste0('.*',bundle.dir.suffix,'$'))
      bundleCount=bundleCount+length(bundleDirs)
    }
    pb=txtProgressBar(min=0,max=bundleCount+2,style=3)
    setTxtProgressBar(pb,progress)
  }
  
  for(sd in sessDirs){
   
    bundles=list()
    absSd=file.path(databaseDir,sd)
    bundleDirs=dir(absSd,pattern=paste0('.*',bundle.dir.suffix,'$'))
    # bundles
    for(bd in bundleDirs){
      #cat("Loading bundle ",bd,"\n")
      absBd=file.path(absSd,bd)
      #b=create.bundle(name=bd)
      # bundle files
      bName=gsub(paste0(bundle.dir.suffix,'$'),'',bd)
      # bundle files must start with bundle name
      bundleFilePattern=paste0('^',bName,'.*$')
      bfs=list.files(absBd,pattern=bundleFilePattern)
      
      signalpaths=list()
      bundle=NULL
      for(bf in bfs){
        annotFile=paste0(bName,bundle.annotation.suffix,'.json')
        absBf=file.path(absBd,bf)
        
        if(bf==annotFile){
          
          # and metadata (annotations)
          annoJSONLns=readLines(absBf,encoding="UTF-8")
          annoJSON=paste(annoJSONLns,collapse='')
          bundle=rjson::fromJSON(annoJSON)
          #class(bundle) <- 'emuDB.bundle'
          bundle=as.bundle(bundle)
          namedLevels=set.list.names(bundle[['levels']],'name')
          bundle[['levels']]=namedLevels
          bundle[['mediaFilePath']]=file.path(databaseDir,bundle[['annotates']])
        }else{
          
          for(ssffTr in schema[['ssffTrackDefinitions']]){
            ssffExt=ssffTr[['fileExtension']]
            ssffFn=paste0(bName,'.',ssffExt)
            
            if(ssffFn==bf){
              signalpaths[[length(signalpaths)+1L]]=absBf
            }
          }
          
        }
      }
      
      # add media file path to signalpaths
      sps=list(bundle[['mediaFilePath']])
      sps=c(sps,signalpaths)
      bundle[['signalpaths']]=sps
     
      #db=append.bundle.to.tmp.list(db,bundle)
      
      schema=db[['DBconfig']]
      maxLbls=db[['DBconfig']][['maxNumberOfLabels']]
      bName=bundle[['name']]
      for(lvl in bundle[['levels']]){
        
        seqIdx=0L
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
          }
          seqIdx=seqIdx+1L
          
          #db[['items']][row,'bundle']=bName
          db[['items']][['bundle']][row]=bName
          itemId=it[['id']]
          if(is.null(itemId)){
            # for instance aetobi has no .hlb files and therefore no links and item ids
            id=paste(db[['name']],bName,sep='_')
            itemId=NA
          }else{
            id=paste(db[['name']],bName,it['id'],sep='_')
          }
          db[['items']][['id']][row]=id
          db[['items']][['bundleId']][row]=as.integer(itemId)
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
            db[['items']][['samplePoint']][row]=as.integer(sp)
          }else{
            db[['items']][['samplePoint']][row]=NA
          }
          ss=it[['sampleStart']]
          if(!is.null(ss)){
            db[['items']][['sampleStart']][row]=as.integer(ss)
          }else{
            db[['items']][['sampleStart']][row]=NA
          }
          sdur=it[['sampleDur']]
          if(!is.null(sdur)){
            db[['items']][['sampleDur']][row]=as.integer(sdur)
          }else{
            db[['items']][['sampleDur']][row]=NA
          }
          
          lbls=it[['labels']]
          lblsLen=length(lbls)
          lbl0=it[['labels']][[1]][['value']]
          db[['items']][['label']][row]=lbl0
          for(i in 1:maxLbls){
            rLbl=NA
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
                }
                rLbl=lbl[['value']]
                db[['labels']][['itemID']][lrow]=id
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
        }
        db[['links']][['bundle']][row]=bName
        db[['links']][['fromID']][row]=as.integer(lk[['fromID']])
        db[['links']][['toID']][row]=as.integer(lk[['toID']])
        lbl=lk[['label']]
        if(is.null(lbl)){
          db[['links']][['label']][row]=NA
        }else{
          db[['links']][['label']][row]=lbl
        }
      }
      
      
      bundle[['levels']]=NULL
      bundle[['links']]=NULL
      bundles[[bName]]=bundle
      
      progress=progress+1L
      if(verbose){
        setTxtProgressBar(pb,progress)
      }
    }
    sessSuffixPattern=paste0(session.suffix,'$')
    sNm=gsub(sessSuffixPattern,'',sd)
    s=emuDB.session(name=sNm,path=absSd,bundles=bundles)
    sessions[[sNm]]=s
    
  }
  db[['sessions']]=sessions 
  
  itemsIdx=db[['itemsIdx']]
  tmpDf=data.frame(db[['items']],stringsAsFactors = FALSE)
  db[['items']]=tmpDf[1:itemsIdx,]
  progress=progress+1L
  if(verbose){
    setTxtProgressBar(pb,progress)
  }

  labelsIdx=db[['labelsIdx']]
  tmpLblsDf=data.frame(db[['labels']],stringsAsFactors = FALSE)
  db[['labels']]=tmpLblsDf[1:labelsIdx,]
  progress=progress+1L
  if(verbose){
    setTxtProgressBar(pb,progress)
  }

  linksIdx=db[['linksIdx']]
  tmpLksdf=data.frame(db[['links']],stringsAsFactors = FALSE)
  db[['links']]=tmpLksdf[1:linksIdx,]
  progress=progress+1L
  if(verbose){
    setTxtProgressBar(pb,progress)
  }

  # assume no redunant links in new format 
  linksForQuery=build.redundant.links.all(db)
  progress=progress+1L
  if(verbose){
    setTxtProgressBar(pb,progress)
  }
  db[['linksExt']]=calculate.postions.of.links(db[['items']],linksForQuery)
  progress=progress+1L
  if(verbose){
    setTxtProgressBar(pb,progress)
  }
  return(db)
  
}
