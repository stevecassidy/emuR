require(stringr)
require(uuid)
require(wrassp)
#require(data.table)


# constants

# API level of database object format
# increment this value if the internal database object format changes  
emuDB.apiLevel=2L

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
create.schema.attributeDefinition <- function(name, type='STRING',labelGroups=NULL){
  o <- list(name=name,type=type,labelGroups=labelGroups)
  class(o) <- c('emuDB.schema.attributeDefinition','list')
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

create.schema.databaseDefinition <- function(name,UUID=uuid::UUIDgenerate(),mediafileBasePathPattern=NULL,mediafileExtension=NULL,ssffTrackDefinitions=list(),levelDefinitions=list(),linkDefinitions=list(),EMUwebAppConfig=NULL,annotationDescriptors=NULL,tracks=NULL,flags=NULL){
  o <- list(name=name,UUID=UUID,mediafileBasePathPattern=mediafileBasePathPattern,mediafileExtension=mediafileExtension,ssffTrackDefinitions=ssffTrackDefinitions,levelDefinitions=levelDefinitions,linkDefinitions=linkDefinitions,EMUwebAppConfig=EMUwebAppConfig,annotationDescriptors=annotationDescriptors,tracks=tracks,flags=flags)
  class(o) <- c('list','emuDB.schema.databaseDefinition')
  #rTypes=list(levelDefinitions=c('list','emuDB.schema.levelDefinition',linkDefinitions=c('list','emuDB.schema.linkDefinition')
  #attr(o,'ips.persist')<-list(rTypes=rTypes)
  invisible(o)
}

summary.emuDB.schema.databaseDefinition<-function(schema,header=TRUE){
 
  cat("Level definitions:\n")
  for(ld in schema[['levelDefinitions']]){
    print(ld)
    cat("\tAttribute definitions:\n")
    for(ad in ld[['attributeDefinitions']]){
      cat("\t")
      print(ad)
    }
    cat("\n")
  }
  cat("\n")
  cat("Link definitions:\n")
  for(ld in schema[['linkDefinitions']]){
    print(ld)
  }
  
  
}

print.emuDB.schema.levelDefinition<-function(levelDefinition){
    cat(levelDefinition[['name']],"\ttype:\t",levelDefinition[['type']],"\n")
}

print.emuDB.schema.attributeDefinition<-function(attributeDefinition){
  cat(attributeDefinition[['name']],"\ttype:\t",attributeDefinition[['type']],"\n")
}

print.emuDB.schema.linkDefinition<-function(linkDefinition){
  cat(toString(linkDefinition),"\n")
}
toString.emuDB.schema.linkDefinition<-function(linkDefinition){
  paste(linkDefinition[['superlevelName']],'->',linkDefinition[['sublevelName']],linkDefinition[['type']],sep="\t")
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

create.database <- function(name,basePath=NULL,DBconfig=create.schema.databaseDefinition(name = name),sessions=NULL,primaryExtension=NULL){
  o <- list(name=name,basePath=basePath,DBconfig=DBconfig,sessions=sessions,primaryExtension=primaryExtension,apiLevel=emuDB.apiLevel)
  class(o) <- c('emuDB','list')
  invisible(o)
}


build.dataframe.table<-function(list,propNames){
  
}

##' Print summary for EMU database object
##' @description Gives an overview of an EMU database object
##' Prints database name, base directory path and informations about annoation levels, attributes, links, and signal file tracks
##' @param object EMU database object
##' @param ... (not used)
##' @method summary emuDB
##' @export
summary.emuDB<-function(object,...){
 
  cat("Name:\t",object[['name']],"\n")
  cat("Directory:\t",object[['basePath']],"\n")
  cat("Session count:",length(object[['sessions']]),"\n")
  bndlCnt=0
  for(s in object[['sessions']]){
    bndlCnt=bndlCnt+length(s[['bundles']])
  }
  cat("Bundle count:",bndlCnt,"\n")
  cat("Annotation item count: ",nrow(object[['items']]),", links count: ",nrow(object[['links']]),"\n")
  cat("\nDatabase configuration:\n\n")
  summary(object[['DBconfig']])
  #cat("SSFF track definitions:\n")
  # TODO 

  
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
# @param sessionName session ID of the bundle
# @param legacyBundleID legacy bundle ID
# @param annotates annotated signal file
# @param sampleRate sample rate
# @param signalpaths pathes of signal files
# @param mediaFilePath path pattern of samples track
# @param levels list of annotation levels
# @param links list of links containing the hierarchical information of the annotation levels
# @return object of class emuDB.bundle
# @author Klaus Jaensch
# @keywords emuDB bundle Emu
# 
create.bundle <- function(name,sessionName=NULL,legacyBundleID=NULL,annotates=NULL,sampleRate,signalpaths=list(),mediaFilePath=NULL,levels=list(),links=list()){
  o <- list(name=name,sessionName=sessionName,legacyBundleID=legacyBundleID,annotates=annotates,sampleRate=sampleRate,signalpaths=signalpaths,mediaFilePath=mediaFilePath,files=signalpaths,levels=levels,links=links)
  return(as.bundle(o))
}

as.bundle <- function(bundleData){
  class(bundleData) <- 'emuDB.bundle'
  attr(bundleData,'ips.persist')<-list(typesJSON=list(levels='array'))
  invisible(bundleData)
}




get.link.level.children<-function(schema,superlevelName){
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

get.level.name.by.attribute.name<-function(schema,attributeName){
  for(lvlD in schema[['levelDefinitions']]){
    aNames=character(0)
    for(ad in lvlD[['attributeDefinitions']]){
      aNames=c(aNames,ad[['name']])
      if(attributeName %in% aNames){
        return(lvlD[['name']])
      }
    }
  }
  return(NULL)
}

get.attribute.names.by.name<-function(schema,levelName){
  aNames=character(0)
  for(lvlD in schema[['levelDefinitions']]){
    if(lvlD[['name']]==levelName){
      for(ad in lvlD[['attributeDefinitions']]){
        aNames=c(aNames,ad[['name']])
      }
      break
    }
  }
  return(aNames)
}

get.all.attribute.names<-function(schema){
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

build.redundant.links.all<-function(database,sessionName=NULL,bundleName=NULL){
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
  return(build.redundant.links.for.pathes(database,lfs,sessionName,bundleName) )
  
}

build.redundant.links<-function(database,fromLevel,toLevel){
  # Legacy EMU and query functions link collections contain links for each possible connection between levels
  # We consider links that do not follow link definition constraints as redundant and therefore we remove them from the
  # link data model. For queries we build links for particular start and end level.
  #
  
  lfs=build.level.partial.pathes(database[['DBconfig']],fromLevel,toLevel)
 
  return(build.redundant.links.for.pathes(database,lfs) )
}
  

build.redundant.links.for.pathes<-function(database,lfs,sessionName='0000',bundleName=NULL){
  maxLfLen=0
  for(lf in lfs){
    lfLen=length(lf)
    if(lfLen>maxLfLen){
      maxLfLen=lfLen
    }
  }
  items=database[['items']]
  links=database[['links']]
  if(nrow(links)==0){
    return(links)
  }
  sqlQuery="SELECT DISTINCT f.session,f.bundle,f.itemID AS fromID,t.itemID AS toID FROM items f,items t"
  sqlQuery=paste0(sqlQuery,' WHERE f.bundle=t.bundle AND f.session=t.session AND ')
 
  if(!is.null(sessionName) & !is.null(bundleName)){
     # only for one bundle
    sqlQuery=paste0(sqlQuery,"f.session='",sessionName,"' AND f.bundle='",bundleName,"' AND ")
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
      sqlQuery=paste0(sqlQuery,"l1.bundle=f.bundle AND l1.bundle=t.bundle AND l1.session=f.session AND l1.session=t.session AND f.itemID=l1.fromID AND t.itemID=l1.toID")
      #sqlQuery=paste0(sqlQuery,"l1.bundle=f.bundle AND l1.bundle=t.bundle AND f.itemID=l1.fromID AND t.itemID=l1.toID")
      #cat(sLf,eLf,"\n")
    }else{
      # TODO start and end connection
      # from start to first in-between item 
      eLf=lf[2]
      #cat(sLf,eLf,"\n")
      sqlQuery=paste0(sqlQuery,"l1.bundle=f.bundle AND l1.bundle=i2.bundle AND l1.session=f.session AND l1.session=i2.session AND f.itemID=l1.fromID AND i2.itemID=l1.toID AND f.level='",sLf,"' AND i2.level='",eLf,"' AND ")
      #sqlQuery=paste0(sqlQuery,"l1.bundle=f.bundle AND l1.bundle=i2.bundle AND f.itemID=l1.fromID AND i2.itemID=l1.toID AND f.level='",sLf,"' AND i2.level='",eLf,"' AND ")
      if(lfLen>3){
        for(j in 2:(lfLen-2)){
          sLf=lf[j]
          eLf=lf[j+1L] 
          #cat(sLf,eLf,"\n")
          sqlQuery=paste0(sqlQuery,"l",j,".bundle=i",j,".bundle AND l",j,".bundle=i",(j+1),".bundle AND l",j,".session=i",j,".session AND l",j,".session=i",(j+1),".session AND i",j,".itemID=l",j,".fromID AND i",(j+1L),".itemID=l",j,".toID AND i",j,".level='",sLf,"' AND i",(j+1L),".level='",eLf,"' AND ")
        }
      }
      # from last in-between item to end item
      sLf=lf[(lfLen-1)]
      eLf=lf[lfLen]
      #cat(sLf,eLf,(lfLen-1),"\n")
      j=lfLen-1
      sqlQuery=paste0(sqlQuery,"l",j,".bundle=i",j,".bundle AND l",j,".bundle=t.bundle AND l",j,".session=i",j,".session AND l",j,".session=t.session AND i",j,".itemID=l",j,".fromID AND t.itemID=l",j,".toID AND i",j,".level='",sLf,"' AND t.level='",eLf,"'")
      #sqlQuery=paste0(sqlQuery,"l",j,".bundle=i",j,".bundle AND l",j,".bundle=t.bundle AND i",j,".itemID=l",j,".fromID AND t.itemID=l",j,".toID AND i",j,".level='",sLf,"' AND t.level='",eLf,"'")
    }
    sqlQuery=paste0(sqlQuery,"))")
    if(i<lfsLen){
      sqlQuery=paste0(sqlQuery," OR ")
    }
  }
  sqlQuery=paste0(sqlQuery,")")
  #cat(sqlQuery,"\n")
  # since version 2.8.x of sqlite the query is very slow without indices
  buildIndexItemsSql='CREATE INDEX items_idx ON items(session,bundle,level,itemID)'
  buildIndexLinksSql='CREATE INDEX links_idx ON links(session,bundle,fromID,toID)'
  comQuery=c(buildIndexItemsSql,buildIndexLinksSql,sqlQuery)
  nls=sqldf(comQuery)
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
  sessionName=bundle[['sessionName']]
  row=1 
  #bdf=data.table(matrix(ncol=length(db[['DBconfig']][['itemColNames']]),nrow=0))
  # caclculate bundle items length
  itCount=0
  for(lvl in bundle[['levels']]){
    lvlItCount=length(lvl[['items']])
    itCount=itCount+lvlItCount
  }
  bdf=data.frame(id=character(itCount),session=character(itCount),bundle=character(itCount),level=character(itCount),itemID=integer(itCount),type=character(itCount),seqIdx=integer(itCount),sampleRate=numeric(itCount),samplePoint=integer(itCount),sampleStart=integer(itCount),sampleDur=integer(itCount),label=character(itCount),stringsAsFactors=FALSE)
  #colnames(bdf)<-db[['DBconfig']][['itemColNames']]
  ldf=NULL
  lrow=1
  maxLbls=db[['DBconfig']][['maxNumberOfLabels']]
  #lblColNames=c('itemID','bundle','labelIdx','name','label')
  ldf=data.frame(itemID=character(0),session=character(0),bundle=character(0),labelIdx=integer(0),name=character(0),label=character(0),stringsAsFactors=FALSE)
  #colnames(ldf)<-lblColNames
  bName=bundle[['name']]
  for(lvl in bundle[['levels']]){
    
    seqIdx=0L
    for(it in lvl[['items']]){
      
      seqIdx=seqIdx+1L
     
      bdf[row,'session']=sessionName
      bdf[row,'bundle']=bName
      itemId=it[['id']]
      if(is.null(itemId)){
        # for instance aetobi has no .hlb files and therefore no links and item ids
        id=paste(db[['name']],sessionName,bName,sep='_')
        itemId=NA
      }else{
        id=paste(db[['name']],sessionName,bName,it['id'],sep='_')
      }
      bdf[row,'id']=id
      bdf[row,'itemID']=itemId
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
            ldf[lrow,'session']=sessionName
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
    otherBundlesSelector=(!(db[['items']][['session']]==sessionName & db[['items']][['bundle']]==bName))
    db[['items']]=db[['items']][otherBundlesSelector,]
    
    otherBundlesSelector=(!(db[['labels']][['session']]==sessionName & db[['labels']][['bundle']]==bName))
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
      dbWr[['db']][['items']][['itemID']][row]=itemId
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

get.bundle.levels.s3 <-function(db,sessionName,bundleName){
  
  levelDefinitions=db[['DBconfig']][['levelDefinitions']]
  find.levelDefinition<-function(name){
    for(lvlDef in levelDefinitions){
      if(name == lvlDef[['name']]){
        return(lvlDef)
      }
    }
  }
  
  # create all levels
  levels=list()
  for(ld in levelDefinitions){
    levels[[ld[['name']]]]=create.bundle.level(name=ld[['name']],type=ld[['type']])
  }
  
  bundleSelector=(db[['items']][['session']]==sessionName & db[['items']][['bundle']]==bundleName)
  items=db[['items']][bundleSelector,]
  bundleSelector=(db[['labels']][['session']]==sessionName & db[['labels']][['bundle']]==bundleName)
  bundleLabels=db[['labels']][bundleSelector,]
  
  nrows=nrow(items)
  cLvl=NULL
  lvlDef=NULL
  lvlItems=list()

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
        lvl=levels[[rLvl]]
        if(lvl[['type']]!=items[r,'type']){
          stop("Wrong item type ",items[r,'type']," for level ",rLvl," type ",lvl[['type']],"\n")
        }
        # create new level object 
        cLvl=create.bundle.level(name=rLvl,type=lvl[['type']],sampleRate=sr,items=lvlItems)
      }
      id=items[r,'itemID']
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
  bdf=data.frame(session=character(0),bundle=character(0),fromID=integer(0),toID=integer(0),label=character(0),stringsAsFactors=FALSE)
  sessionName=bundle[['sessionName']]
  bName=bundle[['name']]
  row=0
  for(lk in bundle[['links']]){
    row=row+1L 
    bdf[row,'session']=sessionName
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
    otherBundlesSelector=(!(db[['links']][['session']]==sessionName & db[['links']][['bundle']]==bName))
    db[['links']]=db[['links']][otherBundlesSelector,]
    otherBundlesSelector=(!(db[['linksExt']][['session']]==sessionName & db[['linksExt']][['bundle']]==bName))
    db[['linksExt']]=db[['linksExt']][otherBundlesSelector,]
  }

  db[['links']]=rbind(db[['links']],bdf)
  redLinksBundle=build.redundant.links.all(database = db,sessionName=sessionName,bundleName=bName)
  #TODO  put level and links method together and use only items of the bundle
  redExtLinksBundle=calculate.postions.of.links(db[['items']],redLinksBundle)
  db[['linksExt']]=rbind(db[['linksExt']],redExtLinksBundle)
  
  return(db)
}




get.bundle.links.s3 <-function(db,sessionName,bundleName){
  bundleSelector=(db[['links']][['session']] ==sessionName & db[['links']][['bundle']]==bundleName)
  linksDf=db[['links']][bundleSelector,]
  nrows=nrow(linksDf)
  
  #links=vector(mode='list',length=nrows)
  links=list()
  if(nrows>0){
    for(row in 1:nrows){
      link=list()
      link[['fromID']]=linksDf[row,'fromID']
      
      link[['toID']]=linksDf[row,'toID']
      lbl=linksDf[row,'label']
      if(!is.null(lbl) && !is.na(lbl)){
        link[['label']]=lbl
      }
      links[[row]]=link
     
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
## @param sessionName sessionName
## @param bundleName name of bundle
## @return bundle in S3 format
## @author Klaus Jaensch
## @keywords emuDB database schema Emu bundle
## 
get.bundle <- function(db,sessionName,bundleName){
  
  schema=db[['DBconfig']]
  #bundle=get.bundle.stub(db,sessionName,bundleName)
  bundle=db[['sessions']][[sessionName]][['bundles']][[bundleName]]
  if(!is.null(bundle)){
    bundle=get.bundle.s3(db,bundle)
  }
  return(bundle)
}

# get.bundle.stub<-function(db,bundleName){
#   sessCount=length(db[['sessions']])
#   bundleStub=NULL
#   for(s in 1:sessCount){
#     sbNms=names(db[['sessions']][[s]][['bundles']])
#     if(bundleName %in% sbNms){
#       # this session contains requested bundle
#       bundleStub=db[['sessions']][[s]][['bundles']][[bundleName]]
#       break
#     }
#   }
#   return(bundleStub)
# }

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
  sName=bundle[['sessionName']]
  # convert levels to s3
  bundle[['levels']]=get.bundle.levels.s3(db,sName,bName)
  bundle[['itemsDataFrame']]=NULL
  # convert links
  bundle[['links']]=get.bundle.links.s3(db,sName,bName)
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

emuDB.session <- function(name,path=NULL,bundles=list){
  o <- list(name=name,path=path,bundles=bundles)
  class(o) <- 'emuDB.session'
  invisible(o)
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


initialize.database.dataframes<-function(db){
  
  baseColNms <- c('id','bundle','level','itemID','type','seqIdx','sampleRate','samplePoint','sampleStart','sampleDur','label')
  maxLbls=db[['DBconfig']][['maxNumberOfLabels']]
  
  colNms=baseColNms
  db[['DBconfig']][['itemColNames']]=colNms
  


  # items 
  db[['items']]=list(id=character(vector.increment),session=character(vector.increment),bundle=character(vector.increment),level=character(vector.increment),itemID=integer(vector.increment),type=character(vector.increment),seqIdx=integer(vector.increment),sampleRate=numeric(vector.increment),samplePoint=integer(vector.increment),sampleStart=integer(vector.increment),sampleDur=integer(vector.increment),label=character(vector.increment))
  #db[['items']]=matrix(nrow=itCount,ncol=length(colNms))w
  #colnames(db[['items']])=colNms
  db[['itemsIdx']]=0L
  # label table
  #lblColNms=c('itemID','bundle','labelIdx','name','label')
  #db[['labels']]=data.frame(matrix(ncol=length(lblColNms),nrow=0),stringsAsFactors=FALSE)
  db[['labels']]=list(itemID=character(vector.increment),session=character(vector.increment),bundle=character(vector.increment),labelIdx=integer(vector.increment),name=character(vector.increment),label=character(vector.increment))
  #db[['labels']]=matrix(nrow=vector.increment,ncol=length(lblColNms))
  #colnames(db[['labels']])=lblColNms
  db[['labelsIdx']]=0L
 
  #linkColNms=c('bundle','fromID','toID','label')
  db[['links']]=list(session=character(vector.increment),bundle=character(vector.increment),fromID=integer(vector.increment),toID=integer(vector.increment),label=character(vector.increment))
  #db[['links']]=matrix(nrow=itCount,ncol=length(linkColNms))
 #colnames(db[['links']])=linkColNms
  db[['linksIdx']]=0L

  #colnames(db[['labels']])<-lblColNms
  return(db)
}



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
emuR.persist.filters[['bundle']][[4]]=c('legacyBundleID')
emuR.persist.filters[['bundle']][[5]]=c('sessionName')

# TODO sampleRate required !!
emuR.persist.filters[['bundle']][[6]]=c('levels','*','sampleRate')

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

emuR.persist.class=list()
emuR.persist.class[['DBconfig']]=list()
emuR.persist.class[['DBconfig']][['emuDB.schema.databaseDefinition']]=character(0)
emuR.persist.class[['DBconfig']][['emuDB.schema.levelDefinition']]=list(c('levelDefinitions','*'))
emuR.persist.class[['DBconfig']][['emuDB.schema.linkDefinition']]=list(c('linkDefinitions','*'))
emuR.persist.class[['DBconfig']][['emuDB.schema.attributeDefinition']]=list(c('levelDefinitions','*','attributeDefinitions','*'))

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
    #if(is.character(x) & length(x)==1){
    #  # Or use jsonlite::toJSON(...,auto_unbox=TRUE)
    #  return(jsonlite::unbox(x))
    #}else{
      return(x)
    #}
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

apply.class<-function(val,path,class){
  if(is.null(val)){
    return(NULL)
  }
  if(is.null(path)){
    class(val)<-c(class,class(val))
  }else{
    pLen=length(path)
    if(pLen==0){
      class(val)<-c(class,class(val))
    }else{
      
      pathElem=path[1]
      restpath=c()
      
      if(pLen>1){
        restpath=path[2:pLen]
      }
      if(pathElem=='*'){
        newVal=list()
        for(ch in val){
          newVal[[length(newVal)+1]]=apply.class(ch,restpath,class)
        }
        val=newVal
      }else{
        val[[pathElem]]=apply.class(val[[pathElem]],restpath,class)
      }
    }
  }
  return(val)
}

unmarshal.from.persistence <- function(x,classMap=list()){
  classNames=names(classMap)
  for(cn in classNames){
    pathes=classMap[[cn]]
    pathesLen=length(pathes)
    if(pathesLen==0){
      x=apply.class(x,c(),cn)
    }else{
      for(path in pathes){
        x=apply.class(x,path,cn)
      }
    }
  }  
  
  return(x);
}

.update.transient.schema.values<-function(schema){
  # get max label array size
#   maxLbls=0
#   for(lvlDef in schema[['levelDefinitions']]){
#     attrCnt=length(lvlDef[['attributeDefinitions']])
#     if(attrCnt > maxLbls){
#       maxLbls=attrCnt
#     }
#   }
  schema[['maxNumberOfLabels']]=max(sapply(schema[['levelDefinitions']], function(ld) { length(ld[['attributeDefinitions']])} ))
  return(schema)
}

## Store EMU database bundle to file
## 
## @param db EMU database (in R workspace)
## @param bundle the bundle to save
## @import jsonlite
## @keywords emuDB database Emu 
## 
store.bundle.annotation <- function(db,bundle){
  dbDir=db[['basePath']]
  # check target dir
  if(!file.exists(dbDir)){
   stop(dbDir," does not exist!")
  }
  sessionName=bundle[['sessionName']]
  bName=bundle[['name']]
  #cat("Store bundle ",bName,"\n")
  #for(s in db[['sessions']]){
  s=db[['sessions']][[sessionName]]
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
      pbpJSON=jsonlite::toJSON(bp,auto_unbox=TRUE,force=TRUE,pretty=TRUE)
      writeLines(pbpJSON,bndFilePth)
      db=move.bundle.levels.to.data.frame(db=db,bundle=bundle,replace=TRUE)

      db=move.bundle.links.to.data.frame(db=db,bundle=bundle,replace=TRUE)
    }
  #}
  return(db)
}

bundle.iterator<-function(db,apply){
  for(s in db[['sessions']]){
    sessionName=s[['name']]
    for(b in s[['bundles']]){
      db=apply(db,b)
    }
  }
  return(db)
}

create.emuDB<-function(name,targetDir,mediaFileExtension='wav'){
  path=file.path(targetDir,name)
  dbConfig=create.schema.databaseDefinition(name=name,mediafileExtension = mediaFileExtension)
  db=create.database(name=name,basePath=path,DBconfig = dbConfig)
  store.emuDB(db,targetDir)
  db=load.emuDB(path)
  return(db)
}

add.bundle<-function(db,sessionName,bundle){
  db[['sessionName']]
}


add.levelDefinition<-function(db,levelDefinition){
  # check if level definition (name) already exists 
  for(ld in db[['DBconfig']][['levelDefinitions']]){
    if(ld[['name']]==levelDefinition[['name']]){
      stop("Level definition:",levelDefinition[['name']]," already exists in database ",db[['name']])
    }
  }
  # add
  db[['DBconfig']][['levelDefinitions']][[length(db[['DBconfig']][['levelDefinitions']])+1]]=levelDefinition

  # update transient values
  db[['DBconfig']]=.update.transient.schema.values(db[['DBconfig']])
  
  # store to disk
  .store.schema(db)
  
  # add levels to exsiting bundles
  # not required!!
  
  #db=bundle.iterator(db,function(db,b){
  #  bs3=get.bundle(sessionName,b[['name']])
  #  bs3[['levels']][[levelDefinition[['name']]]]=create.bundle.level(name=levelDefinition[['name']],type = levelDefinition[['type']],items = list())
  #})
  
  return(db)
}

remove.levelDefinition<-function(db,levelDefinitionName){
  # check if level definition (name)  exists 
  if(!any(sapply(db[['DBconfig']][['levelDefinitions']],function(ld) ld[['name']]==levelDefinitionName))){
    stop("Level definition:",levelDefinitionName," does not exist in database ",db[['name']])
  }
  # check if level is referenced by link defintion
  for(lkd in db[['DBconfig']][['linkDefinitions']]){
    if(lkd[['superlevelName']]==levelDefinitionName |  lkd[['sublevelName']]==levelDefinitionName){
      lkdStr=toString(lkd)
      stop("Cannot remove level definition ",levelDefinitionName,". It is referenced by link definition: ",lkdStr)
    }
  }
  
  
  # check if level is empty
  itsSelector=(db[['items']][['level']]==levelDefinitionName)
  lvlIts=db[['items']][itsSelector,]
  if(nrow(lvlIts)>0){
    stop("Level is not empty. Remove items first to delete level ",levelDefinitionName)
  }
  
  # do removal
  newLvlDefs=list()
  for(lvlDef in db[['DBconfig']][['levelDefinitions']]){
    if(lvlDef[['name']]!=levelDefinitionName){
      newLvlDefs[[length(newLvlDefs)+1]]=lvlDef
    }
  }
  db[['DBconfig']][['levelDefinitions']]=newLvlDefs
  
  # update transient values
  db[['DBconfig']]=.update.transient.schema.values(db[['DBconfig']])
  
  # store to disk
  .store.schema(db)
  
  return(db)
}

add.linkDefinition<-function(db,linkDefinition){
  # check existence of levels
  
  superFound=FALSE
  for(ld in db[['DBconfig']][['levelDefinitions']]){
    if(ld[['name']]==linkDefinition[['superlevelName']]){
      superFound=TRUE
    }
  }
  if(!superFound){
    stop("Super level ",linkDefinition[['superlevelName']]," not found!")
  }
  
  subFound=FALSE
  for(ld in db[['DBconfig']][['levelDefinitions']]){
    if(ld[['name']]==linkDefinition[['sublevelName']]){
      subFound=TRUE
    }
   
  }
  if(!subFound){
    stop("Sub level ",linkDefinition[['sublevelName']]," not found!")
  }
  
  # check if link definition already exists 
  for(lkd in db[['DBconfig']][['linkDefinitions']]){
    if(lkd[['superlevelName']]==linkDefinition[['superlevelName']] & lkd[['sublevelName']]==linkDefinition[['sublevelName']]){
      stop("Link definition:",lkd," already exists in database ",db[['name']])
    }
  }
  
  
  
  # add
  db[['DBconfig']][['linkDefinitions']][[length(db[['DBconfig']][['linkDefinitions']])+1]]=linkDefinition
  
  # store to disk
  .store.schema(db)
  return(db)
}


##' Import media files to EMU database
##' @description Import media files to EMU database
##' @param db object of class emuDB
##' @param dir directory containing mediafiles or session directories
##' @param targetSessionName name of session in which to create the new bundles 
##' @return modified database object
##' @author Klaus Jaensch
##' @import sqldf stringr
##' @keywords emuDB database Emu
##' @examples
##' \dontrun{
##' ## Add mediafiles from directory
##' 
##'  import.mediaFiles(emuDB,'0000',dir="/data/mymedia")
##' 
##' }
"import.mediaFiles"<-function(db,dir,targetSessionName='0000'){
  UseMethod("import.mediaFiles")
}

"import.mediaFiles.emuDB"<-function(db,dir,targetSessionName='0000'){
  dbClass=class(db)
  if(dbClass=='emuDB'){

    dbCfg=db[['DBconfig']]
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
      if(is.null(db[['sessions']][[targetSessionName]])){
        db[['sessions']][[targetSessionName]]=emuDB.session(name=targetSessionName,path = sessDir,bundles=list())
      }
    }
    mediaAdded=FALSE
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
      db[['sessions']][[targetSessionName]][['bundles']][[bundleName]]=b
      # TODO generate empty annotation and SSFFtracks if required
      annoAdded=FALSE
      for(ld in dbCfg[['levelDefinitions']]){
       
        b[['levels']]=list()
        b[['levels']][[ld[['name']]]]=create.bundle.level(name=ld[['name']],type = ld[['type']],items = list())
        
        ## TODO TEST only
        #labelAttrs=list(list(name=ld[['name']],value='Test Huhu!'))
        #b[['levels']][[ld[['name']]]][['items']][[1]]=create.interval.item(id = 1,sampleStart = 0,sampleDur = 50000,labels = labelAttrs )
        #db=move.bundle.levels.to.data.frame(db,b)
        annoAdded=TRUE
      }
      if(annoAdded){
        db=store.bundle.annotation(db,b)
      }
      mediaAdded=TRUE
    }
    
    perspectives=dbCfg[['EMUwebAppConfig']][['perspectives']]
    # create an EMUwebapp default perspective if media has been added 
    if(mediaAdded & (is.null(perspectives) | length(perspectives)==0)){
      sc=create.EMUwebAppConfig.signalCanvas(order=c("OSCI","SPEC"),assign=list(),contourLims=list())
      defPersp=create.EMUwebAppConfig.perspective(name='default',signalCanvases=sc,levelCanvases=list(order=list()),twoDimCanvases=list(order=list()))
      db[['DBconfig']][['EMUwebAppConfig']][['perspectives']]=list(defPersp)
    }
  
    return(db)
  }else{
    NextMethod()
  }
}


.store.schema<-function(db,projectDir=NULL){
  if(is.null(projectDir)){
    projectDir=db[['basePath']]
  }
  # store db schema file
  dbCfgNm=paste0(db[['name']],database.schema.suffix)
  dbCfgPath=file.path(projectDir,dbCfgNm)
  
  persistFilter=emuR.persist.filters[['DBconfig']]
  sp=marshal.for.persistence(db[['DBconfig']],persistFilter)
  psJSON=jsonlite::toJSON(sp,auto_unbox=TRUE,force=TRUE,pretty=TRUE)
  writeLines(psJSON,dbCfgPath)
}


##' Store EMU database to directory
##' 
##' @details 
##' options is a list of key value pairs:
##' rewriteSSFFTracks if TRUE rewrite SSF tracks instead of file copy to get rid of big endian encoded SSFF files (SPARC), default: FALSE
##' ignoreMissingSSFFTrackFiles if TRUE missing SSFF track files are ignored, default: FALSE
##' symbolicLinkSignalFiles if TRUE signal files are symbolic linked instead of copied. Implies: rewriteSSFFTracks=FALSE, Default: FALSE
##' 
##' @param db EMU database (in R workspace)
##' @param targetDir target directory
##' @param options list of options
##' @param showProgress show progress bar
##' @author Klaus Jaensch
##' @import stringr uuid jsonlite
##' @export
##' @keywords emuDB database Emu
##' @seealso  \code{\link{load.emuDB}}
##' @examples
##' \dontrun{
##' # Store database object ae to directory /homes/mylogin/EMUnew/
##' 
##' store.emuDB(ae,"/homes/mylogin/EMUnew/")
##' 
##' }

store.emuDB <- function(db,targetDir,options=NULL,showProgress=TRUE){
  dbApiLevel=db[['apiLevel']]
  if(is.null(dbApiLevel)){
    stop("Database API level differs from R package API level: ",apiLevel,"\nPlease reload the database: db=reload(db)")
  }else if(dbApiLevel!=emuDB.apiLevel){
    stop("Database API level: ",dbApiLevel," differs from R package API level: ",apiLevel,"\nPlease reload the database: db=reload(db)")
  }
  
  # default options
  # ignore missing SSFF track files
  # rewrite SSFF track files
  mergedOptions=list(ignoreMissingSSFFTrackFiles=TRUE,rewriteSSFFTracks=FALSE,symbolicLinkSignalFiles=FALSE)
  if(!is.null(options)){
    for(opt in names(options)){
      mergedOptions[[opt]]=options[[opt]]
    }
  }
  
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
  .store.schema(db,projectDir=pp)
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
      
      # and metadata (annotations)
      ban=str_c(b[['name']],bundle.annotation.suffix,'.json')
      baJSONPath=file.path(bfp,ban)
      pbpJSON=jsonlite::toJSON(bp,auto_unbox=TRUE,force=TRUE,pretty=TRUE)
      writeLines(pbpJSON,baJSONPath)
      
      progress=progress+1L
      if(showProgress){
        setTxtProgressBar(pb,progress)
      }
    }
     
  }
  if(showProgress){
    setTxtProgressBar(pb,progress)
    cat("\n")
  }
  
}

calculate.postions.of.links<-function(items,links){
  if(nrow(links)==0){
    # return empty links data frame
    return(links)
  }
  # for all position related functions we need to calculate the sequence indices of dominated items grouped to one dominance item 
  # Extend links table with sequence index of the targeted (dominated) item
  #links2=sqldf("SELECT k.*,i.seqIdx FROM links k,items i WHERE i.bundle=k.bundle AND k.toID=i.itemID")
  
  # since version 2.8.x of sqlite the query is very slow without indices
  itemsIdxSql='CREATE INDEX items_idx ON items(session,bundle,level,itemID,seqIdx)'
  linksIdxSql='CREATE INDEX links_idx ON links(session,bundle,fromID,toID)'
 
  links2=sqldf(c(itemsIdxSql,linksIdxSql,"SELECT k.*,i.seqIdx,i.level AS toLevel,i.type FROM links k,items i WHERE i.session=k.session AND i.bundle=k.bundle AND k.toID=i.itemID"))
  # extend links table with relative sequence index
  links2IdxSql='CREATE INDEX links2_idx ON links2(session,bundle,fromID,toID,toLevel,type)'
  links3=sqldf(c(itemsIdxSql,links2IdxSql,"SELECT k.*,k.seqIdx-(SELECT MIN(m.seqIdx) FROM links2 m WHERE m.fromID=k.fromID AND m.session=k.session AND m.bundle=k.bundle AND k.toLevel=m.toLevel GROUP BY m.session,m.bundle,m.fromID,m.toLevel) AS toSeqIdx FROM links2 k"))
  # Add length of dominance group sequence
  links3IdxSql='CREATE INDEX links3_idx ON links3(session,bundle,fromID,toID,toLevel,type)'
  links4=sqldf(c(itemsIdxSql,links3IdxSql,"SELECT k.*,(SELECT MAX(m.seqIdx)-MIN(m.seqIdx)+1 FROM links3 m WHERE m.fromID=k.fromID AND m.session=k.session AND m.bundle=k.bundle AND k.toLevel=m.toLevel GROUP BY m.session,m.bundle,m.fromID,m.toLevel) AS toSeqLen FROM links3 k"))
  return(links4)
  #}
}

##' Load EMU database
##' 
##' @param databaseDir directory of the EMU database
##' @param verbose be verbose
##' @return object of class emuDB
##' @author Klaus Jaensch
##' @import jsonlite
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
  # TODO does problem with jsonlite still exist ?
  dbCfgJSONLns=readLines(dbCfgPath,warn=FALSE)
  dbCfgJSON=paste(dbCfgJSONLns,collapse='')
  dbCfgPersisted=jsonlite::fromJSON(dbCfgJSON,simplifyVector=FALSE)
  
  # unmarshal schema object (set class names)
  schema=unmarshal.from.persistence(dbCfgPersisted,emuR.persist.class[['DBconfig']])
  # set transient values
  schema=.update.transient.schema.values(schema)
  # create db object
  db=create.database(name = schema[['name']],basePath = normalizePath(databaseDir),DBconfig = schema)
  
  if(verbose){
    cat("INFO: Loading EMU database from ",databaseDir,"...\n")
  }
  db=initialize.database.dataframes(db)
  
  sessions=list()
  # sessions
  #sessPattern=paste0('^[0-9]{4}',session.suffix,'$')
  # if legacy EMU uses globpattern in path directive session name can be an arbitrary string
  sessPattern=paste0('^.*',session.suffix,'$')
  sessDirs=dir(databaseDir,pattern=sessPattern)
 
  # calculate bundle count
  bundleCount=0
  for(sd in sessDirs){
    absSd=file.path(databaseDir,sd)
    bundleDirs=dir(absSd,pattern=paste0('.*',bundle.dir.suffix,'$'))
    bundleCount=bundleCount+length(bundleDirs)
  }
  
  # progress distribution  
  
  # progress part to build a data frame 1%
  ppBuildDataFrame=as.integer(bundleCount/1L)
  # thress data frames items, labels and links
  ppBuildDataFrames=ppBuildDataFrame*3L
  # progress part to build redundant links 10%
  ppBuildRedLinks=as.integer(bundleCount/10L)
  # progress part to calaculate ext links 10%
  ppBuildExtLinks=as.integer(bundleCount/10L)
  pMax=bundleCount+ppBuildDataFrames+ppBuildRedLinks+ppBuildExtLinks
  if(pMax==0){
    pMax=1
  }
  if(verbose){ 
    pb=txtProgressBar(min=0L,max=pMax,style=3)
    setTxtProgressBar(pb,progress)
  }
  bundleNames=character(bundleCount)
  db[['bundleNamesUnique']]=TRUE
  bundleIdx=0L
  for(sd in sessDirs){
    sessionName=gsub(pattern = paste0(session.suffix,'$'),replacement = '',x = sd)
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
          bundle=jsonlite::fromJSON(annoJSON,simplifyVector=FALSE)
          #class(bundle) <- 'emuDB.bundle'
          bundle=as.bundle(bundle)
          namedLevels=set.list.names(bundle[['levels']],'name')
          bundle[['levels']]=namedLevels
          bundle[['mediaFilePath']]=file.path(absBd,bundle[['annotates']])
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
      # set session name
      bundle[['sessionName']]=sessionName
      
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
          db[['items']][['session']][row]=sessionName
          itemId=it[['id']]
          if(is.null(itemId)){
            # for instance aetobi has no .hlb files and therefore no links and item ids
            id=paste(db[['name']],sessionName,bName,sep='_')
            itemId=NA
          }else{
            id=paste(db[['name']],sessionName,bName,it['id'],sep='_')
          }
          db[['items']][['id']][row]=id
          db[['items']][['itemID']][row]=as.integer(itemId)
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
        }
        db[['links']][['session']][row]=sessionName
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
      
      bundleIdx=bundleIdx+1
      if(db[['bundleNamesUnique']]){
        if(bName %in% bundleNames){
          db[['bundleNamesUnique']]=FALSE
        }else{
          bundleNames[bundleIdx]=bName
        }
      }
      
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
  db[['items']]=tmpDf[0:itemsIdx,]
  progress=progress+ppBuildDataFrame
  if(verbose){
    setTxtProgressBar(pb,progress)
  }

  labelsIdx=db[['labelsIdx']]
  tmpLblsDf=data.frame(db[['labels']],stringsAsFactors = FALSE)
  db[['labels']]=tmpLblsDf[0:labelsIdx,]
  progress=progress+ppBuildDataFrame
  if(verbose){
    setTxtProgressBar(pb,progress)
  }

  linksIdx=db[['linksIdx']]
  tmpLksdf=data.frame(db[['links']],stringsAsFactors = FALSE)
  
  db[['links']]=tmpLksdf[0:linksIdx,]
  progress=progress+ppBuildDataFrame
  if(verbose){
    setTxtProgressBar(pb,progress)
  }

  # assume no redunant links in new format 
  linksForQuery=build.redundant.links.all(db)
  progress=progress+ppBuildRedLinks
  if(verbose){
    setTxtProgressBar(pb,progress)
  }
  db[['linksExt']]=calculate.postions.of.links(db[['items']],linksForQuery)
  progress=progress+ppBuildExtLinks
  if(verbose){
    setTxtProgressBar(pb,progress)
    
    cat("\n")
  }
  return(db)
  
}


##' Reload EMU database
##' @description Reload an EMU database from disk storage
##' @param db object of class emuDB

##' @return refreshed database object
##' @author Klaus Jaensch
##' @import sqldf stringr
##' @seealso \code{\link{load.emuDB}}
##' @keywords emuDB database query Emu
##' @examples
##' \dontrun{
##' ## Reload database object ae
##' 
##' ae=reload(ae)
##' 
##' }
"reload"<-function(db){
  UseMethod("reload")
}


"reload.emuDB"<-function(db){
  return(load.emuDB(db[['basePath']]))
}
