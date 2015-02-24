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
  o <- list(perspectives=perspectives,restrictions=list(showPerspectivesSidebar=TRUE))
  class(o) <- 'emuDB.EMUwebAppConfig'
  invisible(o)
}

load.emuDB.DBconfig<-function(DBconfigFilePath){
  # with warn=TRUE and some files
  # R complains about incomplete (last) line
  # See https://stat.ethz.ch/pipermail/r-help/2006-July/108654.html
  # TODO does problem with jsonlite still exist ?
  dbCfgJSONLns=readLines(DBconfigFilePath,warn=FALSE)
  dbCfgJSON=paste(dbCfgJSONLns,collapse='')
  dbCfgPersisted=jsonlite::fromJSON(dbCfgJSON,simplifyVector=FALSE)
  
  # unmarshal schema object (set class names)
  schema=unmarshal.from.persistence(dbCfgPersisted,emuR.persist.class[['DBconfig']])
  return(schema)
}

get.levelDefinition <- function(DBconfig, name){
  res = NULL
  for(ld in DBconfig$levelDefinitions){
    if(ld$name == name){
      res = ld
      break
    }
  }
  return(res)
}

get.ssfftrack.names.used.by.webapp.config<-function(EMUwebAppConfig){
  nms=c()
  for(perspective in EMUwebAppConfig[['perspectives']]){
    scvss=perspective[['signalCanvases']]
    if(!is.null(scvss)){
      for(cvsNm in scvss[['order']]){
        if(cvsNm!='OSCI' & cvsNm!='SPEC'){
          nms=c(nms,cvsNm)
        }
      }
      for(ass in scvss[['assign']]){
        nms=c(nms,ass[['ssffTrackName']])
      }
    }
    
    twoDimCanvaces=perspective[['twoDimCanvases']]
    if(!is.null(twoDimCanvaces)){
      for(twoDimDrawDef in twoDimCanvaces[['twoDimDrawingDefinitions']]){
        for(dot in twoDimDrawDef[['dots']]){
          
          xSsffTrack=dot[['xSsffTrack']]
          ySsffTrack=dot[['ySsffTrack']]
          if(!is.null(xSsffTrack)){
            nms=c(nms,xSsffTrack)
          }
          
          if(!is.null(ySsffTrack)){
            nms=c(nms,ySsffTrack)
          }
        }
      }
    }
  }
  return(unique(nms))
}

# FOR DEVELOPMENT 
#library('testthat') 
#test_file('tests/testthat/test_database.DBconfig.R')
