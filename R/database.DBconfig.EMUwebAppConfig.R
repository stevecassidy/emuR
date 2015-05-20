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

###########################################
# CRUD operation for perspectives

##' Add perspective to emuDB
##' 
##' Add EMUwebAppConfig$perspective to emuDB
##' @param dbName name of loaded emuDB
##' @param name name of perspective
##' @param dbUUID optional UUID of loaded emuDB
##' @author Raphael Winkelmann
##' @export
##' @keywords emuDB database DBconfig Emu 
add_perspective <- function(dbName, 
                            name,
                            dbUUID = NULL){
  
  dbObj = .load.emuDB.DBI(name = dbName, uuid = dbUUID)
  
  curPersp = list_perspectives(dbName = dbName, dbUUID = dbUUID)
  # check if level defined
  if(name %in% curPersp$name){
    stop("Perspective with name: '", name, "' already exists")
  }
  
  persp = create.EMUwebAppConfig.perspective(name = name, 
                                             signalCanvases = create.EMUwebAppConfig.signalCanvas(order = c("OSCI", "SPEC"), 
                                                                                                  assign = NULL, contourLims = NULL),
                                             levelCanvases = create.EMUwebAppConfig.levelCanvas(order = NULL),
                                             twoDimCanvases = NULL)
  
  l = length(dbObj$DBconfig$EMUwebAppConfig$perspectives)
  
  dbObj$DBconfig$EMUwebAppConfig$perspectives[[l + 1]] = persp
  
  # store changes
  .store.schema(dbObj)
  
}


##' List perspectives of emuDB
##' 
##' List EMUwebAppConfig$perspectives of emuDB
##' @param dbName name of loaded emuDB
##' @param dbUUID optional UUID of loaded emuDB
##' @author Raphael Winkelmann
##' @export
##' @keywords emuDB database DBconfig Emu 
list_perspectives <- function(dbName, dbUUID = NULL){
  
  dbObj=.load.emuDB.DBI(name=dbName, uuid = dbUUID)
  
  df = data.frame(name = character(),
                  signalCanvasesOrder = character(),
                  levelCanvasesOrder = character(),
                  stringsAsFactors = F)
  
  for(p in dbObj$DBconfig$EMUwebAppConfig$perspectives){
    df = rbind(df , data.frame(name = p$name,
                               signalCanvasesOrder = paste(p$signalCanvases$order, collapse = "; "),
                               levelCanvasesOrder = paste(p$levelCanvases$order, collapse = "; "),
                               stringsAsFactors = F))
  }
  
  return(df)
}

modify_perspective <- function(){
  stop("currently not implemented")
}

##' Remove perspective from emuDB
##' 
##' List EMUwebAppConfig$perspective from emuDB
##' @param dbName name of loaded emuDB
##' @param name name of perspective
##' @param dbUUID optional UUID of loaded emuDB
##' @author Raphael Winkelmann
##' @export
##' @keywords emuDB database DBconfig Emu 
remove_perspective <- function(dbName, 
                               name,
                               dbUUID = NULL){
  
  dbObj=.load.emuDB.DBI(name=dbName, uuid = dbUUID)
  
  curPersp = list_perspectives(dbName = dbName, dbUUID = dbUUID)
  
  # check if perspective defined
  if(!name %in% curPersp$name){
    stop("No perspective with name: '", name, "' found!")
  }
  
  for(i in 1:length(dbObj$DBconfig$EMUwebAppConfig$perspectives)){
    if(dbObj$DBconfig$EMUwebAppConfig$perspectives[[i]]$name == name){
      dbObj$DBconfig$EMUwebAppConfig$perspectives[[i]] = NULL
    }
  }
  # store changes
  .store.schema(dbObj)
  
}

###########################################
# CRUD operation for signalCanvasesOrder


##' Set signalCanvasesOrder of emuDB
##' 
##' @param dbName name of loaded emuDB
##' @param perspectiveName name of perspective
##' @param dbUUID optional UUID of loaded emuDB
##' @param order character vector containig names of ssffTrackDefinitions
##' @author Raphael Winkelmann
##' @export
##' @keywords emuDB database DBconfig Emu 
set_signalCanvasesOrder <- function(dbName,
                                    perspectiveName,
                                    order,
                                    dbUUID = NULL){
  
  dbObj=.load.emuDB.DBI(name = dbName, uuid = dbUUID)
  
  curTracks = c("OSCI", "SPEC", list_ssffTrackDefinitions(dbName = dbName, dbUUID = dbUUID)$name)
  
  #check if tracks given are defined
  for(t in order){
    if(!t %in% curTracks){
      stop("No ssffTrackDefinition present with name '", t, "'!")
    }
  }
  
  for(i in 1:length(dbObj$DBconfig$EMUwebAppConfig$perspectives)){
    if(dbObj$DBconfig$EMUwebAppConfig$perspectives[[i]]$name == perspectiveName){
      dbObj$DBconfig$EMUwebAppConfig$perspectives[[i]]$signalCanvases$order = as.list(order)
      break
    }
  }
  
  # store changes
  .store.schema(dbObj)
}


##' Get signalCanvasesOrder of emuDB
##' 
##' @param dbName name of loaded emuDB
##' @param perspectiveName name of perspective
##' @param dbUUID optional UUID of loaded emuDB
##' @author Raphael Winkelmann
##' @export
##' @keywords emuDB database DBconfig Emu 
get_signalCanvasesOrder <- function(dbName,
                                    perspectiveName,
                                    dbUUID = NULL){
  
  dbObj=.load.emuDB.DBI(name = dbName, uuid = dbUUID)
  
  order = NA
  for(p in dbObj$DBconfig$EMUwebAppConfig$perspectives){
    if(p$name == perspectiveName){
      order = unlist(p$signalCanvases$order)
    }
  }
  return(order)
}

###########################################
# CRUD operation for levelCanvasesOrder


##' Set levelCanvasesOrder of emuDB
##' 
##' @param dbName name of loaded emuDB
##' @param perspectiveName name of perspective
##' @param dbUUID optional UUID of loaded emuDB
##' @param order character vector containig names of levelDefinitions
##' @author Raphael Winkelmann
##' @export
##' @keywords emuDB database DBconfig Emu 
set_levelCanvasesOrder <- function(dbName,
                                    perspectiveName,
                                    order,
                                    dbUUID = NULL){
  
  dbObj=.load.emuDB.DBI(name = dbName, uuid = dbUUID)
  
  curLevelNames = list_levelDefinitions(dbName = dbName, dbUUID = dbUUID)$name
  curLevelTypes = list_levelDefinitions(dbName = dbName, dbUUID = dbUUID)$type
  
  #check if levels given are defined and of correct type
  for(t in order){
    if(!t %in% curLevelNames){
      stop("No levelDefinition present with name '", t, "'!")
    }
    lt = curLevelTypes[curLevelNames == t]
    if(!lt %in% c("SEGMENT", "EVENT")){
      stop("levelDefinition with name '", t, "' is not of type 'SEGMENT' or 'EVENT'")
    }
  }
  
  for(i in 1:length(dbObj$DBconfig$EMUwebAppConfig$perspectives)){
    if(dbObj$DBconfig$EMUwebAppConfig$perspectives[[i]]$name == perspectiveName){
      dbObj$DBconfig$EMUwebAppConfig$perspectives[[i]]$levelCanvases$order = as.list(order)
      break
    }
  }  
  # store changes
  .store.schema(dbObj)
}


##' Get levelCanvasesOrder of emuDB
##' 
##' @param dbName name of loaded emuDB
##' @param perspectiveName name of perspective
##' @param dbUUID optional UUID of loaded emuDB
##' @author Raphael Winkelmann
##' @export
##' @keywords emuDB database DBconfig Emu 
get_levelCanvasesOrder <- function(dbName,
                                   perspectiveName,
                                   dbUUID = NULL){
  
  dbObj=.load.emuDB.DBI(name = dbName, uuid = dbUUID)
  
  order = NA
  for(p in dbObj$DBconfig$EMUwebAppConfig$perspectives){
    if(p$name == perspectiveName){
      order = unlist(p$levelCanvases$order)
    }
  }
  return(order)
}


# FOR DEVELOPMENT 
# library('testthat') 
# test_file('tests/testthat/test_database.DBconfig.EMUwebAppConfig.R')

