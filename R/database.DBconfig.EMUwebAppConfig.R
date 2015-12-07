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
create.EMUwebAppConfig.twoDimCanvases <- function(order){
  o <- list(order=order)
  class(o) <- 'emuDB.EMUwebAppConfig.twoDimCanvases'
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



###########################################
# CRUD operation for perspectives

##' Add perspective to emuDB
##' 
##' Add perspective to emuDB. The EMU-webApp subdivides different ways 
##' to look at an emuDB into so called perspectives. These perspectives, 
##' between which you can switch in the web application, contain 
##' information on what levels are displayed, which ssffTracks are drawn, 
##' and so on. For more information on the structural elements of an emuDB 
##' see \code{vignette{emuDB}}.
##' @param dbName name of loaded emuDB
##' @param name name of perspective
##' @param dbUUID optional UUID of loaded emuDB
##' @export
##' @keywords emuDB database DBconfig Emu 
##' @examples
##' \dontrun{
##' 
##' ##################################
##' # prerequisite: loaded "ae" emuDB 
##' # (see ?load_emuDB for more information)
##' 
##' # add perspective called "justTones" to the "ae" emuDB
##' add_perspective(dbName ="ae",
##'                 name = "justTones") 
##'                 
##' # add levelCanvasOrder so only the "Tone" level is displayed
##' set_levelCanvasesOrder(dbName = "ae", 
##'                        perspectiveName = "justTones", 
##'                        order = c("Tone"))
##' 
##' }
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
                                             twoDimCanvases = create.EMUwebAppConfig.twoDimCanvases(order = NULL))
  
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
##' Get signalCanvasesOrder array that specifies which signals are 
##' displayed in the according perspective by the EMU-webApp. A entry in this array 
##' refers to either the name of a ssffTrackDefinition or a predefined string: \code{"OSCI"} which 
##' represents the oscillogram or \code{"SPEC"} which represents the 
##' spectrogram.
##' 
##' @param dbName name of loaded emuDB
##' @param perspectiveName name of perspective
##' @param dbUUID optional UUID of loaded emuDB
##' @export
##' @keywords emuDB database DBconfig Emu
##' @examples 
##' \dontrun{
##' 
##' ##################################
##' # prerequisite: loaded "ae" emuDB 
##' # (see ?load_emuDB for more information)
##' 
##' # get signal canvas order of the "default"
##' # perspective of the "ae" emuDB
##' get_signalCanvasesOrder(dbName = "ae", 
##'                         perspectiveName = "default")
##'                         
##' }

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
##' Get levelCanvasesOrder array that specifies what levels are 
##' displayed in the according perspective by the EMU-webApp. 
##' Note that only levels of type 
##' \code{"SEGMENT"} or \code{"EVENT"} can be displayed as levelCanvases. 
##' Levels of the type \code{"ITEM"} are always visible in the "show hierrachy"
##' modal.
##' @param dbName name of loaded emuDB
##' @param perspectiveName name of perspective
##' @param dbUUID optional UUID of loaded emuDB
##' @export
##' @keywords emuDB database DBconfig Emu 
##' @examples 
##' \dontrun{
##' 
##' ##################################
##' # prerequisite: loaded "ae" emuDB 
##' # (see ?load_emuDB for more information)
##' 
##' # get level canvas order of the "default"
##' # perspective of the "ae" emuDB
##' get_levelCanvasesOrder(dbName = "ae", 
##'                        perspectiveName = "default")
##' 
##' }
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

