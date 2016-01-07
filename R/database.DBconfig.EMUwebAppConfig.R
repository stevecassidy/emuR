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

##' Add / List / Remove perspective to / of / from emuDB
##' 
##' Add / List / Remove perspective to / of / from emuDB. The EMU-webApp subdivides different ways 
##' to look at an emuDB into so called perspectives. These perspectives, 
##' between which you can switch in the web application, contain 
##' information on what levels are displayed, which ssffTracks are drawn, 
##' and so on. For more information on the structural elements of an emuDB 
##' see \code{vignette{emuDB}}.
##' @param dbName name of loaded emuDB
##' @param name name of perspective
##' @param dbUUID optional UUID of loaded emuDB
##' @name AddListRemovePerspective
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
##' # list perspectives of "ae" emuDB
##' list_perspectives("ae")
##' 
##' # remove newly added perspective
##' remove_perspective(dbName = "ae",
##'                    name = "justTones")
##'                    
##' }
##' 
NULL

##' @rdname AddListRemovePerspective
##' @export
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


##' @rdname AddListRemovePerspective
##' @export
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


##' @rdname AddListRemovePerspective
##' @export
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


##' Set / Get signalCanvasesOrder of / to / from emuDB
##' 
##' Set / Get signalCanvasesOrder array that specifies which signals are 
##' displayed in the according perspective by the EMU-webApp. An entry in this character vector 
##' refers to either the name of an ssffTrackDefinition or a predefined string: \code{"OSCI"} which 
##' represents the oscillogram or \code{"SPEC"} which represents the 
##' spectrogram. For more information on the structural elements of an emuDB 
##' see \code{vignette{emuDB}}.
##' 
##' @param dbName name of loaded emuDB
##' @param perspectiveName name of perspective
##' @param order character vector containig names of ssffTrackDefinitions or "OSCI" / "SPEC"
##' @param dbUUID optional UUID of loaded emuDB
##' @name SetGetSignalCanvasesOrder
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
##' 
NULL

##' @rdname SetGetSignalCanvasesOrder
##' @export
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


##' @rdname SetGetSignalCanvasesOrder
##' @export
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


##' Set / Get level canvases order of emuDB
##' 
##' Set / Get which levels of an emuDB to display as level canvases (in a 
##' given perspective of the EMU-webApp),
##' and in what order. Level canvases refer to levels of 
##' the type "SEGMENT" or "EVENT" that are displayed by the EMU-webApp. Levels 
##' of type "ITEM" can always be displayed using the hierarchy view of the
##' web application but can not be displayed as level canvases.
##' For more information on the structural elements of an emuDB 
##' see \code{vignette{emuDB}}.
##' 
##' @param dbName name of loaded emuDB
##' @param perspectiveName name of perspective
##' @param dbUUID optional UUID of loaded emuDB
##' @param order character vector containig names of levelDefinitions
##' @name SetGetlevelCanvasesOrder
##' @keywords emuDB database DBconfig Emu 
##' @examples 
##' \dontrun{
##' 
##' ##################################
##' # prerequisite: loaded "ae" emuDB 
##' # (see ?load_emuDB for more information)
##' 
##' # get level canvases order of "ae" emuDB
##' order = get_levelCanvasesOrder(dbName = "ae",
##'                                perspectiveName = "default")
##' 
##' # reverse the level canvases order of "ae" emuDB
##' set_levelCanvasesOrder(dbName = "ae"
##'                        perspectiveName = "default",
##'                        order = rev(order))
##'                        
##' # get level canvases order of "ae" emuDB                       
##' get_levelCanvasesOrder(dbName = "ae",
##'                        perspectiveName = "default")
##' }
##' 
NULL

##' @rdname SetGetlevelCanvasesOrder
##' @export
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


##' @rdname SetGetlevelCanvasesOrder
##' @export
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

