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
