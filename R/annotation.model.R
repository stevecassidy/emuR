## Creates emuSX annotation level object
## 
## @param name name of the level
## @param type item types: ITEM,EVENT,SEGMENT
## @param sampleRate sample rate of corresponding signal file
## @param items list of items
## @return object of class 'emuSX.annotation.model.Level'
## @author Klaus Jaensch
## @keywords emuSX Level Emu
## 
create.bundle.level <- function(name=NULL,type=NULL,sampleRate=NULL,items=list()){
  o=list(name=name,type=type,sampleRate=sampleRate,items=items)
  class(o) <- 'emuSX.annotation.model.Level'
  invisible(o)
}

create.interval.item <- function(id=NULL,sampleStart,sampleDur,labels){
  o=list(id=id,sampleStart=sampleStart,sampleDur=sampleDur,labels=labels)
  #attr(o,'ips.persist')<-list(localId='id')
  class(o) <- c('emuSX.annotation.model.IntervalItem','emuSX.annotation.model.Item')
  invisible(o)
}

create.event.item <- function(id=NULL,samplePoint,labels){
  o=list(id=id,samplePoint=samplePoint,labels=labels)
  class(o) <- c('emuSX.annotation.model.EventItem','emuSX.annotation.model.Item')
  invisible(o)
}

create.item <- function(id=NULL,labels=NULL){
  o=list(id=id,labels=labels)
  class(o) <- c('emuSX.annotation.model.Item')
  invisible(o)
}

 
## Create emuSX link object
## Directed link of two annotation items
## (A collecation of this objects represents the hierarchical information of EMU hlb files)
## 
## @param fromID Item ID from wich to link
## @param toID Item ID to which to link
## @param label optional label describing the link
## @return object of class emuSX.annotation.model.Link
## @author Klaus Jaensch
## @keywords emuSX link Emu
## 
create.link <- function(fromID,toID,label=NULL){
  o=list(fromID=fromID,toID=toID,label=label)  
  class(o) <- c('emuSX.annotation.model.Link')
  #refs=list(from='fromId',to='toId')
  #types=list(from='emuSX.annotation.model.Item',to='emuSX.annotation.model.Item')
  #attr(o,'ips.persist')<-list(types=types,refs=refs)
 
  invisible(o)
}
