## compatibility to emusegs
## methods:
## read.emusegs OK (type cast to emusegs) Override?
## make. OK different constructor
## is.seglist OK
## modify.seglist Problematic. No S3 method we cannot overload.
## emusegs.database OK
## emusegs.type OK (question: are mixed (EVENT and SEGMENT) seglist possible in legacy Emu ?)
## emusegs.query
## print.emusegs OK But shows too many columns: TODO select a good set of columns for an S3 override method
## [.emusegs OK But not clear what emusegs really does (and code includes version$major switch (on S versions?))
## summary.emusegs OK
## label.emusegs OK
## as.matrix.emusegs OK
## write.emusegs OK (typecast to emusegs) Override?
## start.emusegs OK
## end.emusegs OK
## utt.emusegs OK
## dur.emusegs OK
## getFiles.emusegs TODO implement? Not available in legacy Emu! OK

##' Make emuDB segment list
##' @export make.emuRsegs
make.emuRsegs <- function(dbName,seglist,query,type)
{
  
  
  
#   seglist <- data.frame(labels=I(as.character(labels)),
#                         start=as.numeric(start), 
#                         end=as.numeric(end), 
#                         utts=I(as.character(utts)))
#   
 
  class(seglist) <- c("emuRsegs","emusegs", "data.frame")
  
  
  attr(seglist, "query") <- query
  attr(seglist, "type") <- type
  attr(seglist, "database") <- dbName
  
  seglist
}

### @export
#"modify.emuRsegs" <- function(...){
#  stop("Unlike legacy Emu segment list class 'emusegs', objects of class 'emuRsegs' are immutable and cannot be changed.")
#}

##'
##' @export
"[.emuRsegs"<- function(segs,i,j)
{
  # TODO j and i  can contain sequence values
  # i.e. j==c('utts','start'), j==1:4 , ...
  #cat("i: ",i," len",length(i),"\n")
  
  if(!missing(j)){
    #cat("length of j :",length(j),"\n")
    if(inherits(j,'character') & length(j)>0){
      
      #newColList=character(0)
      if(missing(i)){
        nrow=nrow(segs)
      }else{
        nrow=length(i)
      }
      newDf=data.frame(matrix(NA, nrow = nrow, ncol = 0))
      for(reqCol in j){
        #cat("Req col: ",reqCol,"\n")
        if(reqCol=='labels'){
          lblSSt='[.data.frame'(segs,i,'labelSeqStr')
          col=eval
        }else if(reqCol=='start'){
          sampleSt='[.data.frame'(segs,i,'sampleStart')
          sampleRa='[.data.frame'(segs,i,'sampleRate')
          #cat("Start: ",sampleSt,sampleRa,"\n")
          col=((sampleSt + 0.5 )/ sampleRa * 1000.0)
        }else if(reqCol=='end'){
          sampleEn='[.data.frame'(segs,i,'sampleEnd')
          sampleRa='[.data.frame'(segs,i,'sampleRate')
          col=((sampleEn + 1.5 )/ sampleRa * 1000.0)
        }else if(reqCol=='dur'){
          sampleSt='[.data.frame'(segs,i,'sampleStart')
          sampleEn='[.data.frame'(segs,i,'sampleEnd')
          sampleRa='[.data.frame'(segs,i,'sampleRate')
          col=((sampleEn + 1.5 -sampleSt +0.5)/ sampleRa * 1000.0)
        }else if(reqCol=='sampleDur'){
          sampleSt='[.data.frame'(segs,i,'sampleStart')
          sampleEn='[.data.frame'(segs,i,'sampleEnd')
         
          col=as.integer(sampleEn-sampleSt)
        }else{
          col='[.data.frame'(segs,i,reqCol)
        }
        #cat("Column: ",col,"\n")
        
        newDf=cbind.data.frame(newDf,col)
      }
      names(newDf)<-j
      return(newDf)
    }
    
  }
  NextMethod("[")
}

##'
##' @export
"$.emuRsegs"<- function(segs,j)
{
  if(j=='start'){
  sampleSt=segs[['sampleStart']]
  sampleRa=segs[['sampleRate']]
 
  return((sampleSt + 0.5 )/ sampleRa * 1000.0)
  }else if(j=='end'){
    sampleEn=segs[['sampleEnd']]
    sampleRa=segs[['sampleRate']]
    return((sampleEn + 1.5 )/ sampleRa * 1000.0)
  }
  NextMethod("$")
}

##' Print emuDB segment list
##' @export
"print.emuRsegs" <-  function(x, ...) 
{
  cat(attributes(x)$type, " list from database: ", attributes(x)$database, "\n")
  cat("query was: ", attributes(x)$query, "\n" )
  #if( version$major >= 5 ) {
  #  oldClass(x) <- "data.frame"
  #} else {
  #  class(x) <- "data.frame"
  #}
  
  print.data.frame(x, ...)
}


