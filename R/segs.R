## compatibility to emusegs
## methods:
## read.emusegs OK (type cast to emusegs) Override?
## make. OK different constructor
## is.seglist OK
## modify.seglist Problematic. No S3 method we cannot overload. Warning?
## emusegs.database OK
## emusegs.type OK (question: are mixed (EVENT and SEGMENT) seglist possible in legacy Emu ?)
## emusegs.query
## print.emusegs OK But shows too many columns: TODO select a good set of columns for an S3 override method
## [.emusegs OK But not clear what emusegs really does (and code includes version$major switch (on S versions?))
## summary.emusegs OK
## label.emusegs OK
## as.matrix.emusegs OK
## write.emusegs OK (typecast to emusegs) TODO Print warning about data loss!
## start.emusegs OK
## end.emusegs OK
## utt.emusegs OK
## dur.emusegs OK

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


##' Print emuDB segment list
##' @export
"print.emuRsegs" <-  function(x, ...) 
{
  cat(attributes(x)$type, " list from database: ", attributes(x)$database, "\n")
  cat("query was: ", attributes(x)$query, "\n" )
  #if( version$major >= 5 ) {
  #oldClass(x) <- "data.frame"
  #} else {
  class(x) <- "data.frame"
  #}
  
  
  print.data.frame(x[,c('labels','session','bundle','start','end')], ...)
}


