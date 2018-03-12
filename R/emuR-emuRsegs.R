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
##' @param dbName name of emuDB
##' @param seglist segment list data.frame
##' @param query query string
##' @param type type of list elements
##' #@export make.emuRsegs
make.emuRsegs <- function(dbName,seglist,query,type)
{
  
  class(seglist) <- c("emuRsegs","emusegs", "data.frame")

  attr(seglist, "query") <- query
  attr(seglist, "type") <- type
  attr(seglist, "database") <- dbName
  
  seglist
}


##' Print emuRsegs segment list
##' @param x object to print
##' @param ... additional params
##' @export
"print.emuRsegs" <-  function(x, ...) 
{
  cat(attributes(x)$type, " list from database: ", attributes(x)$database, "\n")
  cat("query was: ", attributes(x)$query, "\n" )
  printX='[.data.frame'(x,c('labels','start','end','session','bundle','level','type'))
  
  print.data.frame(printX, ...)
  
  #cat("\nNOTE: to reduce the verboseness of the output not all colums of an emuRsegs object are printed. Use print.data.frame() to print all columns.\n")
}

##' Sort emuRsegs segment list by session, bundle and sample_start 
##' @param x object to print
##' @param decreasing NOT IMPLEMENTED!
##' @param ... additional params
##' @export
"sort.emuRsegs" <-  function(x, decreasing, ...) 
{

  old_atts = attributes(x)
  
  sl_df_sorted = dplyr::arrange_(x, "session", "bundle", "sample_start")
  
  attributes(sl_df_sorted) = old_atts
  
  return(sl_df_sorted)
}


# S3 method definition
as.emusegs <- function(x, ...){
  UseMethod("as.emusegs", x)
}

as.emusegs.emuRsegs <- function(x, ...){
  emusegs = make.seglist(x$labels, x$start, x$end, x$utts, attr(x, "query"), type = attr(x, "type"), database = attr(x, "database"))
  return(emusegs)
} 
