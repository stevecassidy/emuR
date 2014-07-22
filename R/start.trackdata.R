##' start.trackdata
##' 
##' see function
##' 
##' 
##' @keywords internal
##' @export
"start.trackdata" <- function(x, ...)
{
  x$ftime[,1]
}


##' end.trackdata
##' 
##' see function
##' 
##' 
##' @keywords internal
##' @export
"end.trackdata" <- function(x, ...)
{
  x$ftime[,2]
}

