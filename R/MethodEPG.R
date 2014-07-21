##' expand EPG
##' 
##' see function
##' 
##' 
##' @aliases [.EPG
##' @keywords internal
"[.EPG" <- function (palates, i, j, k) 
{
  o <- NextMethod("[")
  class(o) <- c("EPG")
  o
}
