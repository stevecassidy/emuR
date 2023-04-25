
##' @export
`Math.trackdata` <- function (x,...) 
{
  
  ftime <- x$ftime
  inds <- x$index
  nm <- x$name
  o <- get(.Generic)(x$data)
  as.trackdata(o, inds, ftime, nm)
}


##' @export
`Math2.trackdata` <- function (x, digits)
{
  
  ftime <- x$ftime
  inds <- x$index
  nm <- x$name
  o <- get(.Generic)(x$data, digits)
  as.trackdata(o, inds, ftime, nm)
}


##' @export
`Ops.trackdata` <- function (e1, e2) 
{
  x = e1
  y = e2
  arithmetic = c("+", "-", "*", "^", "%%", "%/%", "/")
  compare = c("==", ">", "<", "!=", "<=", ">=")
  
  if (inherits(x, "trackdata")) {
    ftime <- x$ftime
    inds <- x$index
    nm <- x$name
  }
  else {
    ftime <- y$ftime
    inds <- y$index
    nm <- y$name
  }
  if (inherits(x, "trackdata") & inherits(y, "trackdata"))
    o <- get(.Generic)(x$data, y$data)
  else if (inherits(x, "trackdata") & (!inherits(y, "trackdata")))
    o <- get(.Generic)(x$data, y)
  else if ((!inherits(x, "trackdata")) & inherits(y, "trackdata"))
    o <- get(.Generic)(x, y$data)
  if (.Generic  %in% arithmetic)
    result <-    as.trackdata(o, inds, ftime, nm)
  else if (.Generic  %in% compare)
    result <- o
  result
}



##' @export
`Summary.trackdata` <- function (x,..., na.rm=TRUE) 
{
  get(.Generic)(x$data)   
  
}
