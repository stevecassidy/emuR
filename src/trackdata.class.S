#############################################################################
#                                                                           #
#   copyright            : (C) 2000 SHLRC, Macquarie University             #
#   email                : Steve.Cassidy@mq.edu.au			    #
#   url			 : http://www.shlrc.mq.edu.au/emu		    #
#									    #
#   This program is free software; you can redistribute it and/or modify    #
#   it under the terms of the GNU General Public License as published by    #
#   the Free Software Foundation; either version 2 of the License, or       #
#   (at your option) any later version.                                     #
#									    #
#############################################################################


## Methods that define operations on the class "trackdata"
## see also track and frames

"print.trackdata"<- function(x, ...)
{
  if(is.null(x$trackname)) 
    cat("trackdata from unknown track.\n")
  else
    cat("trackdata from track:", x$trackname,"\n")

  cat("index:\n")
  print(x$index, ...)
  cat("ftime:\n")
  print(x$ftime, ...)
  cat("data:\n")
  print(x$data, ...)
}

"[.trackdata"<- function(dataset, i, j, ...)
{
  ## dataset[i] gets the data for the i'th segment as a trackdata object

  if (missing(i)) {
    i <- 1:nrow(dataset$index)
  }

  ftime <- dataset$ftime[i, ,drop=F]

  index <- dataset$index[i, ,drop=F]

  ## work out which rows of data we want
  datarows <- NULL
  for( ind in 1:nrow(index) ){
    datarows <- c(datarows, seq(from=index[ind,1], to=index[ind,2]))
  }

  if(is.matrix(dataset$data)){
    if(missing(j))
      data <- dataset$data[datarows, ,drop=F]
    else
      data <- dataset$data[datarows,j,drop=F]
  }
  else {
      data <- dataset$data[datarows, drop=F]
  }

  # now we need to rehash the index matrix to corrspond to the new
  # data matrix

  lval <- index[, 2] - index[, 1] + 1
  right <- cumsum(lval)
  left <- right + 1
  left <- left[ - length(left)]
  left <- c(1, left)
  nindex <- cbind(left, right)

  dataset$index <- nindex
  dataset$ftime <- ftime
  dataset$data <- data
  return(dataset)
}

"summary.trackdata" <- function(object, ...)
{
  if( is.matrix(object$data)){
    dimens <- ncol(object$data)
    len <- nrow(object$data)
  }
  else {
    dimens <- 1
    len <- length(object$data)
  }
  cat("Emu track data from", nrow(object$index), "segments\n\n")
  cat("Data is ", dimens, "dimensional from track", 
      object$trackname,"\n")
  cat("Mean data length is ", len/nrow(object$index), " samples\n")
  invisible()
}


"as.trackdata" <- function( data, index, ftime, trackname="" )
{
  mat <- list( data=as.matrix(data), 
	      index=index, 
	      ftime=ftime,
	      trackname=trackname)
  if( version$major >= 5 ) {
    oldClass(mat) <- "trackdata"
  } else {
    class(mat) <- "trackdata"
  }
  mat
}

"is.trackdata" <-
function (object) 
{
    return(inherits(object, "trackdata"))
}


# Local Variables:
# mode:S
# S-temp-buffer-p:t
# End:
