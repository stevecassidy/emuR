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

"print.trackdata"<- function(dataset,...)
{
  if(is.null(dataset$trackname)) 
    cat("trackdata from unknown track.\n")
  else
    cat("trackdata from track:", dataset$trackname,"\n")

  cat("index:\n")
  print(dataset$index)
  cat("ftime:\n")
  print(dataset$ftime)
  cat("data:\n")
  print(dataset$data)
}

"[.trackdata"<- function(dataset, i=1:nrow(dataset$index), j, ...)
{
  ## dataset[i] gets the data for the i'th segment as a trackdata object

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

"summary.trackdata" <- function(dataset, ...)
{
  if( is.matrix(dataset$data)){
    dimens <- ncol(dataset$data)
    len <- nrow(dataset$data)
  }
  else {
    dimens <- 1
    len <- length(dataset$data)
  }
  cat("Emu track data from", nrow(dataset$index), "segments\n\n")
  cat("Data is ", dimens, "dimensional from track", 
      dataset$trackname,"\n")
  cat("Mean data length is ", len/nrow(dataset$index), " samples\n")
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

# Local Variables:
# mode:S
# S-temp-buffer-p:t
# End:
