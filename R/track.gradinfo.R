
track.gradinfo <- function( trackdata ) {
  ## track.gradinfo  --
  ## generate various bits of information about a trackdata
  ## object:
  ##    - duration
  ##    - start, end: data values at the start and end of the segment
  ##    - delta: the difference between start and end data points
  ##    - slope: the slope of the data (delta/duration)
  ##
  
  result <- dapply(trackdata, track.gradinfo.sub)
  ## all we want is the data which will be one row per segment
  result <- data.frame( result$data )
  
  ## Put appropriate column headers on the data frame
  ## 
  ## this would be better off in track.gradinfo.sub but
  ## because of the way that dapply works it has to go here
  w <- ncol(trackdata$data)
  names(result) <- c("duration", 
                     paste("start", 1:w, sep=""),
                     paste("end", 1:w, sep=""),
                     paste("delta", 1:w, sep=""),
                     paste("slope", 1:w, sep="") )
  return( result )
}

track.gradinfo.sub <- function( data, ftime ) {
  ## track.gradinfo.sub -- 
  ## do the work of track.gradinfo, return the various
  ## measures in the right form for dapply
  n <- nrow(data)
  dur <- ftime[2]-ftime[1]
  ## delta is the difference between the start and end data points
  delta <- diff( data[c(1,n),] )
  ## slope is the delta/duration
  slope <- delta/dur
  data <- matrix( c( dur, data[1,], data[n,], delta, slope ), nrow=1)
  
  ## ftime will be discarded anyway but let's do the right thing
  ## and set the start and end to the segment mid point
  mid <- ftime[1]+dur/2
  ftime <- c(mid, mid)
  return( list( data=data, ftime=ftime ) )
}

