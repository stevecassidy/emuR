#############################################################################
#                                                                           #
#   copyright            : (C) 2003 SHLRC, Macquarie University             #
#   email                : Steve.Cassidy@mq.edu.au			    #
#   url			 : http://www.shlrc.mq.edu.au/emu		    #
#									    #
#   This program is free software; you can redistribute it and/or modify    #
#   it under the terms of the GNU General Public License as published by    #
#   the Free Software Foundation; either version 2 of the License, or       #
#   (at your option) any later version.                                     #
#									    #
#############################################################################

applyDCT <- function(trackdata, numOcoef=3, prop=1) {
  ## apply the DCT function to the data for each  segment
  ## in the trackdata object

  if( prop <= 0 || prop > 1 ) {
    stop("prop argument to applyDCT must be between 0 and 1")
  }

  # we're only interested in the data matrix since each segment
  # is transformed to a single row in  the result 
  return(dapply( trackdata, applyDCT.sub, numOcoef, prop )$data)
}


applyDCT.sub <- function( data, ftime, numOcoef, prop )  {
  ## apply the DCT function to one segment's worth of data
  ## conform  to the dapply  rules and return a list of
  ## new data and ftime

  ## data is a matrix of data
  ## ftime is a list of start/end times

  # only apply the transform to the proportion given  by prop
  rows <- nrow(data)
  data <- data[1:(rows*prop),]

  result <- NULL
  for(i in 1:ncol(data)) {
    result <- c(result, DCT(data[,i], inv=F)[1:numOcoef])
  }
  # generate an ftime vector corresponding  to the center of
  # the data
  time <- ftime[1] + prop*(ftime[2]-ftime[1])/2

  #  return  the appropriate structure
  return(list(data=matrix(result,nrow=1), ftime=c(time,time)))
}

DCT <- function(data, inv = T) {
  ##
  ## transform one - forward DCT
  ## this function calculates the discrete cosine transform of the time varying
  ## signal b_n
  ##
  ##                               N-1
  ##               G(k) = 2*e(n)/N*SUM b_n*cos[pi*(2k+1)*n/2N] k=0..N-1(3)
  ##                               n=0
  ## transform two - inverse DCT
  ## this function reconstructs a time domain waveform from the cosine
  ## waveform G(k), a smoothed version of the orginal b_n can be 
  ## obtained if less than  N-1 cosines are used.
  ## 
  ##                     N-1
  ##               b_n = SUM e(n)*G(k)*cos[pi*(2k+1)*n/2N] n=0..N-1(2)
  ##                     k=0
  ##
  ##
  ## where
  ##
  ##               e(n) = 1/sqrt(2) if n=0
  ##                    = 1, otherwize
  ## ref Discrete Cosine Transform by K.R. Rao and P. Yip
  ##

  ldat <- length(data)
  transdat <- vector(length = ldat)

  if(!inv) {
    ##  transform one
    transdat[1] <- (2/(ldat * sqrt(2))) * sum(data)
    for(n in 1:(ldat - 1)) {
      j <- 0:(ldat - 1)
      transdat[n + 1] <- (2/ldat) * sum(data *
                                        cos((pi * n * (2 * j + 1))/(2 * ldat)))
    }
  } else {
    ## transform two
    m <- 1:(ldat - 1)
    for(n in 0:(ldat - 1)) {
      transdat[n + 1] <- (1/sqrt(2)) * data[1] *
        cos((pi * 0 * (2 * n + 1))/(2 * ldat)) +
          sum( data[m + 1] * cos((pi * m * (2 * n + 1))/(2 * ldat)))
     
      ## I have checked this formula many times, just remember when
      ## the summation is done it is done for the nth sample of each
      ## of the m cosines with amplitudes data[m+1] 
    }
  }
  transdat
}

