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


"ddiff"<- function(dataset, n = 1, smoothing = T)
{
  ## differentiates a list, as returned by track, to the nth
  ## order, readjusting the index and ftime values each time
  ## dataset: a list as returned by track
  ## n: the order of differentiation

  ## now we apply the function to the data using dapply
  outdat <- dapply(dataset, ddiff.sub, n = n)
  if(smoothing)
    dsmooth(outdat)
    else outdat
}

ddiff.sub <- function(data, ftime, n)
{
  ## a function to be called by dapply
  ## data: a data matrix
  ## ftime: a start-end pair
  ## n: number of times to differentiate
  ## smoothing: if T, apply smooth to data too
  ## returns: a list of $data values differentiated 
  ## and $ftime values adjusted accordingly
  ## values in $data that are returned are per millisecond
  if(is.matrix(data)) lval <- nrow(data) else lval <- length(data
							     )
  if(lval < 1) stop("not enough data points in ddiff")	
  ## compute the time between samples
  interval <- (ftime[2] - ftime[1])/lval	
  ## do the differentiation
  data <- diff(data, differences = n)
  if(is.matrix(data))
    lval <- nrow(data)
    else lval <- length(data)
  timefactor <- (n * interval)/2
  ftime[1] <- ftime[1] + timefactor
  ftime[2] <- ftime[2] - timefactor	
  ## smooth the data as appropriate
  data <- data/interval	
  ## and return the data in the required format
  list(data = data, ftime = ftime)
}



# Local Variables:
# mode:S
# S-temp-buffer-p:t
# End:


