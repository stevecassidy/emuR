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

"dcut" <- function( dataset, cut=0.5 )
{
  ## work out the cut time
  times <- dataset$ftime[,1] + (dataset$ftime[,2] - dataset$ftime[,1])*cut

  dtime( dataset, times, single=T, method="first" )
}



"dtime"<- function(dataset, times, single = F, method = "average")
{
  ## extracts the data values at the corresponding times in ms
  ## single: if T, then always return only
  ## one value - sometimes, two time values are
  ## returned in times[n] is equitemporal between
  ## dataset$ftime[n,1] and dataset$ftime[n,2]
  ## method: only applies if single=T; if
  ## times[n] is equitemporal between
  ## dataset$ftime[n,1] and dataset$ftime[n,2], then
  ## the two returned values are averaged if
  ## method = "average"; otherwise the first
  ## of the two values is taken
  if(!is.matrix(dataset$data)) dataset$data <- cbind(dataset$data)
  if(!is.matrix(dataset$index)) {
    dataset$index <- rbind(dataset$index)
    dataset$ftime <- rbind(dataset$ftime)
  }
  mat <- NULL
  for(j in 1:length(times)) {
    left <- dataset$index[j, 1]
    right <- dataset$index[j, 2]
    dat <- dataset$data[left:right,  ]
    if(!is.matrix(dat))
      dat <- cbind(dat)
    lval <- right - left + 1
    left.time <- dataset$ftime[j, 1]
    right.time <- dataset$ftime[j, 2]
    seq.times <- seq(left.time, right.time, length = lval)
    cval <- closest(seq.times, times[j])
    if(single) {
      if(length(cval) > 1) {
	if(method == "average") {
	  cval <- mean(cval)
	} else {
	  cval <- cval[1]
	}
      }
    }
    mat <- rbind(mat, dat[cval,  ])
  }
  if(ncol(mat) == 1) {
    c(mat)
  } else { 
    mat
  }
}

# Local Variables:
# mode:S
# S-temp-buffer-p:t
# End:
