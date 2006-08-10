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


dtime <- function(dataset, times, single = T, average = T) {
  if(!is.matrix(dataset$data))
    dataset$data <- cbind(dataset$data)

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
        if(average) {
          cval <- mean(cval)
        }
        else {
          cval <- cval[1]
        }
      }
    }
    mat <- rbind(mat, dat[cval,  ])
  }

  if(ncol(mat) == 1) {
    c(mat)
  }
  else {
    mat
  }
}






"dcut" <-
function (trackdata, left.time, right.time, single = T, average = T, 
    prop = F) 
{
    if (prop) {
        if (missing(right.time)) 
            omat <- dextract(trackdata, left.time)
        else omat <- dextract(trackdata, left.time, right.time)
    }
    else {
        if (missing(right.time)) 
            omat <- dtime(trackdata, left.time, single = single, 
                average = average)
        else {
            if (length(left.time) != nrow(trackdata$ftime)) {
                stop("different number of elements in left.time and $ftime")
            }
            if (length(right.time) != nrow(trackdata$ftime)) {
                stop("different number of elements in right.time and $ftime")
            }
            if (any(left.time < trackdata$ftime[, 1])) 
                stop("some left.time values are less than $ftime[,1]")
            if (any(right.time > trackdata$ftime[, 2])) 
                stop("some right.time values are greater than $ftime[,2]")
            if (any(right.time <= left.time)) 
                stop("some right.time values are before the corresponding left.time")
            lval <- nrow(trackdata$index)
            for (j in 1:lval) {
                tdat <- dcut.sub(trackdata[j], left.time[j], 
                  right.time[j])
                if (j == 1) 
                  omat <- tdat
                else omat <- bind(omat, tdat)
            }
        }
    }
if(is.spectral(trackdata$data))
{
if(is.trackdata(omat))
{
attr(omat$data, "fs") <- attr(trackdata$data, "fs")
if(!is.spectral(omat$data))
class(omat$data) <- c(class(omat$data), "spectral")
}
else
{
attr(omat, "fs") <- attr(trackdata$data, "fs")
if(!is.spectral(omat))
class(omat) <- c(class(omat), "spectral")
}

}
    return(omat)
}




"dcut.sub" <- function(trackdata, left.time, right.time)
{
  vals <- trackdata$data
  left <- trackdata$ftime[1]
  right <- trackdata$ftime[2]

  if(is.matrix(vals))
    N <- nrow(vals)
  else
    N <- length(vals)
  
  times <- seq(left, right, length = N)
  first <- closest(times, left.time)

  if(length(first) > 1)
    first <- round(mean(first))

  second <- closest(times, right.time)

  if(length(second) > 1)
    second <- round(mean(second))

  if(is.matrix(vals))
    trackdata$data <- vals[first:second,  ]
  else
    trackdata$data <- cbind(vals[first:second]
                               )
  trackdata$ftime <- cbind(times[first], times[second])
  trackdata$index <- cbind(1, length(first:second))
  
  as.trackdata(trackdata$data, trackdata$index, trackdata$ftime)
}


# Local Variables:
# mode:S
# S-temp-buffer-p:t
# End:
