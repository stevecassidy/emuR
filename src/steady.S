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

"steady"<- function(values, percent = 25, cumulative = F, meanv = T,
                    leftlim = NULL, rightlim = NULL, smoothing = F)
{
  ## leftlim, rightlim. can be between 0 and 1; e.g. to constrain
  ## the target to be found in the first 30% of the syllable, set
  ## leftlim to 0, rightlim to 30
  lval <- values$index[, 2] - values$index[, 1] + 1
  frlength <- round((lval * percent)/100)
  values$index.old <- values$index
  if(!is.null(leftlim))
    values$index[, 1] <- steady.int(values, leftlim)
  if(!is.null(rightlim))
    values$index[, 2] <- steady.int(values, rightlim)
  if(smoothing)
    values <- dsmooth(values)
  addvals <- values$index[, 1] - 1
  for(j in 1:nrow(values$index)) {
    newdata <- values$data[((values$index[j, 1]):(values$index[j, 2])),  ]
    numframes <- frlength[j]
    distvals <- euclidean(newdata)
    sumvals <- steady.step(distvals, frlength[j])
    temp <- sumvals == min(sumvals)
    nums <- c(1:length(sumvals))
    minno <- nums[temp]
    vec <- c(minno, minno + (numframes - 1))
    vec <- vec + addvals[j]
    if(meanv) {
      sdata <- rbind(values$data[((vec[1]):(vec[2])),  ])
      meanvals <- apply(sdata, 2, mean)
      values$mean <- rbind(values$mean, meanvals)
      values$inds <- rbind(values$inds, vec)
    }
    values$steady <- rbind(values$steady, vec)
  }
  midval <- (values$steady[, 2] + values$steady[, 1])/2
  numval <- midval - values$index.old[, 1]
  denval <- values$index.old[, 2] - values$index.old[, 1]
  values$time <- numval/denval	
  ## calculate the times of the left and right target boundaries
  smat <- NULL
  smat$index <- values$index.old
  smat$ftime <- values$ftime
  left.time <- steady.time(values$inds[, 1], smat)
  right.time <- steady.time(values$inds[, 2], smat)
  values$stime <- cbind(left.time, right.time)
  if( version$major >= 5 ) {
    oldClass(values) <- NULL
  } else {
    class(values) <- NULL
  }
  values
}


"steady.int"<- function(dataset, cut)
{
  ## returns the index number corresponding to cut
  leftin <- dataset$index[, 1]
  rightin <- dataset$index[, 2]
  scalein <- (rightin - leftin) * cut
  round(leftin + scalein)
}


"steady.step"<-
  function(vec, nframes)
{
  ## this function takes a vector and adds up frames 1 to n
  ## then 2 to n+1 then 3 to n + 2 etc. The value of n 
  ## is specified by nframes - for example if the length
  ## of the vector is 10 and nframes is 3, 
  ##  the frames that are added are 1:4, 2:5, 3:6, 4:7, 5:8, 
  ## 6:9, 7:10.
  m <- nframes - 1
  mat <- NULL
  for(k in 1:(length(vec) - m)) {
    mat <- c(mat, sum(vec[k:(k + m)]))
  }
  mat
}


"steady.plot"<- function(values, vec, labs = NULL, ylim = NULL,
                         col = 1, main = "")
{
  ## values - the ouput from Steady
  tlen <- length(vec)
  if(tlen > 20) {
    print("maximum of 20 segments allowed")
    break()
  }
  else if(tlen <= 5)
    pict <- c(1, 5)
  else if((tlen > 5) & (tlen <= 10))
    pict <- c(2, 5)
  else if((tlen > 10) & (tlen <= 15))
    pict <- c(3, 5)
  else pict <- c(4, 5)
  par(mfrow = pict)
  decum <- values$index[, 1] - 1
  decum <- cbind(decum, decum)
  values$steady <- values$steady - decum
  values$index <- values$index[vec,  ]
  values$index <- rbind(values$index)
  values$steady <- values$steady[vec,  ]
  values$steady <- rbind(values$steady)
  for(j in 1:nrow(values$index)) {
    leftid <- values$index[j, 1]
    rightid <- values$index[j, 2]
    data <- values$data[leftid:rightid,  ]
    if(any(ylim) == F)
      ylim <- range(data)
    notap <- rep(NA, ncol(data))
    newdata <- rbind(data, notap)
    datay <- c(newdata)
    nums <- c(1:nrow(data))
    nums <- c(nums, NA)
    datax <- rep(nums, ncol(data))
    datay1 <- datay[.Uminus(length(datay))]
    datax1 <- datax[.Uminus(length(datax))]
    plot(datax1, datay1, type = "l", xlab = "Number of frames", 
         ylab = "Frequency (Hz)", ylim = ylim, col = col, main
         = main)
    abline(v = values$steady[j, 1], lty = 2)
    abline(v = values$steady[j, 2], lty = 2)
    if(any(labs)) {
      mtext(labs[j], side = 3, line = 1, at = mean(c(values$
                                           index[j, 1], values$index[j, 2])))
    }
  }
  invisible()
}


"steady.time"<- function(ind, vals)
{
  ## calculates the time corresponding to ind1, based
  ## on vals$index and vals$ftime 
  ## inds must be the same length as nrow(vals$index)
  leftind <- vals$index[, 1]
  rightind <- vals$index[, 2]
  rangeind <- rightind - leftind
  left.time <- vals$ftime[, 1]
  right.time <- vals$ftime[, 2]
  range.time <- right.time - left.time
  (((ind - leftind)/rangeind) * (range.time)) + left.time
}



# Local Variables:
# mode:S
# S-temp-buffer-p:t
# End:
