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



"segplot"<- function(segs, param, labels, ylim, data, ylab = "",
                     main = "", zeros = F, pdat = 0.5,
                     onset = 0, offset = 0, colour = T,
                     linetype=F, smoothing = F, mfrow, addlines=NULL,
                     differ=NULL)
{
  ## written by Jonathan Harrington, 1992
  if(!is.null(addlines)){
    if(is.matrix(addlines))
      lval.a _ nrow(addlines)
    else
      lval.a _ length(addlines)
    if(is.matrix(segs))
      lval.s _ nrow(segs)
    else
      lval.s _ 1
    if(lval.a!=lval.s)
      stop("number of entries in the segment list and addlines do not match")
  }
#  oldpar_par()
#  on.exit(par(oldpar))
  oldsegs <- segs
  if(missing(labels)) labels <- label(segs)
  if(!is.matrix(oldsegs)) oldsegs <- rbind(oldsegs)
  if(missing(data)) {
    ## adjust the segment times to include onset and offset
    ## this could maybe an option in emu.track since we can't ensure 
    ## here that there is real data at these times
    segs <- make.seglist(labels,
                         mustart(segs) - onset, 
                         muend(segs) + offset, 
                         utt(segs),
                         emusegs.query(segs),
                         emusegs.type(segs),
                         emusegs.database(segs))
    
    values <- emu.track(segs, param)
  } else {
    values <- data
  }
  
  if(!is.null(differ))
    values _ ddiff(values, differ)
  if(smoothing)
    values _ dsmooth(values)
  if(!is.matrix(values$data)) values$data <- cbind(values$data)

  if(zeros) {
    ## this assumes that there is a prob_voice track, eg from ESPS
    pvoice <- emu.track(segs, "prob_voice")$data
    temp <- pvoice > pdat
    values$data[!temp,  ] <- NA
  }

  if(missing(ylim)) ylim <- range(values$data, na.rm = T)

  ## work out a sensible mfrow default
  if(missing(mfrow)){
    nsegs _ nrow(segs)
    foo _ ceiling(sqrt(nsegs))
    bar _ ceiling(nsegs/foo)
    mfrow _ c(foo, bar)
    
  }
  par(mfrow = mfrow)
  if(!colour)
    labcol _ 1
  else
    labcol _ 7
  tempf _ c(1: ncol(values$data))
  col.lty _ mu.colour(tempf, colour, linetype)
  if(colour)
    col.lty$colour _ col.lty$colour+1
  colour _ col.lty$colour
  lty _ col.lty$linetype

  for(j in 1:nrow(segs)) {
    midtimes <- (mustart(oldsegs[j,  ]) + muend(oldsegs[j,  ]))/2
    boundtimes <- c(mustart(oldsegs[j,  ]), muend(oldsegs[j,  ]))
    plab <- labels[j]
    if(is.null(plab)) plab <- ""
    index.left <- values$index[j, 1]
    index.right <- values$index[j, 2]
    data <- values$data[index.left:index.right,  ]
    if(!is.matrix(data)) data <- cbind(data)
    left.time <- values$ftime[j, 1]
    right.time <- values$ftime[j, 2]
    timevals <- seq(left.time, right.time, length = nrow(data))
    rtimes <- range(c(boundtimes, timevals))

    for(k in 1:ncol(data)) {
      plot(timevals, data[, k], xlim = rtimes, ylim = ylim, 
	   xlab = "", ylab = "", main = "", xaxt = "n", yaxt="n",
	   col=colour[k], lty=lty[k], type = "l")
      par(new = T)
    }
    abline(v = boundtimes, lty = 2, col = labcol)
    if(!is.null(addlines)){
      if(!is.matrix(addlines))
	addlines _ cbind(addlines)
      for(i in 1:ncol(addlines)){
	abline(v=addlines[j,i], col=labcol)
      }}
    mtext(plab, side = 3, line = 1, at = midtimes, col = labcol)
    title(main = main, ylab = ylab, xlab = "time (ms)", xaxt = "s", yaxt="s")
    par(new = F )
    
  }
  par(mfrow=c(1,1))
  invisible()
}


## Local Variables:
## mode:S
## S-temp-buffer-p:t
## End:
