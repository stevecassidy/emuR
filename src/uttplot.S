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


"uttplot"<- function(segs, param, data, labtype, colour=T, linetype=F,
                     defcolour = 1, xlim = NULL, ylim = NULL,
                     xlab = "time (ms)",  ylab = "", 
                     main = "", zeros = F, masktrack = "prob_voice",
                     pdat = 0.5, labcol=7,
                     labels = NULL, labeltimes = NULL, labcentre = F,
                     dolabels=T, differ = NULL, smoothing = F, flipped = F, 
                     axes = T, type = "l", ...)
{
  ## written by Jonathan Harrington, 18.9.92
  uttid <- unique(utt(segs))

  nsegs <- nrow(segs)

  values <- emu.track(segs, param)

  if(!missing(data)) values$data <- data
  if(!is.null(differ)) {
    values <- ddiff(values, differ, smoothing=F)
    smoothing <- T
  }
  if(!is.matrix(values$data)) values$data <- cbind(values$data)
  if(smoothing) values$data <- apply(values$data, 2, smooth)
  if(flipped) values$data <- .Uminus(values$data)

  left.time <- values$ftime[1, 1]
  right.time <- values$ftime[nsegs, 2]

  ## colours for label boundaries in white if colour is T
  if(missing(labcol)) {
    if(!colour)
      labcol <- 1
    else
      labcol <- 7
  }

  timevals <- seq(left.time, right.time, length = nrow(values$data))
  if(zeros) {
    pvoice <- emu.track(segs, masktrack)
    temp <- pvoice$data < pdat
    # sometimes prob_voice isn't of the same length of the data, usually only 
    # by one point so we just truncate it if needs be
    if( length(temp) > length(values$data[,1]) )
      temp <- temp[1:(length(values$data[,1]))]
    values$data[!temp,  ] <- NA
  }

  boundtimes <- c(mustart(segs), muend(segs[nrow(segs),  ]))
  if(is.null(xlim)) xlim <- range(c(boundtimes, timevals))
  if(is.null(ylim)) ylim <- range(values$data, na.rm = T)
  
  ## save old par settings to restore later
  oldpar <- par()
#  on.exit(par(oldpar))
  par(...)
  
  col.lty <- mu.colour(1: ncol(values$data), colour, linetype, defcolour)
  if(colour) col.lty$colour <- col.lty$colour+1
  colour <- col.lty$colour
  lty <- col.lty$linetype

  for(j in 1:ncol(values$data)) {
    plot(timevals, values$data[, j], ylim = ylim, xlim = xlim, 
	 xlab = "", ylab = "", main = "", axes = F, col = colour[j], 
	 type = type, lty=lty[j])
        par(new = T)
  }
  if(!missing(labtype)) {
    ## might not work -- need emu.requery
    lefttime <- mustart(segs[1,  ])
    righttime <- muend(segs[nrow(segs),  ])
    segs <- emu.requery( segs, labtype )
    startvals <- mustart(segs)
    endvals <- muend(segs)
    temp <- (startvals < lefttime) | (endvals > righttime)
    segs <- segs[!temp,  ]
  }
  if(!is.null(labeltimes)) {
    ## draw the labels given 
    uttplot.drawlabels( labels, labeltimes, boundtimes, colour=labcol,
                       line=1, dolabels=dolabels  )
  }
  if(!is.null(segs)) {
    ## draw the labelsn from the segment list
    uttplot.drawsegs( segs, colour=labcol, line=1, dolabels=dolabels )
  }
  if(axes){
    axis(1)
    axis(2)
    box()
  }
  title(main = main, ylab = ylab, xlab = xlab)
}


uttplot.drawsegs <- function(segs, colour=1, dolabels=T, line=0) {

  ## work out where to draw the lines
  if( attributes(segs)$type == "event" ) {
    labeltimes <- mustart(segs)
    boundtimes <- labeltimes
  } else {
    labeltimes <- (mustart(segs) + muend(segs))/2
    boundtimes <- c(mustart(segs[1,]), muend(segs))
  }

  uttplot.drawlabels( label(segs), labeltimes, boundtimes, colour,
                     dolabels, line )
}



uttplot.drawlabels <- function( labels, labeltimes, boundtimes,
                               colour=1, dolabels=T, line=0) {
  abline(v = boundtimes, lty = 2, col = colour)
  if(dolabels) {
    for( i in 1:length(labels)  ) {
      mtext(labels[i], side = 3, line = line, at = labeltimes[i], col = colour)
    } 
  } 
}


# Local Variables:
# mode:S
# S-temp-buffer-p:t
# End:
