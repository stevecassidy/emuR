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


"dplot" <- function(dataset, labs = NULL, offset = 0, ref.time = NULL,
                    average = F, main = "", xlab = "time (ms)",
                    ylab = "", xlim = NULL, ylim = NULL, 
                    cex = 0.5, linetype = F, normalise = F, colour = T,
                    legn = "tl", axes = T, amp = F)
{
  ## this function is used to plot multiple signal files on the same plot, 
  ## i.e. when the $data object returned from track has more than one 
  ## dimension
  if(amp) {
    timevals <- dtime(dataset, ref.time)
    dataset <- dshift(dataset, timevals)
  }
  if(is.logical(legn)) {
    if(!legn)
      legn <- ""
  }
  if(is.matrix(dataset$data)) {
    mat <- NULL
    if(is.null(ylim))
      ylim <- range(dataset$data)
    numcols <- ncol(dataset$data)
    main <- c(rep("", numcols - 1), main)
    xlab <- c(rep("", numcols - 1), xlab)
    ylab <- c(rep("", numcols - 1), ylab)
    legn <- c(rep("", numcols - 1), legn)
    axes <- c(rep(F, numcols - 1), axes)
    for(j in 1:ncol(dataset$data)) {
      mat <- dataset
      mat$data <- mat$data[, j]
      prmat <- dplot.sub(mat, labs = labs, offset = offset, 
			 ref.time = ref.time, average = average, main = 
			 main[j], xlab = xlab[j], ylab = ylab[j], xlim
			 = xlim, ylim = ylim, cex = cex, linetype = 
			 linetype, normalise = normalise, colour = 
			 colour, legn = legn[j], axes = axes[j])
      par(new = T)
    }
  } else {
    prmat <- dplot.sub(dataset, labs = labs, offset = offset, 
		       ref.time = ref.time, average = average, main = main, 
		       xlab = xlab, ylab = ylab, xlim = xlim, ylim = ylim, 
		       cex = cex, linetype = linetype, normalise = normalise, 
		       colour = colour, legn = legn, axes = axes)
  }
  par(new = F)
  invisible(prmat)
}


"dplot.sub"<-  function(dataset, labs = NULL, offset = 0, ref.time = NULL, 
			average = F, main = "", xlab = "time (ms)", ylab = "", 
			xlim = NULL, ylim = NULL, 
			cex = 0.5, linetype = F, normalise = F, colour = T, 
			legn = "tl", axes = T )
{
  ## dataset: as returned from track()
  ## labs: parallel labels
  ## a reference time against which the synchronisation is to be done
  ## average: should averaging be done?
  ## dplot.sub() can only be applied to single parameter tracks i.e.
  ## $data is one-dimensional
  prmat1 <- NULL
  if(is.null(labs)) legn <- ""

  ## find sample rate, assuming times in milliseconds
  samrate <- 1000 * (dataset$index[1, 2] - dataset$index[1, 1]) /
    (dataset$ftime[1, 2] - dataset$ftime[1, 1]) 


  if((offset < 0) | (offset > 1))
    stop("offset must be between 0 and 1")

  if(is.null(labs))
    labs <- rep(1, nrow(dataset$index))

  col.lty <- mu.colour(labs, colour, linetype)
  colour <- col.lty$colour
  linetype <- col.lty$linetype

  if(normalise) {
    normvals <- linear(dataset)
    dataset$data <- normvals$data
    dataset$index <- normvals$index
  }

  if(is.null(ref.time))
    ref.time <- dataset$ftime[, 1] + 
      ((dataset$ftime[, 2] - dataset$ftime[, 1]) * offset)

  maxlen <- 2 * (max(dataset$index[, 2] - dataset$index[, 1] + 1))
  pointval <<- round(maxlen/2)
  mat.na <- matrix(NA, nrow(dataset$index), maxlen)

  for(j in 1:nrow(dataset$index)) {
    left <- dataset$index[j, 1]
    right <- dataset$index[j, 2]
    length.index <- right - left + 1
    times <- dataset$ftime[j,  ]
    refn <- ref.time[j]
    inval <- closest(seq(times[1], times[2], length = length.index), refn)
    inval <- inval[1]
    left.na <- pointval - inval + 1
    right.na <- left.na + length.index - 1
    mat.na[j, left.na:right.na] <- dataset$data[left:right]
  }

  z <- apply(mat.na, 2, mean, na.rm = T)
  natemp <- is.na(z)
  nums <- c(1:length(natemp))
  nonums <- nums[!natemp]
  if(is.null(xlim))
    xlim <- c(nonums[1], nonums[length(nonums)])
    else {
      interval <- 1000/samrate
      xlim <- c(pointval + xlim[1]/interval, pointval + xlim[2]/interval)
    }
  if(is.null(ylim))
    ylim <- range(mat.na, na.rm = T)
  left.tick <- xlim[1] - pointval
  right.tick <- xlim[2] - pointval

  if(!average) {
    for(j in 1:nrow(mat.na)) {
      plot(mat.na[j,  ], 
	   xlim = xlim, ylim = ylim, 
	   xlab = "", ylab = "", 
	   axes = F, 
	   type = "l", 
	   col = colour[j], lty = linetype[j])
      par(new = T)
    }
    prmat1$vals <- mat.na
  } else { 
    ## we average
    if(!is.null(labs)) {
      outmat <- NULL
      outlabs <- NULL
      for(j in unique(labs)) {
	temp <- labs == j
	vals <- mat.na[temp,  ]
	mvals <- apply(vals, 2, mean, na.rm = T)
	outmat <- rbind(outmat, mvals)
	outlabs <- c(outlabs, j)
      }
    } else {
      outmat <- apply(mat.na, 2, mean, na.rm = T)
      outmat <- rbind(outmat)
      outlabs <- 1
    }

    col.code <- match(col.lty$legend$lab, unique(labs))
    colour <- col.lty$legend$col
    linetype <- col.lty$legend$lty

    for(j in 1:nrow(outmat)) {
      plot(outmat[j,  ], xlim = xlim, ylim = ylim, xlab = "", ylab = "", axes = F, 
	   type = "l", col = colour[col.code[j]], lty = linetype[col.code[j]])
      par(new = T)
    }
    prmat1$vals <- outmat
  }

  if(axes) {
    if(!normalise)
      axis(side = 1, 
	   at = seq(xlim[1], xlim[2], by = round(( xlim[2] - xlim[1])/10)), 
	   labels = (( seq(left.tick, 
			   right.tick, 
			   by = round((right.tick - left.tick)/10))
		      * 1000)
		     / samrate),
	   cex = cex)
      else {
	axis(side = 1, at = seq(xlim[1], xlim[2], length = 11), 
	     labels = seq(0, 1, length = 11), cex = cex)
	xlab <- "normalised time"
      }
    axis(side = 2, cex = cex)
    box()
  }

  title(main = main, xlab = xlab, ylab = ylab, cex = cex)

  if(legn != "") {
    legn <- mu.legend(legn, xlim, ylim)
    legend(legn$x, legn$y, 
	   col.lty$legend$lab, 
	   col = col.lty$legend$col, 
	   lty = col.lty$legend$lty, 
	   cex = cex)
  }

  prmat1$ref <- pointval
  invisible(prmat1)
}


# Local Variables:
# mode:S
# S-temp-buffer-p:t
# End:
