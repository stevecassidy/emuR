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


"dplot"<-
function(dataset, labs = NULL, offset = 0, ref.time = NULL,
         average = F, main = "", xlab = "time (ms)", ylab = "",
         xlim = NULL, ylim = NULL, cex = 0.5, linetype = F,
         normalise = F, colour = T, legn = "tl", axes = T, 
         amp = F, n = 20)
{
  pout <- NULL

  if(amp) {
    if( is.null(ref.time) )
      stop( "ref.time must be supplied if amp=T" )
    timevals <- dtime(dataset, ref.time)
    dataset <- dshift(dataset, timevals)
  }

  if(is.logical(legn)) {
    if(!legn) legn <- ""
  }

  if(is.matrix(dataset$data)) {
    pout <- as.list(NULL)
    pout$data <- as.list(NULL)
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
      if(!normalise)
        vals <- dplot.time(mat, labs = labs, offset = offset,
                           ref.time = ref.time, average = average,
                           main = main[j], xlab = xlab[j], ylab = ylab[j],
                           xlim = xlim, ylim = ylim, cex = cex,
                           linetype = linetype, colour = colour, 
                           legn = legn[j], axes = axes[j])
      else
        vals <- dplot.norm(mat, labs = labs, average = average,
                           main = main[j], xlab = xlab[j], ylab = ylab[j],
                           xlim = xlim, ylim = ylim, cex = cex,
                           linetype = linetype, colour = colour,
                           legn = legn[j], axes = axes[j], n = n)
      
      par(new = T)
      pout$data[[j]] <- vals$data
      if(j == ncol(dataset$data)) {
        pout$time <- vals$time
        pout$labs <- vals$labs
      }
    }
  } else {
    if(!normalise)
      pout <- dplot.time(dataset, labs = labs, offset = offset,
                         ref.time = ref.time, average = average, 
                         main = main, xlab = xlab, ylab = ylab,
                         xlim = xlim, ylim = ylim, cex = cex,
                         linetype = linetype, colour = colour, legn = legn,
                         axes = axes)
    else
      pout <- dplot.norm(dataset, labs = labs, average = average,
                         main = main, xlab = xlab, ylab = ylab,
                         xlim = xlim, ylim = ylim, cex = cex,
                         linetype = linetype, colour = colour,
                         legn = legn, axes = axes, n = n)
  }
  par(new = F)
  invisible(pout)
}

"dplot.time"<- function(dataset, labs = NULL, offset = 0, ref.time = NULL,
                        average = F, main = "", xlab = "time (ms)", ylab = "",
                        xlim = NULL, ylim = NULL, cex = 0.5,
                        linetype = F, colour = T, legn = "tl",
                        axes = T)
{
  ## dataset: as returned from track()
  ## labs: parallel labels
  ## a reference time against which the synchronisation is to be done
  ## average: should averaging be done?
  ## dplot.sub() can only be applied to single parameter tracks i.e.
  ## $data is one-dimensional
  ## ovec: a list for storing the values if printmatrix is T
  ovec <- as.list(NULL)
  if(is.null(labs))
    legn <- ""

  samrate <- 1000/((dataset$ftime[1, 2] - dataset$ftime[1, 1])/
                   (dataset$index[1, 2] - dataset$index[1, 1]))

  if((offset < 0) | (offset > 1))
    stop("offset must be between 0 and 1")

  if(is.null(labs))
    labs <- rep(1, nrow(dataset$index))

  col.lty <- mu.colour(labs, colour, linetype)
  colour <- col.lty$colour
  linetype <- col.lty$linetype

  if(is.null(ref.time))
    ref.time <- dataset$ftime[, 1] +
                ((dataset$ftime[, 2] - dataset$ftime[, 1]) * offset)

  maxlen <- 2 * (max(dataset$index[, 2] - dataset$index[, 1] + 1))
  pointval <- round(maxlen/2)
  mat.na <- matrix(NA, nrow(dataset$index), maxlen)

  for(j in 1:nrow(dataset$index)) {
    left <- dataset$index[j, 1]
    right <- dataset$index[j, 2]
    length.index <- right - left + 1
    times <- dataset$ftime[j,  ]
    refn <- ref.time[j]
    inval <- closest(seq(times[1], times[2], length = length.index),
                     refn)
    inval <- inval[1]
    left.na <- pointval - inval + 1
    right.na <- left.na + length.index - 1
    mat.na[j, left.na:right.na] <- dataset$data[left:right]
  }

  z <- apply(mat.na, 2, mean, na.rm = T)
  natemp <- is.na(z)
  nums <- c(1:length(natemp))
  nonums <- nums[!natemp]
  interval <- 1000/samrate

  if(is.null(xlim))
    xlim <- c(nonums[1], nonums[length(nonums)])
  else xlim <- c(pointval + xlim[1]/interval, pointval + xlim[2]/interval
                 )
  time1 <- (1 - pointval) * interval
  time2 <- (ncol(mat.na) - pointval) * interval
  xtime <- seq(time1, time2, length = ncol(mat.na))
  xtimelim <- (xlim - pointval) * interval

  if(is.null(ylim))
    ylim <- range(mat.na, na.rm = T)

  if(!average) {
    for(j in 1:nrow(mat.na)) {
      plot(xtime, mat.na[j,  ], xlim = xtimelim, ylim = ylim, 
           xlab = "", ylab = "", axes = F, type = "l", col
           = colour[j], lty = linetype[j])
      par(new = T)
    }
    ovec$data <- mat.na
    ovec$time <- xtime
    ovec$labs <- labs
  }
  else {
    if(!is.null(labs)) {
      outmat <- NULL
      outlabs <- NULL
      for(j in unique(labs)) {
        temp <- labs == j
        vals <- mat.na[temp,  ]
        ## take the mean only if there are more than one
        if( is.matrix(vals) ) {
          mvals <- apply(vals, 2, mean, na.rm = T)
        } else {
          mvals <- vals
        }
        outmat <- rbind(outmat, mvals)
        outlabs <- c(outlabs, j)
      }
    }
    else {
      outmat <- apply(mat.na, 2, mean, na.rm = T)
      outmat <- rbind(outmat)
      outlabs <- 1
    }
    col.code <- match(col.lty$legend$lab, unique(labs))
    colour <- col.lty$legend$col
    linetype <- col.lty$legend$lty
    for(j in 1:nrow(outmat)) {
      plot(xtime, outmat[j,  ], xlim = xtimelim, ylim = ylim, 
           xlab = "", ylab = "", axes = F, type = "l", col
           = colour[col.code[j]], lty = linetype[col.code[
                                    j]])
      par(new = T)
    }
    ovec$data <- outmat
    ovec$time <- xtime
    ovec$labs <- outlabs
  }

  if(axes) {
    axis(side = 1, cex = cex)
    axis(side = 2, cex = cex)
    box()
  }
  title(main = main, xlab = xlab, ylab = ylab, cex = cex)

  if(legn != "") {
    legn <- mu.legend(legn, xlim, ylim)
    legend(legn$x, legn$y, col.lty$legend$lab, col = col.lty$legend$
           col, lty = col.lty$legend$lty, cex = cex)
  }

  invisible(ovec)
}



"dplot.norm"<- function(dataset, labs = NULL, average = F,
                        main = "", xlab = "time (ms)", ylab = "",
                        xlim = NULL, ylim = NULL, cex = 0.5,
                        linetype = F, colour = T,
                        legn = "tl", axes = T, n = 20)
{
  ovec <- NULL
  if(is.null(ylim))
    ylim <- range(dataset$data)
  if(is.null(xlim))
    xlim <- c(0, 1)
  if(is.null(labs)) {
    legn <- ""
    labs <- rep(1, nrow(dataset$index))
  }

  col.lty <- mu.colour(labs, colour, linetype)
  colour <- col.lty$colour
  linetype <- col.lty$linetype

  mat.na <- linear(dataset, n)
  mat.na$ftime <- dataset$ftime
  class(mat.na) <- "trackdata"

  xvec <- seq(0, 1, length = n)
  lval <- nrow(dataset$index)

  if(!average) {
    for(j in 1:lval) {
      plot(xvec, mat.na[j]$data, xlim = xlim, ylim = ylim, 
           xlab = "", ylab = "", axes = F, type = "l", col
           = colour[j], lty = linetype[j])
      par(new = T)
    }
    ovec$data <- mat.na
    ovec$time <- xvec
    ovec$labs <- labs
  }
  else {
    if(!is.null(labs)) {
      outmat <- NULL
      outlabs <- NULL
      for(j in unique(labs)) {
        temp <- labs == j
        vals <- mat.na[temp]$data
        vals <- matrix(vals, ncol = n, byrow = T)
        ## take the mean only if there are more than one
        if( is.matrix(vals) ) {
          mvals <- apply(vals, 2, mean)
        } else {
          mvals <- vals
        }
        outmat <- rbind(outmat, mvals)
        outlabs <- c(outlabs, j)
      }
    } else {
      outmat <- apply(matrix(mat.na, ncol = 20, byrow = T), 2,
                      mean)
      outmat <- rbind(outmat)
      outlabs <- 1
    }
    col.code <- match(col.lty$legend$lab, unique(labs))
    colour <- col.lty$legend$col
    linetype <- col.lty$legend$lty
    for(j in 1:nrow(outmat)) {
      plot(xvec, outmat[j,  ], xlim = xlim, ylim = ylim, xlab
           = "", ylab = "", axes = F, type = "l", col = 
           colour[col.code[j]], lty = linetype[col.code[j]
                                  ])
      par(new = T)
    }
    ovec$data <- outmat
    ovec$time <- xvec
    ovec$labs <- labs
  }
  if(axes) {
    axis(side = 1, cex = cex)
    axis(side = 2, cex = cex)
    box()
  }
  
  title(main = main, xlab = xlab, ylab = ylab, cex = cex)

  if(legn != "") {
    legn <- mu.legend(legn, xlim, ylim)
    legend(legn$x, legn$y, col.lty$legend$lab, col = col.lty$legend$
           col, lty = col.lty$legend$lty, cex = cex)
  }
  invisible(ovec)
}
