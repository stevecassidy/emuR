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

##########################################################################
"eplot"<- function(data, labs, chars, formant = F, scaling = "linear",
                   nsdev = 2.4477470000000001, dopoints = F, classify = F,
                   doellipse = T, centroid = F, main = "",
                   xlab = "", ylab = "", axes = T, 
                   xlim, ylim, colour = T, linetype = F, ...)
{
  ## written by Jonathan Harrington, 12.12.91
  ## modified by Steve Cassidy 18.3.94
  if(missing(labs)) labs <- rep(".", nrow(data))	
  ## if we don't want ellipses and we don't want points we should 
  ## at least have centroids
  if(!doellipse & !dopoints)
    centroid <- T
  if(nrow(data) != length(labs))
    stop("Data and labels don't match")
  if(ncol(data) != 2)
    stop("Eplot needs 2 dimensional data")
  if(!missing(chars))
    if(length(labs) != length(chars))
      stop("Length of chars must match that of labs")
  if(scaling == "mel")
    data <- mel(data)
  if(scaling == "bark")
    data <- bark(data)
  if(formant)
    data <- cbind(-data[, 2], -data[, 1])
  col.lty <- mu.colour(labs, colour, linetype)
  uniqlabels <- unique(labs)
  emat <- nums <- cen <- k <- l <- NULL

  for(j in uniqlabels) {
    temp <- labs == j
    mat <- data[temp, ,drop=F]
    if( nrow(mat) > 2 ){
      ## use eigen rather than prcomp for portability to R
      evals <- eigen(var(mat))
      m1 <- mean(mat[, 1])
      m2 <- mean(mat[, 2])
      e <- ellipse(m1, m2,
		   sqrt(evals$values[1]) * nsdev,
		   sqrt(evals$values[2]) * nsdev, 
		   aperm(evals$vectors, c(2, 1)))
    
    } else {
      cat("Too few data points for label ", j, " will plot a point or a line\n" )
      m1 <- mean(mat[, 1])
      m2 <- mean(mat[, 2])
      e <- mat  # a line or a point depending on how many rows in mat
    }
    nums <- c(nums, nrow(e))
    emat <- rbind(emat, e)
    # find the right colour and linetype for these points
    # from the colour object calculated earlier
    k <- c(k, mu.colour.get(col.lty, j))
    l <- c(l, mu.linetype.get(col.lty, j))

    if(centroid) cen <- rbind(cen, cbind(m1, m2))
  }

  if(doellipse) {
    if(missing(xlim))
      xlim <- range(c(emat[, 1], data[, 1]))
    if(missing(ylim))
      ylim <- range(c(emat[, 2], data[, 2]))
  } else {
    if(missing(xlim))
      xlim <- range(data[, 1])
    if(missing(ylim))
      ylim <- range(data[, 2])
  }

  rightlim <- cumsum(nums)
  leftlim <- cumsum(nums) - (nums - 1)
  rowmarker <- cbind(leftlim, rightlim)	

  ## Pass all of the extra args to par
  old.par <- par()	
  ##  on.exit(par(old.par))
  par(...)

  for(j in 1:nrow(rowmarker)) {
    lowerlim <- rowmarker[j, 1]
    upperlim <- rowmarker[j, 2]
    if(doellipse) {
      plot(emat[lowerlim:upperlim,  ], type = "l", axes = F, 
	   xlim = xlim, ylim = ylim, 
	   col = k[j], xlab = "", ylab = "", lty = as.integer(l[j]))
    } else {
      plot(emat[lowerlim:upperlim,  ], type = "n", axes = F, 
	   xlim = xlim, ylim = ylim, col = k[j], xlab
	   = "", ylab = "", lty = as.integer(l[j]))
    }
    if(dopoints) {
      singlelab <- uniqlabels[j]
      temp <- labs == singlelab
      if(!missing(chars))
	text(data[temp, 1], data[temp, 2], chars[temp], 
	     col = k[j])
      else text(data[temp, 1], data[temp, 2], labs[temp], col
		= k[j])
    }
    if(centroid) {
      singlelab <- uniqlabels[j]
      text(cen[j, 1], cen[j, 2], singlelab, col = k[j])
    }
    if(j < nrow(rowmarker))
      par(new = T)	# don't do this on the last iteration
  }
  par(col = 1)
  box()
  if(axes) {
    if(formant) {
      if(missing(xlab))
	xlab <- "F2"
      if(missing(ylab))
	ylab <- "F1"
      xaxp <- par("xaxp")
      yaxp <- par("yaxp")
      xat <- seq(xaxp[1], xaxp[2], length.out = xaxp[3] + 1)
      yat <- seq(yaxp[1], yaxp[2], length.out = yaxp[3] + 1)
      axis(1, at = xat, label =  - xat)
      axis(2, at = yat, label =  - yat, srt = 90)
    }
    else {
      axis(1)
      axis(2)
    }
  }
  title(main = main, xlab = xlab, ylab = ylab)
  if(classify) {
    tdat <- train(data, labs)
    mdat <- mahal(data, tdat)
    outlab <- sortmatrix(confusion(labs, mdat), unique(labs))
  }
  if(classify)
    outlab
}

"ellipse"<- function(x, y, rx, ry, orient, incr = 360/100)
{
	rincr <- radians(incr)
	theta <- seq(0, 2 * pi, rincr)
	xcoord <- rx * cos(theta)
	ycoord <- ry * sin(theta)
	mat <- cbind(xcoord, ycoord)
	mat <- mat %*% orient
	mat[, 1] <- mat[, 1] + x
	mat[, 2] <- mat[, 2] + y
	mat
}

"polygonplot" <- function(data, labels, order,
                          formant=T, axes=T,
                          xlab="", ylab="",
                          main = "", xlim, ylim)
{

  if( ncol(data) > 2 ) {
    data <- data[,1:2]
  }
  if( ncol(data) != 2 ) {
    error( "polygonplot() requires two columns of data" )
  }

  if(formant)
    data <- cbind(-data[, 2], -data[, 1])


  points <- NULL
  for( l in order ) {
    tmp <- matrix(data[labels==l],ncol=2)
    points <- rbind( points, apply(tmp, 2, mean) )
  }

  plot( points, type="b", pch=" ", axes=F, xlab="", ylab="" )
  text( points, order, axes=F, , xlab="", ylab="" )

  par(col = 1)
  box()
  if(axes) {
    if(formant) {
      if(missing(xlab))
	xlab <- "F2"
      if(missing(ylab))
	ylab <- "F1"
      xaxp <- par("xaxp")
      yaxp <- par("yaxp")
      xat <- seq(xaxp[1], xaxp[2], length.out = xaxp[3] + 1)
      yat <- seq(yaxp[1], yaxp[2], length.out = yaxp[3] + 1)
      axis(1, at = xat, label =  - xat)
      axis(2, at = yat, label =  - yat, srt = 90)
    }
    else {
      axis(1)
      axis(2)
    }
  }
  title(main = main, xlab = xlab, ylab = ylab)
}


# Local Variables:
# mode:S
# S-temp-buffer-p:t
# End:
