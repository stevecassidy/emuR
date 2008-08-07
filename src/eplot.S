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
`eplot` <-
function (x, labs, chars, formant = FALSE, scaling = "linear", 
    prob = 0.95, nsdev = NULL, dopoints = FALSE, doellipse = TRUE, 
    centroid = FALSE,  axes = TRUE, 
    xlim, ylim, col = TRUE, lty = FALSE,  
    lwd = NULL, ...) 
{
    ocall <- match.call()
    if (is.null(nsdev)) 
        nsdev <- sqrt(qchisq(prob, 2))
    
    if (missing(labs)) 
        labs <- rep(".", nrow(x))
    if (!doellipse & !dopoints) 
        centroid <- TRUE
    if (nrow(x) != length(labs)) 
        stop("x and labels don't match")
    if (ncol(x) != 2) 
        stop("Eplot needs 2 dimensional x")
    if (!missing(chars)) 
        if (length(labs) != length(chars)) 
            stop("Length of chars must match that of labs")
    if (scaling == "mel") 
        x <- mel(x)
    if (scaling == "bark") 
        x <- bark(x)
    if (formant) {
        x <- cbind(-x[, 2], -x[, 1])
        if (!missing(xlim)) 
            xlim <- -rev(xlim)
        if (!missing(ylim)) 
            ylim <- -rev(ylim)
    }
    col.lty <- mu.colour(labs, col, lty, lwd)
    lty <- col.lty$linetype
    linewidth <- col.lty$lwd
    uniqlabels <- unique(labs)
    emat <- nums <- cen <- k <- l <- NULL
    for (j in uniqlabels) {
        temp <- labs == j
        mat <- x[temp, , drop = FALSE]
        if (nrow(mat) > 2) {
            evals <- eigen(var(mat))
            m1 <- mean(mat[, 1])
            m2 <- mean(mat[, 2])
            e <- ellipse(m1, m2, sqrt(evals$values[1]) * nsdev, 
                sqrt(evals$values[2]) * nsdev, aperm(evals$vectors, 
                  c(2, 1)))
        }
        else {
            cat("Too few x points for label ", j, " will plot a point or a line\n")
            m1 <- mean(mat[, 1])
            m2 <- mean(mat[, 2])
            e <- mat
        }
        nums <- c(nums, nrow(e))
        emat <- rbind(emat, e)
        k <- c(k, col.lty$legend$col[match(j, col.lty$legend$lab)])
        l <- c(l, col.lty$legend$lty[match(j, col.lty$legend$lab)])
        linewidth <- c(linewidth, col.lty$legend$lwd[match(j, 
            col.lty$legend$lab)])
        if (centroid) 
            cen <- rbind(cen, cbind(m1, m2))
    }
    if (doellipse) {
        if (missing(xlim)) 
            xlim <- range(c(emat[, 1], x[, 1]))
        if (missing(ylim)) 
            ylim <- range(c(emat[, 2], x[, 2]))
    }
    else {
        if (missing(xlim)) 
            xlim <- range(x[, 1])
        if (missing(ylim)) 
            ylim <- range(x[, 2])
    }
    rightlim <- cumsum(nums)
    leftlim <- cumsum(nums) - (nums - 1)
    rowmarker <- cbind(leftlim, rightlim)
    for (j in 1:nrow(rowmarker)) {
        lowerlim <- rowmarker[j, 1]
        upperlim <- rowmarker[j, 2]
        if (doellipse) {
            plot(emat[lowerlim:upperlim, ], type = "l", axes = FALSE, 
                xlim = xlim, ylim = ylim, col = k[j], 
               lty = as.numeric(l[j]), lwd = as.numeric(linewidth[j]), xlab="", ylab="", main="")
        }
        else {
            plot(emat[lowerlim:upperlim, ], type = "n", axes = FALSE, 
                xlim = xlim, ylim = ylim, col = k[j], 
              lty = as.numeric(l[j]), lwd = as.numeric(linewidth[j]), xlab="", ylab="", main="")
        }
        if (dopoints) {
            centroid <- FALSE
            singlelab <- uniqlabels[j]
            temp <- labs == singlelab
            if (!missing(chars)) 
			{
			if(is.numeric(chars))
                points(x[temp, 1], x[temp, 2], pch=chars[temp], 
                  col = k[j])
				  else
				  text(x[temp, 1], x[temp, 2], chars[temp], 
                  col = k[j])
				  }
            else text(x[temp, 1], x[temp, 2], labs[temp], 
                col = k[j])
        }
        if (centroid) {
            singlelab <- uniqlabels[j]
            text(cen[j, 1], cen[j, 2], singlelab, col = k[j])
        }
        if (j < nrow(rowmarker)) 
            par(new = TRUE)
    }
    par(col = 1)
    
    if (axes) {
        if (formant) {
            xaxp <- par("xaxp")
            yaxp <- par("yaxp")
            xat <- seq(xaxp[1], xaxp[2], length.out = xaxp[3] + 
                1)
            yat <- seq(yaxp[1], yaxp[2], length.out = yaxp[3] + 
                1)
            axis(1, at = xat, label = -xat)
            axis(2, at = yat, label = -yat, srt = 90)
        }
        else {
            axis(1)
            axis(2)
        }
    }
	title(...)
	box(...)
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
                          formant=TRUE, axes=TRUE,
                          xlab="", ylab="",
                          main = "", xlim, ylim)
{

  if( ncol(data) > 2 ) {
    data <- data[,1:2]
  }
  if( ncol(data) != 2 ) {
    stop( "polygonplot() requires two columns of data" )
  }

  if(formant)
    data <- cbind(-data[, 2], -data[, 1])


  points <- NULL
  for( l in order ) {
    tmp <- matrix(data[labels==l],ncol=2)
    points <- rbind( points, apply(tmp, 2, mean) )
  }

  plot( points, type="b", pch=" ", axes=FALSE, xlab="", ylab="" )
  text( points, order, axes=FALSE, , xlab="", ylab="" )

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
