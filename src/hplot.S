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


"hplot"<- function(vec, labs, xlab = "", ylab = "", main = "", 
                   colour = T, xlim = NULL, axes = T, legn="tl")
{
  ## written by Jonathan Harrington, 1992
  on.exit(par(new=F))

  if(missing(labs)) {
    legn <- F
    labs <- rep(".", length(vec))
  }

  len <- length(unique(labs))
  
  ## calculate the density, angle and colour vectors
  dens <- rep(30, len)  

  if(colour){
    col <- 1:len
    angle <- rep(45, len)
  }
  else{
    col <- rep(1, len)
    angle <- seq(45, 180, length = len)
  }

  mat <- NULL
  for(j in unique(labs)) {
    temp <- labs == j
    vals <- vec[temp]
    hvals <- hist(vals, plot = F)
    xvals <- hvals$breaks
    yvals <- hvals$counts
    xvals.d <- (diff(xvals)[1])/2
    xr1 <- c(xvals[1] - xvals.d, xvals[length(xvals)] + xvals.d)
    yr1 <- max(yvals)
    mat$y <- c(mat$y, yr1)
    mat$x <- c(mat$x, xr1)
    mat$diff <- c(mat$diff, xvals.d)
  }
  ylim <- c(0, max(mat$y) + (max(mat$y)/20))
  xlim <- range(mat$x)
  k <- 1
  lableg <- NULL
  for(j in unique(labs)) {
    temp <- labs == j
    vals <- vec[temp]
    hist(vals, xlab = "", ylab = "", main = "", col = col[k], xlim = 
	 xlim, ylim = ylim)
    lableg$lab <- c(lableg$lab, j)
    lableg$col <- c(lableg$col, col)
    lableg$angle <- c(lableg$angle, angle[k])
    k <- k + 1
    par(new = T)
  }
  if(legn!=F){
    legn <- mu.legend(legn, xlim, ylim)
    legend(legn$x, legn$y, legend = lableg$lab, 
	   col = lableg$col)
  }
  title(main = main, xlab = xlab, ylab = ylab, col = 1)
}


# Local Variables:
# mode:S
# S-temp-buffer-p:t
# End:
