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


"nplot"<- function(values, labs=NULL, xlab = "", ylab = "probability density", 
                   main = "", colour=T, linetype=F, plot.lab = T, xlim = NULL)
{
  ## values: a vector of parameter values
  ## labs: parallel labels
  if(is.null(labs)){
    plot.lab _ F
    labs _ rep(1, length(values))}
  col.lty _ mu.colour(unique(labs), colour, linetype)
  colour _ col.lty$colour
  lty _ col.lty$linetype

  vals.norm <- NULL
  newlab <- NULL
  for(j in unique(labs)) {
    temp <- muclass(labs, j)
    vec1 <- values[temp]
    meanvals <- mean(vec1)
    sdvals <- sqrt(var(vec1))
    normvals <- dnorm(vec1, meanvals, sdvals)
    dat1 <- cbind(vec1, normvals)
    vals.norm <- rbind(vals.norm, dat1[sort.list(dat1[, 1]),  ])
    newlab <- c(newlab, rep(j, sum(temp)))
  }
  
  if(is.null(xlim))
    xrange <- range(vals.norm[, 1])
  else xrange <- xlim

  yrange <- range(vals.norm[, 2])

  k <- 1
  for(j in unique(newlab)) {
    temp <- muclass(newlab, j)
    plot(vals.norm[temp,  ], type = "l", axes = F, xlab = "", ylab
	 = "", col = colour[k], xlim = xrange, ylim = yrange, lty
	 = lty[k])
    if(plot.lab) {
      mtext(j, line = 0.1, side = 3, at = mean(vals.norm[temp, 
				     1]), col = colour[k])
    }
    k <- k + 1
    par(new = T)
  }
  axis(1)
  axis(2)
  box()
  title(main = main, ylab = ylab, xlab = xlab)
  par(new = F)
}


# Local Variables:
# mode:S
# S-temp-buffer-p:t
# End:
