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


"locus"<- function(data.onset, data.mid, labels.stop, labels.vow,
                   colnum = 1, which = unique(labels.stop)[1],
                   col = 1, ylab = "", xlab = "",
                   main = paste( "locus equation for", which),
                   sub = T, xlim = NULL, ylim = NULL, 
                   plot.graph = T, printval = T, lineval = 0.5)
{
# calculates a locus equation from formant data.
# data.onset: a list returned by track of formant values at the segment onset
# data.mid: as data.onset, but taken at the segment midpoint. Note
# that both data.onset and data.mid can consist of one or more formants
# labels.stop: a vector of labels for the consonants (usually stops)
# labels.vow: a vector of labels for the vowels
# colnum: which column from data.onset$data and data.mid$data should
# be plotted?
# which: which label type should by plotted? e.g. b, d, or g.
# plot.graph: if T, a data will be plotted in the plane of
# data.onset$data (x-axis) against data.mid$data (y-axis) for whichever
# column (e.g. 1 -3) is chosen in colnum, and whichever label type
# is selected in which. A locus equation is drawn through the plot
# if printval, the calculated locus frequency, the slope of the
# locus line and its intercept are plotted. Note that the locus 
# equation is calculated from the frequency at which the regression
# line through the points bisects the line y=x. 
# sub: if T, then the values of the locus frequency, intercept
# of the regression line, and regression line slope will be printed
# on the x-axis: see lineval, below, to control the distance of
# this text from the x-axis.
# lineval: number of units away the sub text is from the x-axis
# (the sub text contains information on locus freq., intercept, slope)
	temp <- muclass(labels.stop, which)
	if(!is.matrix(data.onset))
		data.onset <- cbind(data.onset)
	if(!is.matrix(data.mid))
		data.mid <- cbind(data.mid)
	l.xv <- labels.vow[temp]
	midvals <- data.mid[temp, colnum]
	onsetvals <- data.onset[temp, colnum]
	if(any(xlim) == F)
		xlim <- range(midvals)
	if(any(ylim) == F)
		ylim <- range(onsetvals)
	if(plot.graph)
		plot(midvals, onsetvals, type = "n", col = col, ylab = "", xlab
			 = "", xlim = xlim, ylim = ylim)
	regr <- lsfit(midvals, onsetvals)
	mat <- rbind(cbind(1, -1), cbind(1, -regr$coef[2]))
	vec <- c(0, regr$coef[1])
	regr$solve <- ((solve(mat) %*% vec)[1])
	if(plot.graph) {
		if(sub)
			sub <- paste("locus =", paste(format(round(regr$solve)),
				"Hz", sep = ""), "intercept=", format(round(
				regr$coef[1], 2)), "slope=", format(round(regr$
				coef[2], 2)))
		abline(regr$coef, col = col)
		abline(0, 1, col = 50, lty = 2)
		text(midvals, onsetvals, l.xv, col = col)
		title(main = main, ylab = ylab, xlab = xlab)
		mtext(sub, 3, line = lineval)
	}
	if(printval)
		c(regr$solve, regr$coef)
}

# Local Variables:
# mode:S
# S-temp-buffer-p:t
# End:


