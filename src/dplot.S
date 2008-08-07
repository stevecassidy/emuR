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



`dplot` <-
function (x, labs = NULL, offset = 0, prop=TRUE, 
    average = FALSE, 
	    xlim = NULL, ylim = NULL,  lty = FALSE, normalise = FALSE, 
    colour = TRUE, lwd = NULL, pch=NULL, legend = "topright", axes = TRUE, type="l",
    n = 20, ...) 
{



if(prop)
{
if(length(offset) != 1)
stop("Specify only one offset time when prop=T")
else if ((offset < 0) | (offset > 1)) 
        stop("offset must be between 0 and 1 when prop=T")


}

else
if(nrow(x) != length(offset))
stop("nrow(x) and length(offset) must be the same when prop=F")

    pout <- NULL
    if (is.matrix(x$data)) {
        pout <- as.list(NULL)
        pout$data <- as.list(NULL)
        mat <- NULL
        if (is.null(ylim)) 
            ylim <- range(x$data)
        numcols <- ncol(x$data)
          
        for (j in 1:ncol(x$data)) {
            mat <- x
            mat$data <- mat$data[, j]
            if (!normalise) 
                vals <- dplot.time(mat, labs = labs, offset = offset, 
                  prop=prop, average = average,  xlim = xlim, 
                  ylim = ylim,  lty = lty, 
                  colour = colour, legend = legend, lwd = lwd, pch=pch, type=type)
            else vals <- dplot.norm(mat, labs = labs, average = average, 
                xlim = xlim, ylim = ylim,  lty = lty, 
                colour = colour, legend = legend, lwd = lwd, pch=pch, type=type,  n = n)
            par(new = TRUE)
            pout$data[[j]] <- vals$data
            if (j == ncol(x$data)) {
                pout$time <- vals$time
                pout$labs <- vals$labs
            }
        }
    }
    else {
        if (!normalise) 
            pout <- dplot.time(x, labs = labs, offset = offset, 
                prop=prop, average = average,  xlim = xlim, ylim = ylim, 
                 lty = lty, colour = colour, 
                lwd = lwd, pch=pch, type=type, legend = legend)
        else pout <- dplot.norm(x, labs = labs, average = average, xlim = xlim, 
            ylim = ylim,  lty = lty, colour = colour, 
            lwd = lwd, pch=pch, type=type, legend = legend,  n = n)
    }

    par(new = FALSE)

    invisible(pout)
	title(...)
	box(...)
	 if (axes) {
        axis(side = 1)
        axis(side = 2)
    }
}




`dplot.time` <-
function (x, labs = NULL, offset = 0, prop=TRUE, 
    average = FALSE,  
    xlim = NULL, ylim = NULL, lty = FALSE, colour = TRUE, 
    lwd = NULL, pch=NULL, legend = "topright",  type="l", ...) 
{
    ovec <- as.list(NULL)
    samrate <- 1000/((x$ftime[1, 2] - x$ftime[1, 
        1])/(x$index[1, 2] - x$index[1, 1]))
        if (is.null(labs)) 
        labs <- rep(1, nrow(x$index))
    col.lty <- mu.colour(labs, colour, lty, lwd, pch)
    colour <- col.lty$colour
    lty <- col.lty$linetype
    lwd <- col.lty$lwd
	pch <- col.lty$pch
    if (prop) 
        ref.time <- x$ftime[, 1] + ((x$ftime[, 2] - 
            x$ftime[, 1]) * offset)
			else ref.time <- offset
    maxlen <- 2 * (max(x$index[, 2] - x$index[, 1] + 
        1))
    pointval <- round(maxlen/2)
    mat.na <- matrix(NA, nrow(x$index), maxlen)
    for (j in 1:nrow(x$index)) {
        left <- x$index[j, 1]
        right <- x$index[j, 2]
        length.index <- right - left + 1
        times <- x$ftime[j, ]
        refn <- ref.time[j]
        inval <- closest(seq(times[1], times[2], length = length.index), 
            refn)
        inval <- inval[1]
        left.na <- pointval - inval + 1
        right.na <- left.na + length.index - 1
        mat.na[j, left.na:right.na] <- x$data[left:right]
    }
    z <- apply(mat.na, 2, mean, na.rm = TRUE)
    natemp <- is.na(z)
    nums <- c(1:length(natemp))
    nonums <- nums[!natemp]
    interval <- 1000/samrate
    if (is.null(xlim)) 
        xlim <- c(nonums[1], nonums[length(nonums)])
    else xlim <- c(pointval + xlim[1]/interval, pointval + xlim[2]/interval)
    time1 <- (1 - pointval) * interval
    time2 <- (ncol(mat.na) - pointval) * interval
    xtime <- seq(time1, time2, length = ncol(mat.na))
    xtimelim <- (xlim - pointval) * interval
    if (is.null(ylim)) 
        ylim <- range(mat.na, na.rm = TRUE)
    if (!average) {
        for (j in 1:nrow(mat.na)) {
            plot(xtime, mat.na[j, ], xlim = xtimelim, ylim = ylim, 
                xlab = "", ylab = "", axes = FALSE, type = type, 
                col = colour[j], lty = as.numeric(lty[j]), bty="n",
                lwd = as.numeric(lwd[j]), pch=as.numeric(pch[j]))
            par(new = TRUE)
        }
        ovec$data <- mat.na
        ovec$time <- xtime
        ovec$labs <- labs
    }
    else {
        if (!is.null(labs)) {
            outmat <- NULL
            outlabs <- NULL
            for (j in unique(labs)) {
                temp <- labs == j
                vals <- mat.na[temp, ]
                if (is.matrix(vals)) {
                  mvals <- apply(vals, 2, mean, na.rm = TRUE)
                }
                else {
                  mvals <- vals
                }
                outmat <- rbind(outmat, mvals)
                outlabs <- c(outlabs, j)
            }
        }
        else {
            outmat <- apply(mat.na, 2, mean, na.rm = TRUE)
            outmat <- rbind(outmat)
            outlabs <- 1
        }
        col.code <- match(col.lty$legend$lab, unique(labs))
        colour <- col.lty$legend$col
        lty <- col.lty$legend$lty
        lwd <- col.lty$legend$lwd
		pch <- col.lty$legend$pch
        for (j in 1:nrow(outmat)) {
            plot(xtime, outmat[j, ], xlim = xtimelim, ylim = ylim, 
                xlab = "", ylab = "", axes = FALSE, type = type, bty="n", 
                col = colour[col.code[j]], lty = as.numeric(lty[col.code[j]]), 
                lwd = as.numeric(lwd[col.code[j]]), pch = as.numeric(pch[col.code[j]]))
            par(new = TRUE)
        }
        ovec$data <- outmat
        ovec$time <- xtime
        ovec$labs <- outlabs
    }
   
    if (is.logical(legend)) {
        if (legend) 
		{
            legend <- "topright"
			if ((type=="l") | is.null(pch))
			legend(legend, NULL, col.lty$legend$lab, col = col.lty$legend$col, 
            lty = as.numeric(col.lty$legend$lty), lwd = as.numeric(col.lty$legend$lwd))
			else if(type=="p")
			legend(legend, NULL, col.lty$legend$lab, col = col.lty$legend$col, 
            pch = as.numeric(col.lty$legend$pch) )
			else
        legend(legend, NULL, col.lty$legend$lab, col = col.lty$legend$col, 
            lty = as.numeric(col.lty$legend$lty), lwd = as.numeric(col.lty$legend$lwd),pch = as.numeric(col.lty$legend$pch) )
			}
    }
    else 
	{
	if ((type=="l") | is.null(pch))
	legend(legend, NULL, col.lty$legend$lab, col = col.lty$legend$col, 
        lty = as.numeric(col.lty$legend$lty), lwd = as.numeric(col.lty$legend$lwd))
		else if(type=="p")
		legend(legend, NULL, col.lty$legend$lab, col = col.lty$legend$col, 
        pch = as.numeric(col.lty$legend$pch) )
		else
	legend(legend, NULL, col.lty$legend$lab, col = col.lty$legend$col, 
        lty = as.numeric(col.lty$legend$lty), lwd = as.numeric(col.lty$legend$lwd),pch = as.numeric(col.lty$legend$pch) )
		
		}

    invisible(ovec)
	
}




`dplot.norm` <-
function (x, labs = NULL, average = FALSE, 
 xlim = NULL, ylim = NULL,  lty = FALSE, type="l",
    colour = TRUE, lwd = NULL, pch=NULL, legend = "topright",  
    n = 20) 
{
    ovec <- NULL
    if (is.null(ylim)) 
        ylim <- range(x$data)
    if (is.null(xlim)) 
        xlim <- c(0, 1)
    if (is.null(labs)) {
        labs <- rep(1, nrow(x$index))
    }
    col.lty <- mu.colour(labs, colour, lty, lwd, pch)
    colour <- col.lty$colour
    lty <- col.lty$linetype
    lwd <- col.lty$lwd
	pch <- col.lty$pch
    mat.na <- linear(x, n)
    mat.na$ftime <- x$ftime
    class(mat.na) <- "trackdata"
    xvec <- seq(0, 1, length = n)
    lval <- nrow(x$index)
    if (!average) {
        for (j in 1:lval) {
            plot(xvec, mat.na[j]$data, xlim = xlim, ylim = ylim, 
                xlab = "", ylab = "", axes = FALSE, type = type, bty="n", 
                col = colour[j], lty = as.numeric(lty[j]), 
                lwd = as.numeric(lwd[j]), pch = as.numeric(pch[j]))
            par(new = TRUE)
        }
        ovec$data <- mat.na
        ovec$time <- xvec
        ovec$labs <- labs
    }
    else {
        if (!is.null(labs)) {
            outmat <- NULL
            outlabs <- NULL
            for (j in unique(labs)) {
                temp <- labs == j
                vals <- mat.na[temp]$data
                vals <- matrix(vals, ncol = n, byrow = TRUE)
                if (is.matrix(vals)) {
                  mvals <- apply(vals, 2, mean)
                }
                else {
                  mvals <- vals
                }
                outmat <- rbind(outmat, mvals)
                outlabs <- c(outlabs, j)
            }
        }
        else {
            outmat <- apply(matrix(mat.na, ncol = 20, byrow = TRUE), 
                2, mean)
            outmat <- rbind(outmat)
            outlabs <- 1
        }
        col.code <- match(col.lty$legend$lab, unique(labs))
        colour <- col.lty$legend$col
        lty <- col.lty$legend$lty
        lwd <- col.lty$legend$lwd
		pch <- col.lty$legend$pch
        for (j in 1:nrow(outmat)) {
            plot(xvec, outmat[j, ], xlim = xlim, ylim = ylim, 
                xlab = "", ylab = "", axes = FALSE, type = type, bty="n", 
                col = colour[col.code[j]], lty = as.numeric(lty[col.code[j]]), 
                lwd = as.numeric(lwd[col.code[j]]), pch = as.numeric(pch[col.code[j]]))
            par(new = TRUE)
        }
        ovec$data <- outmat
        ovec$time <- xvec
        ovec$labs <- labs
    }
    
    if (is.logical(legend)) {
        if (legend) 
		{
            legend <- "topright"
			if ((type=="l") | is.null(pch))
			legend(legend, NULL, col.lty$legend$lab, col = col.lty$legend$col, 
            lty = as.numeric(col.lty$legend$lty), lwd = as.numeric(col.lty$legend$lwd))
			else if(type=="p")
			legend(legend, NULL, col.lty$legend$lab, col = col.lty$legend$col, 
            pch = as.numeric(col.lty$legend$pch) )
			else
        legend(legend, NULL, col.lty$legend$lab, col = col.lty$legend$col, 
            lty = as.numeric(col.lty$legend$lty), lwd = as.numeric(col.lty$legend$lwd),pch = as.numeric(col.lty$legend$pch) )
			}
    }
    else 
	{
	if ((type=="l") | is.null(pch))
	legend(legend, NULL, col.lty$legend$lab, col = col.lty$legend$col, 
        lty = as.numeric(col.lty$legend$lty), lwd = as.numeric(col.lty$legend$lwd))
		else if(type=="p")
		legend(legend, NULL, col.lty$legend$lab, col = col.lty$legend$col, 
        pch = as.numeric(col.lty$legend$pch) )
		else
	legend(legend, NULL, col.lty$legend$lab, col = col.lty$legend$col, 
        lty = as.numeric(col.lty$legend$lty), lwd = as.numeric(col.lty$legend$lwd),pch = as.numeric(col.lty$legend$pch) )
		
		}
    invisible(ovec)
}

