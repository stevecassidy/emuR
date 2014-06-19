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


`mu.colour` <-
function (labs, col = TRUE, linetype = FALSE, lwd = NULL, pch=NULL) 
{
    result <- NULL
    if (is.logical(col)) {
        if (col) 
            result$colour <- label_num(labs)
        else result$colour <- rep(1, length(labs))
    }
    else if (length(col) == length(labs)) 
        result$colour <- col
    else if (length(col) == length(unique(labs))) {
        k <- 1
        result$colour <- labs
        for (j in unique(labs)) {
            temp <- labs == j
            result$colour[temp] = col[k]
            k <- k + 1
        }
    }
    else if (length(col) == 1) 
        result$colour <- rep(col, length(labs))
    if (is.logical(linetype)) {
        if (linetype) 
            result$linetype <- label_num(labs)
        else result$linetype <- rep(1, length(labs))
    }
    else if (length(linetype) == length(labs)) 
        result$linetype <- linetype
    else if (length(linetype) == length(unique(labs))) {
        k <- 1
        result$linetype <- labs
        for (j in unique(labs)) {
            temp <- labs == j
            result$linetype[temp] = linetype[k]
            k <- k + 1
        }
    }
    else if (length(linetype) == 1) 
        result$linetype <- rep(linetype, length(labs))
    if (is.null(lwd)) 
        result$lwd <- rep(1, length(labs))
    else if (length(lwd) == length(labs)) 
        result$lwd <- lwd
    else if (length(lwd) == length(unique(labs))) {
        k <- 1
        result$lwd <- labs
        for (j in unique(labs)) {
            temp <- labs == j
            result$lwd[temp] = lwd[k]
            k <- k + 1
        }
    }
    else if (length(lwd) == 1) 
        result$lwd <- rep(lwd, length(labs))
		
		 if (is.null(pch)) 
        result$pch <- rep(1, length(labs))
    else if (length(pch) == length(labs)) 
        result$pch <- pch
    else if (length(pch) == length(unique(labs))) {
        k <- 1
        result$pch <- labs
        for (j in unique(labs)) {
            temp <- labs == j
            result$pch[temp] = pch[k]
            k <- k + 1
        }
    }
    else if (length(pch) == 1) 
        result$pch <- rep(pch, length(labs))

		
    p1 <- paste(labs, result$colour, result$linetype, result$lwd, result$pch)
    p1.temp <- duplicated(p1)
    result$legend$lab <- labs[!p1.temp]
    result$legend$col <- result$colour[!p1.temp]
    result$legend$lty <- result$linetype[!p1.temp]
    result$legend$lwd <- result$lwd[!p1.temp]
	result$legend$pch <- result$pch[!p1.temp]
    result
}





## return the colour for a given label via the colour object
mu.colour.get <- function(col.lty, label) {

  colour <- col.lty$legend$col[match(label, col.lty$legend$lab)]
  return( colour )

}

mu.linetype.get <- function(col.lty, label) {

  lty <- col.lty$legend$lty[match(label, col.lty$legend$lab)]
  return( lty )

}


# Local Variables:
# mode:S
# S-temp-buffer-p:t
# End:
