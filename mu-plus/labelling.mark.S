##########################################################################
#									 #
#	  This file is part of the MU+ Speech Database System		 #
#			  (c) Copyright 1994,				 #
#	     Speech Hearing and Language Research Centre,		 #
#	       Macquarie University, Sydney, Australia.			 #
#									 #
#	Copying and redistribution of this software is subject to the	 #
#	conditions of the MU+ licence.  Any modifications of this code 	 #
#	remain the property of Macquarie University.			 #
#									 #
##########################################################################

"labelling.mark"<-
function(rvals, uttid, init.time, col = 1)
{
  ## called by uttplot() to do labelling
  cat("Click left mouse button on the plot to place a mark, then enter label\n")
  cat("Click middle mouse button to quit\n")
  mat <- NULL
  repeat {
    flag <- F
    z <- locator(1)
    y <- z$y
    z <- z$x
    if(!any(z)) break else {
      abline(v = z, col = col)
      cat("Label >")
      labelchar <- readline()
      mat$times <- c(mat$times, z)
      mat$lab <- c(mat$lab, labelchar)
      mtext(labelchar, side = 3, at = z, line = 2, col = col)
    }
    ## else if(y > rvals[2]) {
    ## num <- closest(mat$times, z)
    ## abline(v = mat$times[num], col = 0)
    ## mtext(mat$lab[num], side = 3, at = mat$times[num], 
    ##	line = 2, col = 0)
    ## mat$times <- mat$times[ - num]
    ## mat$lab <- mat$lab[ - num]
    ## }
  }
  firstvals <- c(init.time, mat$times[.Uminus(length(mat$times))])
  lastvals <- mat$times
  tvals <- cbind(firstvals, lastvals, rep(uttid, length(mat$times)))
  make.seglist(mat$lab, 
	       firstvals, 
	       lastvals, 
	       rep(uttid, length(mat$times)),
	       "Labelled with uttplot")
}


# Local Variables:
# mode:S
# S-temp-buffer-p:t
# End:
