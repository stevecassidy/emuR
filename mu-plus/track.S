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

"track"<-
function(segs, param, cut = NULL, index = T)
{
  x <- frames(segs, param)
  if(!missing(cut)) {
    index <- F
    vals <- x$index[, 1] + round((x$index[, 2] - x$index[, 1]) * cut)
    if(!is.matrix(x$data))
      x$data <- cbind(x$data)
    x$data <- x$data[vals,  ]
  }

  if(index)
    results <- x
  else results <- x$data
  return(results)
}



# Local Variables:
# mode:S
# S-temp-buffer-p:t
# End:
