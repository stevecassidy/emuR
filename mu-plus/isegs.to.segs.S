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

"isegs.to.segs"<-
function(x, utterances, labs = NULL, level=NULL)
{
  segs <- matrix(x, ncol = 3)
  segs <- cbind(segs[, 2], segs[, 3], utterances[segs[, 1]])
  dimnames(segs) <- list(labs, c("start", "end", "utt"))
  attributes(segs)$level <- level
  class(segs) <- "segs"
  segs
}


# Local Variables:
# mode:S
# S-temp-buffer-p:t
# End:
