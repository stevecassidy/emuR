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

"segs.to.isegs"<-
function(segs)
{
  if(!is.segs(segs)) stop("Not a segment list")
  utterances <- unique(sort(utt(segs)))
  isegs <- cbind(match(utt(segs), utterances), mustart(segs), muend(segs))
  names(isegs) <- names(segs)
  list(segs = isegs, utts = utterances)
}


# Local Variables:
# mode:S
# S-temp-buffer-p:t
# End:
