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

"rate"<-
function(utterance, track = "samples",  extn = options()$extensions )
{
  rate <- -1
  r <- .C("myrate",
	  as.character(utterance),
	  as.character(""),
	  as.integer(1),
	  as.character(track),
	  as.character(extn),
	  as.integer(length(extn)),
	  as.single(rate))
  rate <- r[[7]]
  if(rate > -0.5) return( rate )
}


# Local Variables:
# mode:S
# S-temp-buffer-p:t
# End:
