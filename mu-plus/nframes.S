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

"nframes"<-
function(segments, param)
{
  ## computes the number of frames of data for a segment list on param
  indvals <- frames(segments, param, justindex=T)
  1 + (indvals[, 2] - indvals[, 1])
}


# Local Variables:
# mode:S
# S-temp-buffer-p:t
# End:
