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

"seglist.time"<-
function(k, ftime, frnum)
{
  ftime[, 1] + 
    (((ftime[, 2] - ftime[, 1]) * (k - frnum[, 1]))/(frnum[, 2] - frnum[, 1]))
}


# Local Variables:
# mode:S
# S-temp-buffer-p:t
# End:
