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

"sunplay"<-
function(filename, internalspkr = F)
{
# this function plays a sun format .au file through the external or internal speaker
	invisible(.C("sunplay",
		as.character(filename),
		as.integer(internalspkr)))
}


# Local Variables:
# mode:S
# S-temp-buffer-p:t
# End:
