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

"mucolnames"<- function(template="labed_template", dir=options()$templatedir)
{
  command _ paste( "awk '/^#/{print $1}' ", dir, "/", template, 
	     " | sed -e 's/#//'", sep="" )
  unix(command)
}


# Local Variables:
# mode:S
# S-temp-buffer-p:t
# End:
