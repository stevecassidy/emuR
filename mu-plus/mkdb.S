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

"mkdb"<- 
function(directory = options()$dbdefault, 
	 extn = options()$labextn, pattern = "*")
{
  filename <- paste(directory, "/", pattern, ".", extn, sep = "")
  cmd <- paste(sep = "", "ls ", filename, 
               "| sed -e 's/\\(.*\\)\\.", extn, "/\\1/'")
  invisible(unix(cmd))
}


# Local Variables:
# mode:S
# S-temp-buffer-p:t
# End:
