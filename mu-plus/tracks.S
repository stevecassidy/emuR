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

"tracks"<-
function(utterance, duplicates = FALSE, extn = options()$extensions)
{
  # if they gave us an utterance list, take the first one.
  if(length(utterance) > 1) utterance _ utterance[1]
  ntracks <- 0
  r <- .C("myntracks",
	  as.character(utterance),
	  as.character(""),  # dbdirs
	  as.integer(1),     # length(dbdirs)
	  as.integer(ntracks),
	  as.character(extn),
	  as.integer(length(extn)))
  ntracks <- r[[4]]
  tracknames <- rep("", ntracks)
  r <- .C("mytracks",
	  as.character(utterance),
	  as.character(""),  #dbdirs
	  as.integer(1),     # length(dbdirs)
	  as.character(tracknames),
	  as.character(extn),
	  as.integer(length(extn)))
  tracknames <- r[[4]]
  if(duplicates)
    tracknames
  else
    tracknames[!duplicated(tracknames)]
}


# Local Variables:
# mode:S
# S-temp-buffer-p:t
# End:
