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

"muppfsl" <- function(segs, file="ppfdata")
{
  ## Generate an sd file and a matching segment list from the given
  ## segments, via muppf
  
  ## first find out the max lenght of segments, this will determine
  ## the delay we ask for between items
  mseglen <- max(mudur(segs))

  ## add a margin over this
  mseglen <- mseglen+400

  ## we will ask muppf to generate an sd file with mseglen delay between 
  ## the onsets of each segment, so we can calculate the start and end
  ## times of the segments in the new file
  
  ## muppf always puts 1 sec of silence at the start of each file
  ppfstart <- 1000   
  start.times <- seq(ppfstart, by=mseglen, length.out=slen(segs))
  end.times <- start.times + mudur(segs)
  uttnames <- rep(file, slen(segs))

  newsegs <- as.segs(cbind(start.times, end.times, uttnames), 
		     label(segs),
		     level=attributes(segs)$level)

  ## now call muppf
  muppf(segs, file=file, delay=mseglen, space=0, warn=0)
  
  newsegs
}

# Local Variables:
# mode:S
# S-temp-buffer-p:t
# End:
