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

"muppfcrosssl" <- function(segs, segs2, file="ppfdata")
{
  ## Generate an sd file and a matching segment list from the given
  ## segment lists, via muppfcross
  
  ## first find out the max length of combined segments, this will determine
  ## the delay we ask for between items
  mseglen <- max(mudur(segs))+max(mudur(segs2))

  ## add a margin over this
  mseglen <- max(1000, mseglen+400)

  ## we will ask muppfcross to generate an sd file with mseglen delay between 
  ## the onsets of each segment, so we can calculate the start and end
  ## times of the segments in the new file
  
  ## muppf always puts 1 sec of silence at the start of each file
  ppfstart <- 1000   
  start.times <- seq(ppfstart, by=mseglen, length.out=slen(segs))
  end.times <- start.times + mudur(segs) + mudur(segs2)
  uttnames <- rep(file, slen(segs))

  newsegs <- as.segs(cbind(start.times, end.times, uttnames), 
		     paste(label(segs), label(segs2), sep="/"),
		     level=attributes(segs)$level)

  cat("delay=", mseglen)

  ## now call muppf
  muppfcross(segs, segs2, file=file, delay=mseglen, space=0, warn=0)
  
  newsegs
}

# Local Variables:
# mode:S
# S-temp-buffer-p:t
# End:
