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

"segs2musegs"<- function( segs, prefix, uselastonly=F )
{
  # convert a new style emu segment list to an old style mu+ one
  # replace : with / and add prefix unless uselastonly is T in which 
  # case we use only the last part of the utterance name

  newutts _ utt(segs)
  for( i in 1:nrow(segs)){
    bits _ splitstring(utt(segs)[i], ":")
    newutts[i] <- paste(prefix, paste( bits, collapse="/"), sep="/" )
  }
  as.segs(cbind(mustart(segs), muend(segs), newutts), label(segs), 
	  level=attributes(segs)$level)
}


# Local Variables:
# mode:S
# S-temp-buffer-p:t
# End:
