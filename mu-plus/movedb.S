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

"movedb"<- function(segs.or.utts, fromdir, todir ) {
  if( is.segs( segs.or.utts ) )
    tmp <- utt( segs.or.utts )
  else 
    tmp <- segs.or.utts
     
     
  tmp <- paste( todir, substring( tmp, nchar(fromdir)+1 ), sep="" )
  
  if( is.segs( segs.or.utts ) )
    segs.or.utts[,3] <- tmp
  else 
    segs.or.utts <- tmp

  return(segs.or.utts)

}


# Local Variables:
# mode:S
# S-temp-buffer-p:t
# End:
