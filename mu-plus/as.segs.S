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

"as.segs"<-
function(x, labels, level="Unknown")
{
  if( missing(level) && !is.null(attributes(x)$level) ) 
    level <- attributes(x)$level 

  if(missing(labels)){
    ll <- dimnames(x)[[1]]
    if(!is.null(ll))
      labels <- ll
    else
      labels <- rep("",times=length(x)/3)
  }
  if( !is.null(class(x)) ) if( class(x) == "data.frame" ) x <- as.matrix(x)
  x  <- matrix(x, ncol = 3, dimnames = list(labels, c("start", "end", "utt")))
  class(x) <- "segs"
  attributes(x)$level <- level
  x
}


# Local Variables:
# mode:S
# S-temp-buffer-p:t
# End:
