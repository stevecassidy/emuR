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

"label"<-
function(segs, attribute = "Phoneme", 
	 extn = options()$labextn, dirc=options()$templatedir)
{
  ## written by Andrew McVeigh, 30/6/'91

  ## just now there is now real label fn for emu
  if( inherits(segs,"emusegs")) return( as.character(segs$label) )

  if( !inherits(segs,"segs")) stop( "not a segment list" )

  if( missing(attribute) ) {
    return( dimnames(segs)[[1]] )
  } else {
    .C("Save_Comparison",
       as.character("?"),
       as.integer(0),
       as.character("?"),
       as.character("?"),
       as.character("?"),
       as.integer(1),   # Version
       as.character(dirc))
    isegs <- segs.to.isegs(segs)	# separate out the fields
    nsegs <- nrow(segs)	# get the number of segments
    labs <- rep("", nsegs * length(attribute))
    expr <- rep(0, length(attribute))	
    ## allocate enough character pointers for the return data
    utterances <- isegs$utts	# get the utterance names
    isegs <- isegs$segs	
    ## get the start and the end times of the utterance
    attrlen <- length(attribute)
    .C("Get_Columns")
    r <- .C("Extended_Label_Fn",
	    as.integer(isegs),
	    as.character(utterances),
	    as.integer(nsegs),
	    as.character(attribute),
	    as.integer(attrlen),
	    as.integer(expr),
	    as.character(attributes(segs)$level),
	    as.character(labs),
	    as.integer(0),    # Offset
	    as.character(extn))
    labs <- r[[8]]	# get the result
    .C("Free_Columns")
    newlabs <- matrix(labs, nsegs)	#return the list
    .C("myfreexlabs")
    dimnames(newlabs) <- list(NULL, c(attribute))
    return(newlabs)
  }
}


# Local Variables:
# mode:S
# S-temp-buffer-p:t
# End:
