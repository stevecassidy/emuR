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

"phon"<-
function(utterances, query = paste(find, "!=''"), find = "Phonetic", 
	 ret = find, extn = options()$labextn,  
	 dirc = options()$templatedir)
{
  .C("Save_Comparison",
     as.character(query),
     as.integer(length(query)),
     as.character(find),
     as.character(ret),
     as.character(""),  # baseobject
     as.integer(1),
     as.character(dirc))
  .C("Get_Columns")
  nsegs <- 0
  r <- .C("myphon",
	  as.character("*"),
	  as.integer(1),     # ??
	  as.integer(0),     # offset
	  as.character(utterances),
	  as.integer(length(utterances)),
	  as.character("x"), # dbdirs
	  as.integer(1),     # length(dbdirs)
	  as.integer(nsegs),
	  as.character(extn),
	  as.integer(length(extn)))
  .C("Free_Columns")
  nsegs <- r[[8]]
  if(nsegs == 0) {
    warning("No such segments")
    return(NULL)
  }
  segs <- matrix(0, nsegs, 3)
  r <- .C("mygetsegs",
	  as.single(segs),
	  as.integer(nsegs))
  segs <- r[[1]]
  labs <- rep("", nsegs)
  r <- .C("mygetlabs",
	  as.character(labs),
	  as.integer(nsegs))
  newlabs <- r[[1]]
  .C("myfreelabs")
  isegs.to.segs(segs, utterances, newlabs, level=ret)
}


# Local Variables:
# mode:S
# S-temp-buffer-p:t
# End:
