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

"frames"<-
function(segs, trackname, justindex = F, justftime = F, 
	 justdata = F, extn = options()$extensions)
{
	retv <- NULL
	if(is.matrix(segs) == F)
		segs <- rbind(segs)
	nframes <- 0
	width <- 0
	isegs <- segs.to.isegs(segs)
	utterances <- isegs$utts
	isegs <- isegs$segs
	Indx <- rep(-1, slen(segs) * 2)
	FTime <- rep(-1, slen(segs) * 2)
	r <- .C("mytotframes",
		as.single(isegs),
		as.integer(slen(segs)),
		as.integer(0),   # lhs
		as.integer(0),   # rhs
		as.integer(0),   # markseg
		as.character(utterances),
		as.character(""), # dbdirs
		as.integer(nframes),
		as.integer(width),
		as.character(extn),
		as.integer(length(extn)),
		as.character(trackname),
		as.integer(Indx),
		as.single(FTime))
	nframes <- r[[8]]
	width <- r[[9]]
	Indx <- r[[13]]
	FTime <- r[[14]]
	if(justindex) {
		return(matrix(Indx, slen(segs), 2))
	}
	if(justftime) {
		return(matrix(FTime, slen(segs), 2))
	}
	frms <- rep(NA, nframes * width)
	r <- .C("myframes",
		as.single(isegs),
		as.integer(slen(segs)),
		as.character(trackname),
		as.integer(FALSE),   # difference, not used
		as.integer(0),   # lhs
		as.integer(0),   # rhs
		as.integer(0),   # markseg
		as.character(utterances),
		as.integer(length(utterances)),
		as.single(frms),
		NAOK = TRUE)
	frms <- r[[10]]
	if(width == 1)
		retv$data <- frms
	else retv$data <- matrix(frms, nframes, width)	# return the result

	if( justdata ) return( retv$data )

	retv$index <- matrix(Indx, slen(segs), 2)
	retv$ftime <- matrix(FTime, slen(segs), 2)
	retv$trackname <- trackname
	class(retv) <- "trackdata"
	return(retv)
}


# Local Variables:
# mode:S
# S-temp-buffer-p:t
# End:
