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

"muspec"<-
function(segs, npoints = 256, dbspec = T, bark = FALSE, barklim = c(0, 19), 
	 lpc = FALSE, lpcorder = 10, samples = FALSE, offset = 0, 
	 boost = FALSE, raw = FALSE, 
	 centering = FALSE, hamming = TRUE, normalise = "self", verbose = F)
{
# segs: a segment list produced by phon() 
# npoints: width in number of points for the FFT
# samples: if T, then sampled data is returned in mat$samples
# spec: if T, spectral values are returned in mat$spec
# bark: if T, energy values in  critical bands are
# returned in mat$bark
# lpc: if T, lpc coefficients are returned in mat$lpc
# offset: a value or a vector of values between 0 and 1 defining 
# the starting point(s) for the left edge of the FFT
# centering: if T, the FFT is centered at offset
# boost: if T, apply 6db/octave boost above 500 Hz
# raw: if T, return raw spectrum instead of dB
# normalise: a string, 
#              "self" for self normalisation (default)
#              "meanrms" for norm. wrt mean rms of current utterance
#              "meanvoiced" for norm. wrt mean rms of voiced parts of curr utt
#              "none" for no normalisation
#
# written by Jonathan Harrington and Steve Cassidy to interface
# with the muspec program written by Steve Cassidy. See UNIX 
# level documentation on muspec

  if(any(offset>1) | any(offset<0))
    stop("offset should be between 0 and 1 in muspec")

  if(!is.matrix(segs)) segs <- rbind(segs)
  mat <- NULL
  segfile <- tempfile("muspec")
  on.exit(unlink(segfile), add = T)
  command <- paste(options()$muhome, "/bin/muspec ", sep="")
  if(samples) {
    sampfile <- tempfile("muspec")
    on.exit(unlink(sampfile), add = T)
    command <- paste(command, "-s", sampfile)
  }
  if(bark) {
    barkfile <- tempfile("muspec")
    on.exit(unlink(barkfile), add = T)
    command <- paste(command, "-b", barkfile, "-r", barklim[1], 
		     "-u", barklim[2])
  }
  if(lpc) {
    lpcfile <- tempfile("muspec")
    on.exit(unlink(lpcfile), add = T)
    command <- paste(command, "-l", lpcfile, "-p", lpcorder)
  }
  if(dbspec) {
    specfile <- tempfile("muspec")
    on.exit(unlink(specfile), add = T)
    command <- paste(command, "-d", specfile)
  }
  ## add a -o for each offset specified
  val <- ""
  for(i in offset) {
    val <- paste(val, "-o", 100 * i)
  }
  command <- paste(command, val)
  if(centering)
    command <- paste(command, "-c")
  if(npoints != 256) {
    val <- paste("-w", npoints, sep = " ")
    command <- paste(command, val)
  }
  ## Hamming window
  if( !hamming ) command <- paste( command, "-n" )
  ## Normalisation
  norm <- switch(normalise,
		 self = 1,
		 meanrms = 2,
		 meanvoiced = 3,
		 none = 4)
  command <- paste(command, "-N", norm)

  if( boost )  command <- paste( command, "-B")
  if( raw )    command <- paste( command, "-R")

  ## Last arg to add is the segment file
  command <- paste(command, segfile )

  cat( "class of segs is ", class(segs), "\n" ) 
  museg.write(segs, segfile)
  if(verbose) print(command)	# Execute the command
  unix(command)
  copies <- length(offset)
  if(dbspec) {
    if( raw ) 
      ## this is a bug, raw gives only n/2 points per spectrum
      mat$spec <- matscan(specfile, copies * ((npoints/2)))
    else 
      mat$spec <- matscan(specfile, copies * ((npoints/2) + 1))
  }
  if(samples) {
    mat$samples <- matscan(sampfile, copies * npoints)
  }
  if(bark) {
    mat$bark <- matscan(barkfile, copies * (1 + barklim[2] - 
					    barklim[1]))
  }
  if(lpc) {
    mat$lpc <- matscan(lpcfile, copies * lpcorder)
  }
  mat
}


# Local Variables:
# mode:S
# S-temp-buffer-p:t
# End:
