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

"muslice"<-
function(segs, npoints = 256, shift = npoints/2, nslices, centering=F, 
	 bark=T, barklim=c(0,19), lpc = F, lpcorder = 10, dbspec = F, 
	 samples = F, hamming = T, verbose=F) 
{
# written by Jonathan Harrington

  if(sum(bark, lpc, dbspec, samples) != 1 ) 
    stop("muslice: specify just one of bark, lpc, dbspec or samples")
  if(!missing(shift) && !missing(nslices))
    stop("muslice: specify only one of shift or nslices")

  muslice.cmd <- "muslice"
  segfile <- tempfile("muslice")
#  on.exit( unix(paste("rm -f", segfile)) )
  

  mat <- NULL
  if(dbspec)
    command <- paste( muslice.cmd, segfile, "-d")
  if(samples)
    command <- paste( muslice.cmd, segfile, "-m")
  if(bark) 
    command <- paste( muslice.cmd, segfile, "-b")
  if(lpc) 
    command <- paste( muslice.cmd, segfile, "-l", lpcorder)


  if(!hamming)
    command <- paste(command, "-h")

  command <- paste(command, "-w", npoints)

  if( !missing(nslices) )
    command <- paste(command, "-n", nslices)
  else
    command <- paste(command, "-s", shift)
  
  if(verbose) command <- paste(command, "-v" )

  if(verbose) cat("Writing segment list to ", segfile, "..." )
  museg.write(segs, segfile)
  if(verbose) cat("done\n")

  if(verbose) cat("Command =>", command, "\n")
  unix(command)

  result.skip <- 4
  if( centering ) result.skip <- result.skip+1
  if( samples )   result.skip <- result.skip-1

  basename <- unix( paste( "basename", segfile ) )
  
  if( samples ) extn <- ".samp"
  if( dbspec )  extn <- ".db"
  if( bark )    extn <- ".bark"
  if( lpc )     extn <- ".lpc"

  result.dir <- paste( segfile, extn, sep="" )
  result.file <- paste( result.dir, "/", basename, extn, sep="" )

#  on.exit( unix( paste("rm -r", result.dir ) ) )

  ## read eg segfile.db/segfile.db skipping the first few lines, this file 
  ## contains the names of the seperate files for each segment
  filename <- matscan( result.file, 2, sk=result.skip, what = "")

  filename[, 1] <- paste(result.dir, filename[, 1], sep = "/")
  
  ## indexvec is the number of samples for each segment
  indexvec <- as.numeric(filename[, 2])

  right <- cumsum(indexvec)
  left <- (right - indexvec) + 1

  index <- cbind(left, right)
  data <- matrix(0, right[length(right)], ((npoints/2) + 1))

  ## work out new ftime matrix
  durs <- mudur(segs)
  period <- durs/indexvec
  ftime _ cbind( mustart(segs)+period/2, muend(segs)-period/2 )

  ## read the data for each segment from the corresponding file
  for(j in 1:length(right)) {
    data[left[j]:right[j],  ] <- matscan(filename[j, 1], ((npoints/2) + 1))
  }

  as.trackdata(data=data, index=index, ftime=ftime)
}



# Local Variables:
# mode:S
# S-temp-buffer-p:t
# End:
