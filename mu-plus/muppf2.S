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

"muppf2"<-
function(segs1, segs2, file = "segs", snr1 = 0, snr2 = 0, warntone = F, warn = 
	1000, delay = 5000, noise1 = -1, noise2 = -1, space = 1000, runppf = T, 
	llead = 0, rlead = 0, mllead = 0, mrlead = 0, pad1 = 0, pad2 = 0, 
	normal = 1)
{
  ppfprogram <- paste(options()$muhome, "ppf/ppf", sep="/" )
  ppfvardecs <- paste(options()$muhome, "ppf/variable-declarations", sep="/")

  ## check that the length of both segments are the same
  if(nrow(segs1) != nrow(segs2)) 
    stop( "segs1 and segs2 must have the same number of rows!")	
  ## put out 1 second of silence
  cat("# 1 second of silence\n{LR\nsnr = 0\ndelay = 1000\n> - 0 0 file0\n}\n\n",
      file = file)	
  ## loop around, and send out the data to the file
  for(i in 1:nrow(segs1)) {
    ##
    ## the warning tone first
    if(warntone) {
      cat("# warning tone\n{LR\ndelay = ", warn + space, "\n",
	  file = file, append = T)
      cat("mllead = 0\nmrlead = 0\n", file = file, append = T
	  )
      cat("normal = 1\n", file = file, append = T)
      cat("noise = -1\nsnr = 0\n", file = file, append = T)
      cat("llead = 0\nrlead = 0\n", file = file, append = T)
      cat("> - 0 ", warn, " file0\n}\n\n", file = file, 
	  append = T)
    }
    ## now the actual segments
    cat("# a real segment\n{L\ndelay = ", delay, "\n", file = file, 
	append = T)
    cat("snr = ", snr1, "\n", file = file, append = T)
    cat("normal = ", normal, "\n", file = file, append = T)
    cat("pad = ", pad1, "\n", file = file, append = T)
    cat("noise = ", noise1, "\n", file = file, append = T)
    cat("llead = ", llead, "\n", file = file, append = T)
    cat("rlead = ", rlead, "\n", file = file, append = T)
    cat("mllead = ", mllead, "\n", file = file, append = T)
    cat("mrlead = ", mrlead, "\n", file = file, append = T)
    cat(">", label(segs1)[i], " ", mustart(segs1[i,  ]) - llead, 
	"  ", muend(segs1[i,  ]) + rlead, file = file, append
	= T)
    cat("   ", segs1[i, 3], "\n}\n\n", file = file, append = T)	#
    ##
    ## the next segment
    cat("# another real segment\n{R\ndelay = ", delay, "\n", file
	= file, append = T)
    cat("snr = ", snr2, "\n", file = file, append = T)
    cat("normal = ", normal, "\n", file = file, append = T)
    cat("pad = ", pad2, "\n", file = file, append = T)
    cat("noise = ", noise2, "\n", file = file, append = T)
    cat("llead = ", llead, "\n", file = file, append = T)
    cat("rlead = ", rlead, "\n", file = file, append = T)
    cat("mllead = ", mllead, "\n", file = file, append = T)
    cat("mrlead = ", mrlead, "\n", file = file, append = T)
    cat(">", label(segs2)[i], " ", mustart(segs2[i,  ]) - llead, 
	"  ", muend(segs2[i,  ]) + rlead, file = file, append = T)
    cat("   ", segs2[i, 3], "\n}\n\n", file = file, append = T)	
  }

  ## run the ppf program if asked to
  if(runppf) {
    cat(">>> Running PPF\n")
    unix(paste(ppfprogram, "2", file, ppfvardecs))
  }
}


# Local Variables:
# mode:S
# S-temp-buffer-p:t
# End:

