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

"muppfcross"<-
function(segs1, segs2, file = "segs", snr = 0, warntone = F, warn = 1000, delay
	 = 5000, noise = -1, space = 1000, runppf = T, llead = 0, rlead = 0, 
	 mllead = 0, mrlead = 0, normal = 0, pad = 0)
{
  ppfprogram <- paste(options()$muhome, "ppf/ppf", sep="/" )
  ppfvardecs <- paste(options()$muhome, "ppf/variable-declarations", sep="/")

  ## error check
  if(nrow(segs1) != nrow(segs2)) 
    stop( "segs1 and segs2 must have same number of rows!")	
  ## the snr variable
  cat("snr = ", snr, "\n", file = file)

  ## put out 1 second of silence
  cat("## 1 second of silence\n{L\ndelay = 1000\n> - 0 0 file0\n}\n\n", 
      file = file, append = T) 

  ## loop around, and send out the data to the file
  for(i in 1:nrow(segs1)) {
    ##
    ## the warning tone first
    if(warntone) {
      cat("# warning tone\n{L\ndelay = ", warn + space, "\n", 
	  file = file, append = T)
      cat("mllead = 0\nmrlead = 0\n", file = file, append = T
	  )
      cat("normal = 1\n", file = file, append = T)
      cat("noise = -1\nsnr = 0\n", file = file, append = T)
      cat("llead = 0\nrlead = 0\n", file = file, append = T)
      cat("> - 0 ", warn, " file0\n}\n\n", file = file, 
	  append = T)
    }
    ## now the actual segment
    cat("# start of a real segment\n{L\ndelay = ", delay, "\n", 
	file = file, append = T)
    cat("snr = ", snr, "\n", file = file, append = T)
    cat("noise = ", noise, "\n", file = file, append = T)
    cat("normal = ", normal, "\n", file = file, append = T)
    cat("llead = ", llead, "\n", file = file, append = T)
    cat("rlead = ", rlead, "\n", file = file, append = T)
    cat("mllead = ", mllead, "\n", file = file, append = T)
    cat("mrlead = 0\n", file = file, append = T)
    # first segment
    cat(">", label(segs1)[i], " ", mustart(segs1[i,  ]) - llead, 
	"  ", muend(segs1[i,  ]) + rlead, file = file, append
	= T)
    cat("   ", segs1[i, 3], "\n", file = file, append = T)
    # pad if requested
    if(pad) {
      cat("pad = ", pad, "\n", file = file, append = T)
    }
    # second segment
    cat(">", label(segs2)[i], " ", mustart(segs2[i,  ]) - llead, 
	"  ", muend(segs2[i,  ]) + rlead, file = file, append
	= T)
    cat("   ", segs2[i, 3], "\n}\n\n", file = file, append = T)
  }
  ##
  ## run the ppf program if asked to
  if(runppf) {
    cat(">>> Running PPF\n")
    unix(paste(ppfprogram, "1", file, ppfvardecs))
  }
}


# Local Variables:
# mode:S
# S-temp-buffer-p:t
# End:
