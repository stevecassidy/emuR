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

"muppf"<-
function(segs, file = "segs", snr = 0, warntone = F, warn = 1000, delay = 5000, 
	 noise = -1, space = 1000, runppf = T, llead = 0, rlead = 0, mllead = 0, 
	 mrlead = 0, normal = 1)
{

  ppfprogram <- paste(options()$muhome, "ppf/ppf", sep="/" )
  ppfvardecs <- paste(options()$muhome, "ppf/variable-declarations", sep="/")


  ## the snr variable
  cat("snr = ", snr, "\n", file = file)	#
  ## put out 1 second of silence
  cat("# 1 second of silence\n{L\ndelay = 1000\n> - 0 0 file0\n}\n\n", 
      file = file, append = T)	#
  ## loop around, and send out the data to the file
  for(i in 1:slen(segs)) {
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
    cat("# a real segment\n{L\ndelay = ", delay, "\n", file = file, append = T)
    cat("snr = ", snr, "\n", file = file, append = T)
    cat("normal = ", normal, "\n", file = file, append = T)
    cat("noise = ", noise, "\n", file = file, append = T)
    cat("llead = ", llead, "\n", file = file, append = T)
    cat("rlead = ", rlead, "\n", file = file, append = T)
    cat("mllead = ", mllead, "\n", file = file, append = T)
    cat("mrlead = ", mrlead, "\n", file = file, append = T)
    cat(">", label(segs)[i], " ", mustart(segs[i,  ]) - llead, "  ",
	muend(segs[i,  ]) + rlead, file = file, append = T)
    cat("   ", utt(segs[i, ]), "\n}\n\n", file = file, append = T)
  }

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
