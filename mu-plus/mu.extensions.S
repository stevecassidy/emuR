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

"mu.extensions" <- function(option)
{
  ## option can be one of "ESPS", "SSFF" or "ACCOR", the 
  ## "extensions" option is set to the appropriate extensions list
  if(option=="ESPS")
    options(extensions=c( "sd", "sgram", "f0", "fb", "fpwr", "phys" ))
  else if(option=="SSFF")
    options(extensions= c( "SSFF_sd", "SSFF_sgram", "SSFF_f0",
			    "SSFF_fb", "SSFF_fpwr", "SSFF_phys" ))
  else if(option=="ACCOR")
    options(extensions=c("SSFF_sd", "SSFF_sgram", "SSFF_f0", "SSFF_fb",
			    "SSFF_epg", "SSFF_lx", "SSFF_nasa", "SSFF_orala" ))
  else
    stop(paste("Option", option, "not understood in mu.extensions"))

  cat("Default extensions set to ", option, "\n")

  invisible()
}

# Local Variables:
# mode:S
# S-temp-buffer-p:t
# End:
