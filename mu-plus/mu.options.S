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

"mu.options" <- function(default=F, ...)
{
  args <- list(...)

  ## the place where mu is installed
  if( is.null(args$muhome) ){
    if(default) 
      options(muhome=paste(getenv("SHOME"), "library/mu", sep="/"))
  }
  else
    options(muhome=args$muhome)

  ## the default extension assumed for label files
  if( is.null(args$labextn) ){
    if(default) 
      options(labextn="lab")
  }
  else
    options(labextn=args$labextn)

  ## the default database location for mkdb etc
  if( is.null(args$dbdefault) ){
    if(default)
      options(dbdefault=paste(options()$muhome, "dbase", sep="/"))
    }
  else
    options(dbdefault=args$dbdefault)

  ## Playback of speech data
  if( is.null(args$internalspeaker) ){
    if(default) 
      options(internalspeaker=T)   
  }
  else
    options(internalspeaker=args$internalspeaker )

  ## the default location of the labed_template and tts files
  if( is.null(args$templatedir) ){
    if(default)
      options(templatedir=paste(options()$muhome, "ttsfiles", sep="/"))
    }
  else
    options(templatedir=args$templatedir )


}


# Local Variables:
# mode:S
# S-temp-buffer-p:t
# End:
