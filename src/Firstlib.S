#############################################################################
#                                                                           #
#   copyright            : (C) 2000 SHLRC, Macquarie University             #
#   email                : Steve.Cassidy@mq.edu.au			    #
#   url			 : http://www.shlrc.mq.edu.au/emu		    #
#									    #
#   This program is free software; you can redistribute it and/or modify    #
#   it under the terms of the GNU General Public License as published by    #
#   the Free Software Foundation; either version 2 of the License, or       #
#   (at your option) any later version.                                     #
#									    #
#############################################################################


".First.lib"<- function(thisdir, sectname)
{

  ## setup defaults 
  emu.init()

  cat("\nEMU Speech Database system \n",
      "Version ", emu.options("version"),
      "(C) SHLRC, Macquarie University, 1998\n\n")
  
  ## call user's .First.emu if present

  if( exists(".First.emu") )
    do.call(".First.emu", list(paste(thisdir, sectname, sep="/")))
  
  invisible()
}


# Local Variables:
# mode:S
# S-temp-buffer-p:t
# End:
