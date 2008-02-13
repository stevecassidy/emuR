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

#  if (interactive()) {
#     cat("\nR Package of the EMU Speech Database system - Version ", emu.options("version"),
#         "\nBIN path of EMU Installation:", emu.options("bindir"),
#         "\n\n		(C) Institute of Phonetics and Speech Processing (IPS) \n University of Munich 2008\n\n")
#  }
  
  
 if (interactive()) {
   
   packpath = .path.package(package = "emu", quiet = FALSE)
  sepa = .Platform$file.sep
  nfile = "emudirectory"
  filepath = paste(packpath,sepa,nfile,sep = "")
  

cat("\n",
"\n     _  _________     _______     ",
"\n    | ||  ______ \\  / _______|    ",
"\n    | || | _____| || |_______     ",
"\n    | || ||______/  \\_______ \\    ",
"\n    | || | _   . . .   _ ___| |   ",
"\n    |_||_||_||.:.:.:.||_|____/    ",
"\n               : : :             ",
"\n      INSTITUTE OF PHONETICS      ",
"\n      AND SPEECH PROCESSING       ", 
"\n                                  ",
"\n(C) IPS, University of Munich 2008",
"\n                                  ", 
"\nR Package of the EMU Speech Database system - Version ", emu.options("version"),
"\n\nBIN path of EMU Installation:", emu.options("bindir"), 
"\nFor configuration, modify file",filepath, "\n",
"\nFor support requests, contact http://sourceforge.net/projects/emu \n")

}
 
  
  
  
  
  
  
  ## call user's .First.emu if present

  if( exists(".First.emu") )
    do.call(".First.emu", list(paste(thisdir, sectname, sep="/")))
  
  invisible(TRUE)
}


# Local Variables:
# mode:S
# S-temp-buffer-p:t
# End:
