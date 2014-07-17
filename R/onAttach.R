##############################################################################
##                                                                           #
##   copyright            : (C) 2012 IPS, LMU Munich                         #
##   email                : Steve.Cassidy@mq.edu.au			     #
##   url	          : http://www.emu.sourceforge.net                   #
##									     #
##   This program is free software; you can redistribute it and/or modify    #
##   it under the terms of the GNU General Public License as published by    #
##   the Free Software Foundation; either version 2 of the License, or       #
##   (at your option) any later version.                                     #
##									     #
##############################################################################



".onAttach"<- function(libname, pkgname)
{
 
  if (interactive()) {
    
    packpath = path.package(package = "emuR", quiet = FALSE)
    sepa = .Platform$file.sep
    nfile = "emudirectory"
    filepath = paste(packpath,sepa,nfile,sep = "")
    mess = paste("\n \n     _  _________     _______    \n    | ||  ______ \\  / _______|    \n    | || | _____| || |_______     \n    | || ||______/  \\_______ \\    \n    | || | _   . . .   _ ___| |   \n    |_||_||_||.:.:.:.||_|____/            \n               : : :             \n      INSTITUTE OF PHONETICS      \n      AND SPEECH PROCESSING        \n                                  \n(C) IPS University of Munich\n\nR Package of the EMU Speech Database system - Version",packageVersion("emuR")," \nFor support see https://github.com/IPS-LMU/emuR \n")    
   packageStartupMessage(mess, appendLF = FALSE)
   packageStartupMessage( "\nLibrary successfully loaded." )
  }
}





emudata.init <- function() {
   messemudatamore = ""
  if(require(emudata)) {
    
    messemudata="V Additional datasets successfully loaded from package emudata."
  } else {
    messemudata="X NO additional databases loaded."
    messemudatamore="    To have access to all datasets used in \n    Harrington, J. (2010). The Phonetic Analysis of Speech Corpora. Blackwell,\n    install package emudata."
  }
  if (interactive()) {
    message(messemudata)
    cat(messemudatamore)
  }
}




## Local Variables:
## mode:S
## End:
