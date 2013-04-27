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

    ### ------------------------------------------------------------------------
    ### ----------There are no more user configurable values in this file ------
    ### ------------------------------------------------------------------------
    
    
    ## this will get changed by the Makefile
emu.version<-"4.4"
emu.date<-"2013-04-28"
    
emu.year<-unlist(strsplit(emu.date, "-"))[1]


## we need a canonical way of working out what platform we're running
## on, R and S differ and even versions of R have different os settings...
emu.platform <- function(what) {
  if( is.R() ) {
    if(version$platform == "Windows" ||
       version$os == "Win32" ||
       version$os == "mingw32") {
      return(what == "Windows")
    } else if (
	version$os == "MacOS" ||
	substr(version$os,0,6) == "darwin" ) {
      return(what == "MacOS")
    } else {
      return(what == "Unix")
    }
  } else {
    if( version$platform == "WIN386" ) {
      return(what == "Windows")
    } else {
      return(what == "Unix")
    }
  }
}


".onAttach"<- function(libname, pkgname)
{
 
    ## Modify the value of emu.directory to match your Emu installation, for
    ## example if you have Emu installed on your Windows machine under
    ## C:\My Programs\Emu then change the value of 'emu.directory' below to
    ## "C:\\My Programs\\Emu" (note that you must always double the slashes
    ## for Splus).
    
    if (emu.platform("Windows")) {
    
      emu.directory <- "C:\\Program Files\\Emu"
    
    } else if ( emu.platform("MacOS") ) {
      ## macos can't exec so it doesn't really matter
      emu.directory <- "/Applications/Emu.app/Contents/lib"
    } else {
    
      ## change this value for unix
      emu.directory <- "/usr/local/Emu/lib"
    
    }
    

  ##  if (interactive()) {
  ##     cat("\nR Package of the EMU Speech Database system - Version ", emu.options("version"),
  ##         "\nBIN path of EMU Installation:", emu.options("bindir"),
  ##         "\n\n		(C) Institute of Phonetics and Speech Processing (IPS) \n University of Munich 2012\n\n")
  ##  }
  
 
  
  if (interactive()) {
    
    packpath = path.package(package = "emu", quiet = FALSE)
    sepa = .Platform$file.sep
    nfile = "emudirectory"
    filepath = paste(packpath,sepa,nfile,sep = "")
    mess = paste("\n \n     _  _________     _______    \n    | ||  ______ \\  / _______|    \n    | || | _____| || |_______     \n    | || ||______/  \\_______ \\    \n    | || | _   . . .   _ ___| |   \n    |_||_||_||.:.:.:.||_|____/            \n               : : :             \n      INSTITUTE OF PHONETICS      \n      AND SPEECH PROCESSING        \n                                  \n(C) IPS University of Munich"  ,emu.year,"\n\nR Package of the EMU Speech Database system - Version  ",emu.version," \nFor support requests contact http://emu.sourceforge.net \n")    
   packageStartupMessage(mess, appendLF = FALSE)
   packageStartupMessage( "\nV Library successfully loaded." )
  }


emudata.init()
  
  ## setup defaults 
  emu.init()
  ## call user's .First.emu if present

  if( exists(".First.emu") )
    do.call(".First.emu", list(paste(libname, pkgname, sep="/")))
  
  invisible(TRUE)
}


## Local Variables:
## mode:S
## End:
