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

## Modify the value of emu.directory to match your Emu installation, for
## example if you have Emu installed on your Windows machine under
## C:\My Programs\Emu then change the value of 'emu.directory' below to
## "C:\\My Programs\\Emu" (note that you must always double the slashes
## for Splus).

if (version$platform == "Windows" ||
    version$platform == "WIN386" ||
    version$os == "Win32") {

  ## change this value for windows
  emu.directory <- "C:\\Program Files\\Emu"

} else {

  ## change this value for unix
  emu.directory <- "/usr/local"

}

### ------------------------------------------------------------------------
### ----------There are no more user configurable values in this file ------
### ------------------------------------------------------------------------


## this will get changed by the Makefile
emu.version<-"1.4"

"emu.init" <- function() {

  if (emu.platform("Windows")) {
    dirsep <- "\\"
    gettrack <- "gettrack.exe"
  } else {
    dirsep <- "/"
    gettrack <- "gettrack"
  }
  bindir <- paste(emu.directory, "bin", sep=dirsep)                   

  ## test emu.directory to find one of the executables, query if
  ## it isn't found
  crashout <- FALSE
  modified <- FALSE
  while( !crashout  &&
        !file.exists( paste( bindir, gettrack, sep=dirsep ) ) ) {

    cat( "Configuration error: emu.directory (", emu.directory, ") doesn't\n" )
    cat( "correspond to an Emu installation.\n" )
    cat( "Please enter the directory in which Emu is installed\n" )
    cat( "on your system, eg. C:\\Program Files\\Emu on Windows \n" )
    cat( "or /usr/local on Unix.\n\n" )
    cat( "Enter nothing and press return to exit this configuration loop.\n\n")

    ## we assign to the global namespace to make sure this is retained
    emu.directory <<-  readline( "Emu Directory: " )
    modified <- TRUE
    if( emu.directory == "" ) {
      crashout <- TRUE
    } else {
      bindir <- paste(emu.directory, "bin", sep=dirsep)
    }
  }
  if( crashout ) { return( FALSE ) }

  if( modified && is.R() ) {
    cat( "\nYou can avoid this dialog by modifying the variable\n",
        "emu.directory in the emu library source file which can be found in\n",
        "the emu/R subdirectory of the R installation (", .lib.loc, ").\n"
        )
  }

  if( is.R() ) {
    assign( "emu", 
         list( 
              version=emu.version,
              deprecated.warnings=1,
              bindir=bindir,
              dirsep=dirsep,
	      gettrack=gettrack
              ),
           env = .GlobalEnv )
  } else {
    assign( "emu",
           list( 
                version=emu.version,
                deprecated.warnings=1,
                bindir=bindir,
                dirsep=dirsep,
                gettrack=gettrack
                ),
           frame=0)
  }

  
  if( is.R() ) {
    ## eda is needed for smooth, used in dsmooth
    library("eda")
  }

  invisible()
}

"emu.options" <- function( option ) {
    emu[[option]]
}

