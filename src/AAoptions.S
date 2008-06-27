##############################################################################
##                                                                           #
##   copyright            : (C) 2000 SHLRC, Macquarie University             #
##   email                : Steve.Cassidy@mq.edu.au			     #
##   url			 : http://www.shlrc.mq.edu.au/emu	     #
##								             #
##   This program is free software; you can redistribute it and/or modify    #
##   it under the terms of the GNU General Public License as published by    #
##   the Free Software Foundation; either version 2 of the License, or       #
##   (at your option) any later version.                                     #
##                                                                           #
##############################################################################



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


## Modify the value of emu.directory to match your Emu installation, for
## example if you have Emu installed on your Windows machine under
## C:\My Programs\Emu then change the value of 'emu.directory' below to
## "C:\\My Programs\\Emu" (note that you must always double the slashes
## for Splus).

if (emu.platform("Windows")) {

  emu.directory <- "C:\\Program Files\\Emu"

} else if ( emu.platform("MacOS") ) {
  ## macos can't exec so it doesn't really matter
  emu.directory <- ""
} else {

  ## change this value for unix
  emu.directory <- "/usr/local"

}

### ------------------------------------------------------------------------
### ----------There are no more user configurable values in this file ------
### ------------------------------------------------------------------------


## this will get changed by the Makefile
emu.version<-"3.3"

"emu.init" <- function() {

  packpath = .path.package(package = "emu", quiet = FALSE)
  sepa = .Platform$file.sep
  nfile = "emudirectory"
  filepath = paste(packpath,sepa,nfile,sep = "")
  
  if (file.exists(filepath)) {
    tcllibpaths <- readLines(con = filepath)
    w <- tcllibpaths!=""
    tcllibpaths <- tcllibpaths[w]
  } else {
    tcllibpaths=character(0)
  }

  if (length(tcllibpaths)==0) {
    if (emu.platform("MacOS")) {
      user <- Sys.getenv("USER")
      tcllibpaths <- c("/Library/Tcl",paste("/Users/",user,"/Library/Tcl",sep=""))
    } else if (emu.platform("Windows")) {
      tcllibpaths <- "C:\\Program Files\\Emu\\lib"
    } else if (emu.platform("Unix")) {
      tcllibpaths <- "/usr/local/lib"
    } else {
      stop("Panic! I can't work out what platform I'm running on!\
Please report this as  a bug at http://sf.net/projects/emu including\
details of the operating system you are running on and the value of\
the variable 'version'.")
    }
  }
  

  ## test emu.directory to find one of the executables, query if
  ## it isn't found
  crashout <- FALSE
  modified <- FALSE

  if ( class( try( library( "tcltk" ), silent=TRUE )) == "try-error" ) {
    cat( "Error trying to load tcltk. Make sure your R version is\n" )
    cat( "tcltk enabled.\n")
    cat( "On Macintosh OS X try loading X11\n")
    return( FALSE )
  }
  for (path in tcllibpaths) {
    addTclPath(path)
  }
  tclfound <- tclRequire( "emuR" ,warn = FALSE )
  
  if (interactive()) {
    while( !crashout  && class(tclfound) != "tclObj" ) {

      cat( "Configuration error: Cannot find Emu Tcl libraries in\n" )
      cat( tcllibpaths, sep=" or " )
      cat( "\n" )
      cat( "Please enter the directories seperated by \";\" in which\n" )
      cat( "Emu libraries and tcllib >= 1.8 are installed on your system\n" )
      cat( "eg. C:\\Program Files\\Emu\\lib on Windows\n" )
      cat( "or /usr/local/lib on Unix.\n\n" )
      cat( "Enter nothing and press return to exit this configuration loop.\n" )
      cat( "\n")

      ## we assign to the global namespace to make sure this is retained

      emu.directory <<-  readline( "Emu Directory: " )
      
      modified <- TRUE
      if( emu.directory == "" ) {
        crashout <- TRUE
      } else {
        tcllibpaths=strsplit( emu.directory, ";" )[[1]]
        for (path in tcllibpaths) {
          addTclPath(path)
        }
        tclfound <- tclRequire( "emuR", warn = TRUE )
      }
    }
  }
  
  if( crashout ) { return( FALSE ) }
  if( modified && is.R() ) {

    writeLines(tcllibpaths, con = filepath)
    cat("\nThis information was written to the file\n",
        filepath,".\n") 
    
  }



  if( is.R() ) {
    assign( "emu", 
           list( 
                version=emu.version,
                deprecated.warnings=1,
                libdir=tcllibpaths
                ),
           env = .GlobalEnv )
  } 

  
  if( is.R() ) {

    library("stats")
  }

  invisible()
}


"emu.options" <- function( option ) {
  laa = get("emu",env = .GlobalEnv)
  return(laa[[option]])
}

