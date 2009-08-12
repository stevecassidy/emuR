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
  emu.directory <- "/usr/local/Emu/lib"

}

### ------------------------------------------------------------------------
### ----------There are no more user configurable values in this file ------
### ------------------------------------------------------------------------


## this will get changed by the Makefile
emu.version<-"4.1"
emu.date<-"2009-08-12"

emu.year<-unlist(strsplit(emu.date, "-"))[1]

"emu.init" <- function() {

  packpath = .path.package(package = "emu", quiet = FALSE)
  sepa = .Platform$file.sep
  nfile = "emudirectory"
  filepath = paste(packpath,sepa,nfile,sep = "")
 
  # is tcltk available
   if ( class( try( library( "tcltk" ), silent=TRUE )) == "try-error" ) {
    if (interactive()) {
    cat( "Error trying to load tcltk. Make sure your R version is\n" )
    cat( "tcltk enabled.\n")
    cat( "On Macintosh OS X try loading X11\n")
    return(invisible());
    }
  }
  
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
      tcllibpaths <- c("/Library/Tcl",paste(Sys.getenv("HOME"),"/Library/Tcl",sep=""))
      #tcllibpaths <- ""
    } else if (emu.platform("Windows")) {
      tcllibpaths <- "C:\\Program Files\\Emu\\lib"
    } else if (emu.platform("Unix")) {
      tcllibpaths <- c("/usr/local/Emu/lib",Sys.getenv("HOME"))
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

  for (path in tcllibpaths) {
    addTclPath(path)
  }
  tclfound <- tclRequire( "emuR" ,warn = FALSE )
  
  if (interactive()) {
    if( !crashout  && class(tclfound) != "tclObj" ) {
    if (interactive()) {
    cat( "\nLibrary successfully loaded. \n" )
    cat( "However, some functions that interface to the database notably emu.query() " )
    cat( "and emu.track() will not work unless you have already " )
    cat( "installed Emu (www.emu.sf.net). \n" )
    cat( "This R Add-On will not work with EMU version <= 2.0prev!\n" )
    cat( "If Emu is installed on your system, run the program " )
    cat( "emulink() to establish the link to Emu. \n" )
    }
    }
  }
  
 
  if( is.R() ) {
    assign( "emu", 
           list( 
                version=emu.version,
                deprecated.warnings=1,
                libdir=tcllibpaths,
                year=emu.year
                ),
           env = .GlobalEnv )
  } 

  
  if( is.R() ) {

    library("stats")
  }
  if( class(tclfound) == "tclObj" ) {
    if( is.R() ) {

    writeLines(tcllibpaths, con = filepath)
      if (interactive()) {
    cat("\nTcl lib path of EMU Installation:", emu.options("libdir"))
    cat("\nFor configuration, modify file",filepath,"or use emulink().\n\n")
    cat( "Library successfully loaded and linked to the Emu installation. \n\n" )
    }
  }
  invisible()
}
}

emulink = function (paths="") {

  packpath = .path.package(package = "emu", quiet = FALSE)
  sepa = .Platform$file.sep
  nfile = "emudirectory"
  filepath = paste(packpath,sepa,nfile,sep = "")
 
 # is tcltk available
   if ( class( try( library( "tcltk" ), silent=TRUE )) == "try-error" ) {
     if (interactive()) {
    cat( "Error trying to load tcltk. Make sure your R version is\n" )
    cat( "tcltk enabled. See capabilities(\"tcltk\").\n")
    cat( "On Macintosh OS X try loading X11\n")
    }
    return(invisible());
}
tclfound <- tclRequire( "emuR" ,warn = FALSE )   
   if(class(tclfound) == "tclObj" ) {
     if (interactive()) {
     cat("Your configuration works already. \n")
     cat("Thus the directories in which the Emu libraries and tcllib >= 1.8 \n")
     cat("are installed on your system are one or some of these: \n", tclvalue(tcl("set", "auto_path")), "\n");
     cat("Further configuration does not affect anything. If you like to add an additional tcl path use addTclPath().\n");
     }
 return(invisible());
}  
if (paths == "") {
 # get current configuration
  if (file.exists(filepath)) {
    tcllibpaths <- readLines(con = filepath)
    w <- tcllibpaths!=""
    tcllibpaths <- tcllibpaths[w]
      if (interactive()) {
        cat("Current configuration:", tcllibpaths, " "); 
      }
   for (path in tcllibpaths) {
    addTclPath(path)
   }
   
   tclfound <- tclRequire( "emuR" ,warn = FALSE )   
   if(class(tclfound) != "tclObj" ) {
     if (interactive()) {
     cat("does not work. Will try default configuration. \n");
     }
    tcllibpaths=character(0)
   } 
  } else {
    if (interactive()) {
    cat("No configuration available yet.\n");
    }
    tcllibpaths=character(0)
  }
  
 # if the current configuration does not work get the default configuration
  if (length(tcllibpaths)==0) {
    if (emu.platform("MacOS")) {
      user <- Sys.getenv("USER")
      tcllibpaths <- c("/Library/Tcl",paste(Sys.getenv("HOME"),"/Library/Tcl",sep=""))
    } else if (emu.platform("Windows")) {
      tcllibpaths <- "C:\\Program Files\\Emu\\lib"
    } else if (emu.platform("Unix")) {
      tcllibpaths <- c("/usr/local/Emu/lib",Sys.getenv("HOME"))
    } else {
      stop("Panic! I can't work out what platform I'm running on!\
Please report this as a bug at www.emu.sf.net including\
details of the operating system you are running on and the value of\
the variable 'version'.")
    }
    if (interactive()) {
     cat("Default configuration:",tcllibpaths, " ")
    }
  }



  for (path in tcllibpaths) {
    addTclPath(path)
  }
  tclfound <- tclRequire( "emuR" ,warn = FALSE )
  if( class(tclfound) == "tclObj" ) {
  if (interactive()) {
    cat(" works. \n")
  }
  }

} else {
  tcllibpaths = paths
  cat("Your configuration ")
  for (path in tcllibpaths) {
    addTclPath(path)
  }
  tclfound <- tclRequire( "emuR" ,warn = FALSE )
  if( class(tclfound) == "tclObj" ) {
    if (interactive()) {
    cat("works. \n")
    }
  }


}
  ## test emu.directory to find one of the executables, query if
  ## it isn't found
  crashout <- FALSE
  modified <- FALSE

  
  if (interactive()) {
    while( !crashout  && class(tclfound) != "tclObj" ) {
      cat("does not work.\n")
      cat( "Configuration error: Cannot find Emu Tcl libraries in\n" )
      cat( tcllibpaths, sep=" or " )
      cat( "\n" )
      cat( "Please enter the directories seperated by \";\" in which\n" )
      cat( "Emu libraries and tcllib >= 1.8 are installed on your system\n" )
      cat( "e.g.:", paste(emu.defaultpaths(),collapse=";"), "\n")
      if (paths != "") {cat("You can try emulink() without path argument to set the default configuration given above.\n")}
      cat( "Enter nothing and press return to exit this configuration loop.\n" )
      cat( "\n")

 
      emu.directory <<-  readline( "Emu Directory: " )
      
      modified <- TRUE
      if( emu.directory == "" ) {
        crashout <- TRUE
       cat( "Link to Emu not established. \n")
       cat( "Report Bug to www.emu.sf.net with description of your problem. ")
       cat( "Do not forget to give information about the platform and Emu R and EmuR versions you are running.\n" )
       cat( "Try emulink() again to establish the link to Emu. \n" )    
       
      } else {
        tcllibpaths=strsplit( emu.directory, ";" )[[1]]
        for (path in tcllibpaths) {
          addTclPath(path)
        }
        tclfound <- tclRequire( "emuR", warn = TRUE )
      }
    }
  }
  
  if( crashout ) { return(invisible()); }
  if( is.R() ) {

    writeLines(tcllibpaths, con = filepath)
      if (interactive()) {
    cat("\nThis information was written to the file\n",
        filepath,".\n") 
  
    cat( "Library successfully linked to the Emu installation. \n\n" )
 }
  }



  if( is.R() ) {
    assign( "emu", 
           list( 
                version=emu.version,
                deprecated.warnings=1,
                libdir=tcllibpaths,
                year=emu.year
                ),
           env = .GlobalEnv )
  } 

  
  if( is.R() ) {

    library("stats")
  }

  invisible()
}

"emu.defaultpaths" = function () {
    if (emu.platform("MacOS")) {
      user <- Sys.getenv("USER")
      tcllibpaths <- c("/Library/Tcl",paste(Sys.getenv("HOME"),"/Library/Tcl",sep=""))
    } else if (emu.platform("Windows")) {
      tcllibpaths <- "C:\\Program Files\\Emu\\lib"
    } else if (emu.platform("Unix")) {
      tcllibpaths <- c("/usr/local/Emu/lib",Sys.getenv("HOME"))
    } else {
      tcllibpaths = "NA"
    }
    return(tcllibpaths)
}
"emu.options" <- function( option ) {
  laa = get("emu",env = .GlobalEnv)
  return(laa[[option]])
}

