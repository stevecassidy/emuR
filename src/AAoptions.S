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


"emu.init" <- function() {

  packpath = path.package(package = "emu", quiet = FALSE)
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
            message( "X Library NOT linked to EMU installation !!!" )
            cat( "    Some functions that need database access notably emu.query()\n" )
            cat( "    and emu.track() will not work unless you have already \n" )
            cat( "    installed Emu (www.emu.sf.net). \n" )
            cat( "    This R Add-On works best with EMU version 2.4! You need at least EMU 2.2.4.\n" )
            message( "    Establish the link to your installed EMU System by calling emulink(). \n" )
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
           envir = .GlobalEnv )
  } 

  
  if( is.R() ) {

    library("stats")
  }
  if( class(tclfound) == "tclObj" ) {
    if( is.R() ) {

    # to write the emudirfile is not necessary if the files are in the standard directory
    # futher an error appears when users do not have permissions to write the file
    # writeLines(tcllibpaths, con = filepath)
    if (interactive()) {
      message( "V Library successfully linked to the Emu installation." )
      cat("\tTcl lib path of EMU Installation:", emu.options("libdir"))
      if (unlist(strsplit(tclvalue(.TclEval(sprintf("catch emuR::getdbs"))), " ")) == "0") {
       cat("\n\t\twith EMU/R tcl-library version: ", emurinfo())
       cat("\n\t\tand EMU tcl-library version: ", emuinfo())
      }
      message("\n\tFor configuration use emulink().\n\n")
    }
  }
  invisible()
}
}

emulink = function (paths="") {

  packpath = path.package(package = "emu", quiet = FALSE)
  sepa = .Platform$file.sep
  nfile = "emudirectory"
  filepath = paste(packpath,sepa,nfile,sep = "")
 
  if (paths == "") {
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
               message("Your configuration works already. \n")
               
            if (unlist(strsplit(tclvalue(.TclEval(sprintf("catch emuR::getdbs"))), " ")) == "0") {
              message("\twith EMU/R tcl-library version: ", emurinfo())
              message("\tand EMU tcl-library version: ", emuinfo())
            }
               cat("\n\nThus the directories in which the Emu libraries and tcllib >= 1.8 \n")
               cat("are installed on your system are one or some of these: \n", tclvalue(tcl("set", "auto_path")), "\n");
               if (file.exists(filepath)) {
                  message("The user defined tcl library path is written to", filepath,".")
               }
               message("Further configuration does not affect anything. If you like to add an additional tcl path use addTclPath().\n");
            }
            return(invisible());
          }  
      
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

 
      input <-  readline( "Emu Directory: " )      
      emu.directory <- input
      modified <- TRUE
      if( emu.directory == "" ) {
        crashout <- TRUE
       cat( "Link to Emu not established. \n")
       cat( "Report Bug to www.emu.sf.net with description of your problem. \n")
       cat( "Do not forget to give information about \nthe platform, Emu, R and EmuR versions you are running.\n" )
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
    message("\nThis information was written to the file\n",
        filepath,".\n")
    if (unlist(strsplit(tclvalue(.TclEval(sprintf("catch emuR::getdbs"))), " ")) == "0") {
        message("\n\tEMU/R tcl-library version: ", emurinfo())
        message("\n\tand EMU tcl-library version: ", emuinfo())
    }
    message( "\n\nLibrary successfully linked to the Emu installation. \n\n" )
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
           envir = .GlobalEnv )
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
  laa = get("emu",envir = .GlobalEnv)
  return(laa[[option]])
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

