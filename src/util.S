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

# compatability and other functions for the R version of Emu/Splus

# are we running R?
# only needed if we aren't since R defines it
if( !exists("is.R") ) {

  is.R <- function() { 
    exists("version") && !is.null(vl <- version$language) && vl == "R"
  }

}


# system calls are different on each arch. and between S and R

emu.system <- function(cmd) {
  if (is.R()) {
    system(cmd)
  } else if (emu.platform("Windows")) {
    dos(cmd)
  } else if (emu.platform("Unix")) {
    unix(cmd)
  } else {
    stop("don't know how to run a system command -- fix emu.system")
  }
}

## tempfile is fine unless we're on windows where v3.3 can't grok
## long filenames, we need to ensure that when we add .dat and .tim
## on the end we still end up with an 8.3 filename	

emu.tempfile <- function(prefix) {
  if( emu.platform("Windows") ){
    tmp _ splitstring(tempfile(""), ".")
    tmp _ paste(tmp[1],tmp[2], sep="x")
    return(tmp)
  } else {
    return(tempfile(prefix))
  }
}


emu.platform <- function(what) {
  if( is.R() ) {
    if( version$platform == "Windows" || version$os == "Win32" ) {
      return(what == "Windows")
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



