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

  ## file.exists is terribly useful
  ## here's a crude version using ls/dir

  if( !exists("file.exists") ) {
    "file.exists" <- function(file) {
      if (emu.platform("Windows")) {
        r <- emu.system(paste("dir", file))
      } else if (emu.platform("Unix")) {
        r <- emu.system(paste("ls", file))
      }
      
      if (length(r) == 0) {
        return(F)
      } else {
        return(T)
      }
    }
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
## It seems that under R there's no such problem so we'll
## modify this to check for R as well as windows

emu.tempfile <- function(prefix) {
  if( !is.R() && emu.platform("Windows") ){
    tmp <- splitstring(tempfile(""), ".")
    tmp <- paste(tmp[1],tmp[2], sep="x")
    return(tmp)
  } else {
    return(tempfile(prefix))
  }
}

