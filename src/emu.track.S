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


"emu.inquotes" <- function( string ) {
  return( paste( "\"", string , "\"", sep="" ) )
}

## construct a command line given a name and a list of arguments
## put quotes around the path since it may contain spaces on windows
"emu.command.name" <- function( name, ... )
{
  # first arg is command name
  command <- emu.inquotes( paste(emu.options("bindir"),
                                 emu.options("dirsep"),
                                 name,
                                 sep="" ) )
  arguments <- list(...)
  for( arg in arguments ) {
    command <- paste( command, arg, sep=" " )
  }
  return( command )
}



"emu.track"<- function( seglist, trackname, cut=NULL, 
		       template=attr(seglist, "database")) 
{
  if( !inherits(seglist,"emusegs") ) 
    stop( "emu.track only works for emu segment/event lists" ) 

  if( is.null(template) ) 
    stop( "no template supplied and no default in segment list" )

  nseg _ nrow(seglist)

  segfile <- emu.tempfile( "emu.track" )
  resultbase <- emu.tempfile( "emu.track" )
  
  on.exit( unlink( segfile ), add=TRUE )
  on.exit( unlink( paste( resultbase, ".dat", sep="") ), add=TRUE ) 
  on.exit( unlink( paste( resultbase, ".tim", sep="") ), add=TRUE ) 
  
  museg.write( seglist, segfile )
  
  if(!missing(cut)) {
    cut <- paste( "-c", cut )
  } else {
    cut <- ""
  }
  
  command <- emu.command.name( emu.options("gettrack"),
                              cut,
                              segfile, trackname, resultbase )

#  cat( "Command: ", command, "\n" )
  opstat _ emu.system( command )

#  if( opstat != 0 ) stop("problem with get_track command")

  type <- attr(seglist, "type" )


  ## now read in the time/index data and the data 
  if(type == "event" || cut != "" ) {
    ## just read in the data and return it as a matrix
    as.matrix(read.table( paste( resultbase, "dat", sep=".") ))
  } else {
    data <- read.trackdata( resultbase ) 

    ## a bit of validation
    if( nrow(data$index) != nseg )
      stop( "error in emu.track: not enough data was read" )
    return( data )
  }
}

## read.trackdata
##  read data from two files into a trackdata object, the files
##  contain the data and time components of the object, they're
##  produced by get_track and friends
"read.trackdata" <- function( basename, trackname="data" ) {

  data _ read.table( paste( basename, "dat", sep=".") )
  index _ read.table( paste( basename, "tim", sep=".") )
 
  # the number of segments is the number of rows in the index file
  nseg <- nrow( index )

  ## the sum of the first index column should correspond to
  ## the length of the data
  if( sum(index[,1]) != nrow( data ) ) 
    stop("error in emu.track: not enough data was read" )
  
  ## index[,1] is the lengths of every segment, convert to trackdata index 
  ## which is start and end point of every segment
  ## index[,2:3] is the ftime component of the trackdata object
  
  if (nseg == 1) {
    return( as.trackdata(as.matrix(data),
                        as.matrix(c(1,index[1,1])),
                        as.matrix(index[1,2:3]), 
                        trackname) )
  } else {
    return( as.trackdata(as.matrix(data),
                        cbind(c(1,cumsum(index[1:(nseg-1),1])+1), 
                              cumsum(index[,1])),
                        as.matrix(index[,2:3]), 
                        trackname))
  }
}


"emu.query" <- function(template, pattern=NULL, query="" )
{

  segfile <- emu.tempfile( "emu.query" )
	
  on.exit( unlink( segfile ) )

  if( missing(pattern) ) {
    command <- emu.command.name( "hquery", "stdout", template, ">", segfile )
  } else {
    command <- emu.command.name( "emuquery", "-o", segfile, template,
                                emu.inquotes( pattern ),
                                emu.inquotes( query ) )
  }

#  cat( command, "\n" )
  emu.system( command )

  result <- read.segs( segfile )

  attr(result, "template") <- template
  return(result)
}


"emu.requery" <- function(segs, level, targetlevel=level,
                          justlabels=F, sequence=0, longerok=F )
{
  segfile <- emu.tempfile( "emu.track" )
	
  museg.write( segs, segfile )

  on.exit( unlink( segfile ) )

  resultfile <- emu.tempfile( "emu_requery" )
  on.exit( unlink( resultfile ) )

  options <- ""
  if (justlabels)
    options <- paste( options, "-j" )
  if (sequence != 0)
    options <- paste( options, "-s", sequence )
  if (longerok != 0)
    options <- paste( options, "-l" )

  command <- emu.command.name( "emu_requery", "-o", resultfile, options,
                              segfile, level, targetlevel )

  cat( command, "\n" )
  emu.system( command )
  
  if (justlabels) 
    result <- scan( resultfile, what="" )
  else {
    result <- read.segs( resultfile )
  }

  return(result)
}



# Local Variables:
# mode:S
# S-temp-buffer-p:t
# End:
