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

"get.originalFreq" <- function (fname) {
  ##Original sampling frequency maybe stored at the top of the trackfile
  ##in this format: "# Original_Freq 20000"
  line <- readLines(fname, n=1)
  line <- splitstring(line, " ")
  if ((length(line) == 3) && (line[2] == "Original_Freq")) {
    origfreq <- as.numeric(line[3])
  } else {
    return(NULL)
  }
  if (origfreq > 0) {
    return(origfreq)
  } else {
    return(NULL)
  }
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



"emu.track" <-
function (seglist, trackname, cut = NULL, npoints = NULL, template = attr(seglist, 
    "database")) 
{

    if (!inherits(seglist, "emusegs")) 
        stop("emu.track only works for emu segment/event lists")
    if (is.null(template)) 
        stop("no template supplied and no default in segment list")
    nseg <- nrow(seglist)
    segfile <- emu.tempfile("emu.track")
    resultfile <- emu.tempfile("emu.track")
    on.exit(unlink(segfile), add = TRUE)
    on.exit(unlink(resultfile), add = TRUE)
    write.emusegs(seglist, segfile)
    if (!missing(cut)) {
        cut <- paste("-c", cut)
    }
    else {
        cut <- ""
    }
    if (!missing(npoints)) {
        npoints <- paste("-n", npoints)
    }
    else {
        npoints <- ""
    }
    if (is.R()) {
        currentwd <- getwd()
        if (dirname(template) != ".") {
            setwd(dirname(template))
        }
        else {
            setwd(dirname(emu.options("bindir")))
        }
    }
    command <- emu.command.name(emu.options("gettrack"), cut, 
        npoints, segfile, trackname, resultfile)
    opstat <- emu.system(command)
    if (is.R()) {
        setwd(currentwd)
    }
    if (!file.exists(resultfile)) {
        stop(paste("file ", resultfile, " doesn't exist in emu.track", 
            sep = ""))
    }
    type <- attr(seglist, "type")
    if (npoints == "" && (type == "event" || cut != "")) {
        result <- read.table(resultfile)
        if (ncol(result) > 1) {
            colnames <- paste(trackname, 1:ncol(result), sep = "")
        }
        else {
            colnames <- trackname
        }
        dimnames(result)[[2]] <- make.names(colnames)
     samfreq <- get.originalFreq(resultfile)
key <- get.trackkeywrd(resultfile)
if(any(key %in% c("dft", "css", "lps", "cep")))
{
    attr(result, "fs") <- seq(0, samfreq/2, length=ncol(data))
class(result) <- c(class(result), "spectral")
}
        return(result)
    }
    else {
        data <- read.trackdata(resultfile, trackname)
        if (nrow(data$ftime) != nseg) 
            stop("error in emu.track: mismatch between number of segments and number of data items read")
        return(data)
    }
}


## read.trackdata
##  read data from two files into a trackdata object, the files
##  contain the data and time components of the object, they're
##  produced by gettrack and friends
##
##  Emu 1.7 introduces a new format for this data which can be stored
##  in one file. Try to be backward compatible by first looking
##  for one file as named and if not present falling back to the
##  two file version, now called read.trackdata.twofile
## 
## The new format is:
## segno time data data data
## segno time data data data
"read.trackdata" <-
function (filename, trackname = "data") 
{
    if (!file.exists(filename)) {
        return(read.trackdata.twofile(filename, trackname))
    }
    data <- read.table(filename)
    segnumbers <- data[, 1]
    times <- data[, 2]
    data <- data[, c(-1, -2)]
    data <- as.matrix(data)
    dimnames(data) <- list(times, paste("T", 1:ncol(data), sep = ""))
    if (length(unique(segnumbers)) == 1) {
        index <- cbind(start = 1, end = length(segnumbers))
        ftime <- cbind(start = times[1], end = times[length(times)])
    }
    else {
        index <- cumsum(aggregate(data, by = list(segnum = segnumbers), 
            FUN = length)[, 2])
        index <- cbind(start = c(1, index[1:(length(index) - 
            1)] + 1), end = index)
        ftime <- cbind(start = times[index[, 1]], end = times[index[, 
            2]])
    }
    result <- as.trackdata(data, index, ftime, trackname)
samfreq <- get.originalFreq(filename)
key <- get.trackkeywrd(filename)
if(any(key %in% c("dft", "css", "lps", "cep")))
{
    attr(result$data, "fs") <- seq(0, samfreq/2, length=ncol(data))
class(result$data) <- c(class(result$data), "spectral")
}
    return(result)
}




"read.trackdata.twofile" <- function( basename, trackname="data" ) {

  datname <- paste( basename, "dat", sep=".")
  timname <- paste( basename, "tim", sep=".")
  
  if (!file.exists(datname) || !file.exists(timname) ) {
    ## give a suitable error message
    stop(paste("Can't find data file ", basename, " or ", basename, ".dat/", basename, ".tim in read.trackdata", sep=""))
  } 

  data <- read.table( datname )
  index <- read.table( timname )
 
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
                        matrix(c(1,index[1,1]), nrow=1),
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


"write.trackdata" <- function(trackdata, file) {
   n <- 0:(nrow(trackdata$index)-1)
   indices <- rep(n, trackdata$index[,2]-trackdata$index[,1]+1)
   omat <- NULL

   for(j in 1:nrow(trackdata$ftime)){
      N <- trackdata$index[j,2]-trackdata$index[j,1]+1
      ftimes <- trackdata$ftime[j,]
      omat <- c(omat, seq(ftimes[1], ftimes[2], length=N))
   }

   result <- cbind(indices, omat, trackdata$data)
   write(t(result), file, ncolumns=ncol(result))
}



"emu.query" <- function(template, pattern=NULL, query="" )
{


  segfile <- emu.tempfile( "emu.query" )
	
  on.exit( unlink( segfile ) )

  command <- emu.command.name( "emuquery", "-o", segfile, template,
                              emu.inquotes( pattern ),
                              emu.inquotes( query ) )

  if (is.R()) {
    ## change to the template directory to make relative paths work
    currentwd <- getwd()
    ## this is a simple test to see whether template is a
    ## filename or just a database name
    if ( dirname(template) != "." ) {
      setwd(dirname(template))
    } else {
      setwd(dirname(emu.options("bindir")))
    }
  }
    emu.system( command )
    
  if (is.R()) {
    setwd(currentwd)
  }

  if (!file.exists(segfile) ) {
    ## give a suitable error message
    stop(paste("Can't find the query results in emu.query: there may have been a problem with the query command.", sep=""))
  } 

  result <- read.emusegs( segfile )

  attr(result, "template") <- template
  return(result)
}


"emu.requery" <- function(segs, level, targetlevel=level,
                          justlabels=FALSE, sequence=0, longerok=FALSE )
{
  segfile <- emu.tempfile( "emu.track" )
	
  write.emusegs( segs, segfile )

  on.exit( unlink( segfile ) )

  resultfile <- emu.tempfile( "emurequery" )
  on.exit( unlink( resultfile ) )

  options <- ""
  if (justlabels)
    options <- paste( options, "-j" )
  if (sequence != 0)
    options <- paste( options, "-s", sequence )
  if (longerok != 0)
    options <- paste( options, "-l" )

  command <- emu.command.name( "emurequery", "-o", resultfile, options,
                              segfile, level, targetlevel )

  if (is.R()) {
    template <- emusegs.database(segs)
    ## change to the template directory to make relative paths work
    currentwd <- getwd()
    ## this is a simple test to see whether template is a
    ## filename or just a database name
    if ( dirname(template) != "." ) {
      setwd(dirname(template))
    } else {
      setwd(dirname(emu.options("bindir")))
    }
  }
  
  emu.system( command )
  
  if (is.R()) {
    setwd(currentwd)
  }

    if (!file.exists(resultfile) ) {
    ## give a suitable error message
    stop(paste("Can't find the requery results in emu.requery: there may have been a problem with the query command.", sep=""))
  } 

  
  if (justlabels) 
    result <- scan( resultfile, what="" )
  else {
    result <- read.emusegs( resultfile )
  }

  return(result)
}



# Local Variables:
# mode:S
# S-temp-buffer-p:t
# End:
