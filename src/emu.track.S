#############################################################################
##                                                                         ##
##   copyright            : (C) 2000 SHLRC, Macquarie University           ##
##   email                : Steve.Cassidy@mq.edu.au		           ##
##   url			 : http://www.shlrc.mq.edu.au/emu	   ##
##									   ##
##   This program is free software; you can redistribute it and/or modify  ##
##   it under the terms of the GNU General Public License as published by  ##
##   the Free Software Foundation; either version 2 of the License, or     ##
##   (at your option) any later version.                                   ##
##									   ##
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


"emu.track" <- function(seglist, trackname, cut = NULL, npoints = NULL,
                        template = attr(seglist, "database"))
{
  
  s <- as.matrix(seglist)
  nseg=nrow(seglist)
  ## create a seglist in tcl
  if (missing(cut))
    cutV <- -1.0
  else
    cutV <- cut
  if (missing(npoints))
    npointsV=1
  else
    npointsV=npoints
  TclSegs <- .TclEval(sprintf("set segs [emuR::seglist %%AUTO%% -type %s -database %s -query %s]",
                             emusegs.type(seglist), emusegs.database(seglist),
                             .emu.inbrackets(emusegs.query(seglist))))
  on.exit(.TclEval(sprintf("$%s destroy",as.character(TclSegs))))
  ## fill the Tcl segment list with data from the R seglist
  for(i in c(1:nrow(seglist))) {
    res <- .Tcl(sprintf("$%s append [list %s %s %s %s]",
                        as.character(TclSegs),
                        s[i,1],s[i,2],s[i,3],
                        s[i,4]))
  }
  ## create a tracklist in tcl, a snit object created from within the seglist
  TclTrack <- .Tcl(sprintf("set t [$%s gettrack -t %s -c %s -n %s]",
                           as.character(TclSegs),
                           trackname, cutV, npointsV))
  ##appearently, we don't need to destroy TclTrack, because it will be deleted
  ##when deleting TclSegs (its parent)
  ##on.exit(.TclEval(sprintf("$%s destroy",as.character(TclTrack))),add=T)
  ## get data from tcl (one string!)
  res = .Tcl(paste("join [",as.character(TclTrack)," cget -data]",sep=""))
  samfreq = .TclNum(.Tcl(paste(as.character(TclTrack),
    " cget -origfreq",sep="")))
  numrecs = .TclNum(.Tcl(paste(as.character(TclTrack),
    " cget -numrecs",sep="")))
  key = .TclString(.Tcl(paste(as.character(TclTrack),
    " cget -trackname", sep="")))
  strmat <- as.numeric(res)
  type <- attr(seglist, "type")

  result <- NULL
  result <- matrix(strmat,nrow=numrecs,byrow=TRUE)

  ## create a simple matrix for event kind of data (one sample)
  if (missing(npoints) &&  (type == "event" || !missing(cut))) {
    result=data.frame(result)
    if (ncol(result) > 1) {
      colnames <- paste(trackname, 1:ncol(result), sep = "")
    }
    else {
      colnames=trackname
    }
    dimnames(result)[[2]] <- make.names(colnames)
    if(any(key %in% c("dft", "css", "lps", "cep")))
      {
        attr(result, "fs") <- seq(0, samfreq/2, length=ncol(result))
        class(result) <- c(class(result), "spectral")
      }
    return(result)
  }
  else {
    ##complicated stuff, real track data objects
    segnumbers <- result[,1]
    times <- result[, 2]
    data <- result[, c(-1, -2)]
    data <- as.matrix(data)
    dimnames(data) <- list(times, paste("T", 1:ncol(data), sep = ""))
    if (length(unique(segnumbers)) == 1) {
      index <- cbind(start = 1, end = length(segnumbers))
      ftime <- cbind(start = times[1], end = times[length(times)])
    }  else {
      index <- cumsum(aggregate(data, by = list(segnum = segnumbers), 
                                FUN = length)[, 2])
      index <- cbind(start = c(1, index[1:(length(index) - 
                       1)] + 1), end = index)
      ftime <- cbind(start = times[index[, 1]], end = times[index[, 
                                                  2]])
    }
    result <- as.trackdata(data, index, ftime, trackname)
    if(any(key %in% c("dft", "css", "lps", "cep")))
      {
        attr(result$data, "fs") <- seq(0, samfreq/2, length=ncol(data))
        class(result$data) <- c(class(result$data), "spectral")
      }
    if (nrow(result$ftime) != nseg) {
      cat(paste(nrow(result$ftime), nseg))
      stop("error in emu.track: mismatch between number of segments and number of data items read")
    }
    return(result)
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
  if (!is.trackdata(trackdata)) 
    stop("Argument is not a trackdata object.")
  
  n <- 0:(nrow(trackdata$index)-1)
  indices <- rep(n, trackdata$index[,2]-trackdata$index[,1]+1)
  omat <- NULL

  for(j in 1:nrow(trackdata$ftime)){
    N <- trackdata$index[j,2]-trackdata$index[j,1]+1
    ftimes <- trackdata$ftime[j,]
    omat <- c(omat, seq(ftimes[1], ftimes[2], length=N))
  }

  result <- cbind(indices, omat, trackdata$data)
  append=FALSE
  if (is.spectral(trackdata) {
    append=TRUE
    samfreq <- attr(trackdata$data,"fs")[ncol(trackdata)]
    samfreq <- paste("#","Original_Freq", samfreq)
    trackname <- paste("#", "Trackname", trackdata$trackname)
    cat(samfreq,trackname,file=file,sep="\n")
  }
  write(t(result), file, ncolumns=ncol(result),append=append)
}



"emu.query" <- function(template, pattern=NULL, query="" )
{
  ## create a seglist object in the Tcl interpreter, which performs the query
  tclRes = .TclEval(sprintf("set segs  [emuR::seglist %%AUTO%% -database %s -pattern %s -query %s]",
    template,
    emu.inquotes( pattern ), .emu.inbrackets( query )))
  ##delete the Tcl seglist on exit
  on.exit(.TclEval(sprintf("$%s destroy", as.character(tclRes))))

  type = .TclEval(sprintf("$%s cget -type", as.character(tclRes)))
  type= .TclString(type)
  segs = .TclEval(sprintf("$%s gettext", as.character(tclRes)))
  num = .TclEval(sprintf("$%s numsegs", as.character(tclRes)))
  labs = c()
  stime = c()
  etime = c()
  utts = c()
  
  if(.TclNum(num) == 0) {
    stop(paste("Can't find the query results in emu.query: there may have been a problem with the query command.",sep=""))
  }
  cat("moving data from Tcl to R",sep="\n")
  ## get the data into R
  lsegs <- strsplit(tclvalue(segs),"\n")
  lsegs <- strsplit(lsegs[[1]],"\t")
  mat <- NULL
  ## in libemu, segments consist of 5 item
  mat <- matrix(unlist(lsegs),ncol=5,byrow=T)
  labs <- mat[,1]
  stime <- mat[,2]
  etime <- mat[,3]
  utts <- mat[,4]
  which = labs=="{}"
  labs[which]="*"
  if (length(labs) == 0) {
    stop("Error parsing seglist")
  }

                                        #create the R seglist
  result= make.seglist(labs, stime, etime, utts, 
    query, type, template )

  cat("Read", .TclNum(num), "records\n", file="", sep=" ")
  result
}


"emu.requery" <- function(segs, level, targetlevel=level,
                          justlabels=FALSE, sequence=0, longerok=FALSE )
{
  options <- ""
  if (justlabels)
    options <- paste( options, "-justlabels" )
  if (sequence != 0)
    options <- paste( options, "-sequence", sequence )
  if (longerok)
    options <- paste( options, "-longerok" )
  options <-  paste( options, "-targetlevel", targetlevel)
  s <- as.matrix(segs)
  nseg=nrow(segs)
  ## create a seglist in tcl
  TclSegs <- .TclEval(sprintf("set segs [emuR::seglist %%AUTO%% -type %s -database %s -query %s]",
                             emusegs.type(segs), emusegs.database(segs),
                             .emu.inbrackets(emusegs.query(segs))))

  ## fill the segment list with data
  for(i in c(1:nseg)) {
    lab <- .TclEval(sprintf("set Tlab \"%s\"", s[i,1]))
    res <- .Tcl(sprintf("$%s append [list %s %s %s %s]",
                        as.character(TclSegs),
                        as.character(lab),s[i,2],s[i,3],
                        s[i,4]))
  }
  ##requery
  tclRes <- .TclEval(sprintf("set nsegs [$%s requery %s %s]",
                            as.character(TclSegs), level, options))
  on.exit(.TclEval(sprintf("$%s destroy",as.character(TclSegs))),add=TRUE)
  if (justlabels) {
    ##then TclRes is a simple Tcl list containing just labels
    ##or maybe not so simple, when items contain spaces
    ##    result=splitstring(tclvalue(tclRes), " ")
    result=c()
    length <- .TclNum(.TclEval(sprintf("llength $%s", as.character(tclRes))))
    for(i in 1:length) {
      result=c(result,tclvalue(.TclLindex(tclRes,i-1)))
    }
    return(result)
  }
  else {
    # if not justlabels, then tclRes is a snit object which will be destroyed
    # on exit
    on.exit(.TclEval(sprintf("$%s destroy",as.character(tclRes))),add=TRUE)
    ##then TclRes is a snit type seglist
    type = .TclEval(sprintf("$%s cget -type", as.character(tclRes)))
    type= .TclString(type)
    nsegs = .TclEval(sprintf("$%s gettext", as.character(tclRes)))
    num = .TclEval(sprintf("$%s numsegs", as.character(tclRes)))
    template = .TclEval(sprintf("$%s cget -database", as.character(tclRes)))
    template=.TclString(template)
    labs = c()
    stime = c()
    etime = c()
    utts = c()
    
    if(.TclNum(num) == 0) {
      stop(paste("Can't find the query results in emu.query: there may have been a problem with the query command.",sep=""))
    }
    cat("moving data from Tcl to R",sep="\n")
    ## get the data into R
    lsegs <- strsplit(tclvalue(nsegs),"\n")
    lsegs <- strsplit(lsegs[[1]],"\t")
    mat <- NULL

    ## segments consists of 4 items (5 in the future)
    mat <- matrix(unlist(lsegs),ncol=4,byrow=T)
    labs <- mat[,1]
    stime <- mat[,2]
    etime <- mat[,3]
    utts <- mat[,4]
    which = labs=="{}"
    labs[which]="*"
    if (length(labs) == 0) {
      stop("Error parsing seglist")
    }
    result= make.seglist(labs, stime, etime, utts, 
      "requery", type, template )
    
    cat("Read", .TclNum(num), "records\n", file="", sep=" ")
    return(result)
  }
}

".emu.inbrackets" <- function (string ) {
  return( paste( "{", string , "}", sep="" ) )
}

".TclEval" <- function(cmd) {

  v <- tclVar()
  tclvalue(v) <- .Tcl(cmd)
  return(v)
}


".TclLindex" <- function(val, i) {

  v <- .TclEval(paste("lindex $", as.character(val), " ", i, sep=""))
  return( v )
}

".TclNum" <- function(val) {
  return(as.numeric(tclvalue(val)))
}

".TclString" <- function(val) {
  return(as.character(tclvalue(val)))
}


## Local Variables:
## mode:S
## S-temp-buffer-p:t
## End:
