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



"read.emusegs"<- function(file)
{
  ## scan the lines of the file into a vector

  ## R 1.4 introduced comment.char="#" arg to scan, grrr
  if( is.R() && as.numeric(version$minor) > 3.0 ) {
    ## in R, we need to avoid skipping the # as a comment line
    lines <- scan(file, what = "", sep="\n", comment.char="")
  } else {
    lines <- scan(file, what = "", sep="\n")
  }

  # first three lines are header followed by a hash line
  inheader <- 1
  i <- 1
  labels <- start <- end <- utts <- NULL
  while( i < length(lines) && inheader) {
    if( lines[i] == "#" ) {
      inheader <- 0
    } else {
      foo <- splitstring( lines[i], ":" )
      if( foo[1] == "database" ) database <- paste(foo[-1], sep=":")
      if( foo[1] == "query" ) {
        query <- paste(foo[-1], sep=":")
      }
      if( foo[1] == "type" ) type <- paste(foo[-1], sep=":")
      i <- i + 1
    }
  }

  if (inheader) {
    stop( "End of header (#) not found in segment file" )
  }

  ## now slurp the body of the segment list
  mat <- matscan( file, 4, what="", sk=i )
  
  segs <- make.seglist(mat[,1], mat[,2], mat[,3], mat[,4], 
                     query, type, database )

  segs
}

if( version$major >= 5 ) {
  setOldClass(c("emusegs", "data.frame"))
}

make.seglist <- function(labels, start, end, utts, query, type, database)
{
  seglist <- data.frame(labels=I(as.character(labels)),
			start=as.numeric(start), 
			end=as.numeric(end), 
			utts=I(as.character(utts)))

  if( version$major >= 5 ) {
    oldClass(seglist) <- "emusegs"
  } else {
    class(seglist) <- c("emusegs", "data.frame")
  }

  attr(seglist, "query") <- query
  attr(seglist, "type") <- type
  attr(seglist, "database") <- database

  seglist
}

is.seglist <- function(object) {
  return( inherits(object, "emusegs") )
}

## modify a segment list by changing one or more of the fields
"modify.seglist" <- function( segs,
                             labels=label(segs),
                             start=mustart(segs),
                             end=muend(segs),
                             utts=utt(segs),
                             query=emusegs.query(segs),
                             type=emusegs.type(segs),
                             database=emusegs.database(segs))
{
  make.seglist( labels, start, end, utts,
               query, type, database )
}

"emusegs.database" <- function(sl) 
{ 
  if(is.seglist(sl))
    attr(sl, "database")
  else 
    stop( "not an emu segment list" )
}

"emusegs.type" <- function(sl) 
{ 
  if(is.seglist(sl))
    attr(sl, "type")
  else 
    stop( "not an emu segment list" )
}

"emusegs.query" <- function(sl) 
{ 
  if(is.seglist(sl))
    attr(sl, "query")
  else 
    stop( "not an emu segment list" )
}

"print.emusegs" <-  function(x, ...) 
{
  cat(attributes(x)$type, " list from database: ", attributes(x)$database, "\n")
  cat("query was: ", attributes(x)$query, "\n" )
  if( version$major >= 5 ) {
    oldClass(x) <- "data.frame"
  } else {
    class(x) <- "data.frame"
  }
  print.data.frame(x, ...)
}

"[.emusegs"<- function(segs,i,j)
{
  NextMethod("[",drop=F)
}

if( version$major >= 5 ) {
setMethod("[", "emusegs",
          function(x, i, j=1:ncol(x), drop = T)
          {
            if(missing(drop))
              "[.emusegs"(x, i,j)
            else
              "[.emusegs"(x, i,j)
          }
          )
}

"summary.emusegs" <- function(object, ...)
{
  cat(attributes(object)$type, " list from database: ", attributes(object)$database, "\n")
  cat("query was: ", attributes(object)$query, "\n" )
  cat(" with", length(object$start), "segments\n\n")
  cat("Segment distribution:\n")
  print(table(object$label))
  invisible()
}


"label" <- function(segs)
{
  segs$label
}

"as.matrix.emusegs" <- function(x)
{
  cbind( as.character(x$label), x$start, x$end, as.character(x$utt) )
}

"write.emusegs" <- function(seglist, file)
{
  cat(paste("database:", attributes(seglist)$database, "\n", sep=""), file=file)
  cat(paste("query:", attributes(seglist)$query, "\n", sep=""), file=file, append=T)
  cat(paste("type:", attributes(seglist)$type, "\n", sep=""), file=file, append=T)
  cat("#\n", file=file, append=T)
  write(t(as.matrix(seglist)), file, ncolumns = 4, append=T)
}



# Local Variables:
# mode:S
# S-temp-buffer-p:t
# End:


