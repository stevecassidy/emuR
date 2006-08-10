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


"dapply" <-
function (trackdata, fun, ...) 
{
    if (version$major >= 5 && oldClass(trackdata) != "trackdata") {
        stop("argument to dapply is not of class trackdata.")
    }
    else if (!is.trackdata(trackdata)) 
        stop("argument to dapply is not of class trackdata.")
    if (!is.matrix(trackdata$index)) {
        trackdata$ftime <- rbind(trackdata$ftime)
        trackdata$index <- rbind(trackdata$index)
    }
    thisrow <- 1
    newindex <- trackdata$index
    newdata <- NULL
    newftime <- trackdata$ftime
    for (j in 1:nrow(trackdata$index)) {
        newindex[j, 1] <- thisrow
        tmp <- fun(trackdata[j]$data, trackdata[j]$ftime, ...)
        if (is.matrix(tmp$data)) {
            newdata <- rbind(newdata, tmp$data)
        }
        else {
            newdata <- c(newdata, tmp$data)
        }
        newftime[j, ] <- tmp$ftime
        if (is.matrix(tmp$data)) 
            thisrow <- thisrow + nrow(tmp$data)
        else thisrow <- thisrow + length(tmp$data)
        newindex[j, 2] <- thisrow - 1
    }
    x <- list(data = as.matrix(newdata), index = newindex, ftime = newftime)
    if (version$major >= 5) {
        oldClass(x) <- "trackdata"
    }
    else {
        class(x) <- "trackdata"
    }
    return(x)
}



"dmap"<- function(trackdata, fun, arg)
{
  ## a version of dapply which takes an additional
  ## argument which should be a vector/matrix
  ## with the same length/number of rows as the number of 
  ## segments in the trackdata.  One row of 'arg' is
  ## passed to 'fun' along with the data

  if( version$major >= 5  && oldClass(trackdata)!="trackdata") {
      stop("argument to dmap is not of class trackdata.")
  } else if(class(trackdata)!="trackdata")
    stop("argument to dapply is not of class trackdata.")

  if(!is.matrix(trackdata$index)){
    trackdata$ftime <- rbind(trackdata$ftime)
    trackdata$index <- rbind(trackdata$index)
  }

  thisrow <- 1
  newindex <- trackdata$index
  newdata <- NULL
  newftime <- trackdata$ftime

  for(j in 1:nrow(trackdata$index)) {
    newindex[j,1] <- thisrow

    if (is.matrix(arg)) {
      tmp <- fun(trackdata[j]$data, trackdata[j]$ftime, arg[j,])
    } else {
      tmp <- fun(trackdata[j]$data, trackdata[j]$ftime, arg[j])
    }

    if(is.matrix(tmp$data)){
      newdata <- rbind(newdata, tmp$data)
    } else {
      newdata <- c(newdata, tmp$data)
    }
    
    newftime[j,] <- tmp$ftime

    if(is.matrix(tmp$data))
      thisrow <- thisrow + nrow(tmp$data)
    else
      thisrow <- thisrow + length(tmp$data)
    newindex[j,2] <- thisrow - 1

  }

  x <- list(data=as.matrix(newdata), index=newindex, ftime=newftime)
  if( version$major >= 5 ) {
    oldClass(x) <- "trackdata"
  } else {
    class(x) <- "trackdata"
  }
  return(x)
}




"segdiff"<- function( data, ftime, n=1 )
{
  ## a function to be used with dapply()
  ## differentiates a segment n times returning
  ## the diff'd data and a new ftime pair

  if(is.matrix(data))
    datalength <- nrow(data)
  else
    datalength <- length(data)

  newdata <- apply( data, 2, diff, n )
  samp <- (ftime[2]-ftime[1])/datalength
  newftime <- c(ftime[1]+(n*samp/2), ftime[2]-(n*samp/2))
  return( list(data=newdata, ftime=newftime) )
}

ddiff <- function( trackdata, n=1 ) {
  return( dapply( trackdata, segdiff, n) )
}


"dapply.lab"<- function(data, labs, fun)
{
  ## data can be a list as returned by track(), a vector
  ## or a matrix of data. Returns the output of fun for
  ## each unique label in labs
  if(is.list(data)) {
    data <- data$data
    labs <- norm.sub(data$index, labs)
  }
  if( !is.matrix(data) ) data <- cbind(data)
  mat <- NULL
  for(j in unique(labs)) {
    temp <- labs == j
    vals <- data[temp,  ]
    if( !is.matrix(vals) ) vals <- cbind(vals)
    vecf <- apply(vals, 2, fun)
    mat <- rbind(mat, vecf)
  }
  dimnames(mat) <- list(unique(labs), NULL)
  mat
}

"norm.sub"<- function(indvals, labs)
{
  mat <- NULL
  for(j in 1:nrow(indvals)) {
    rightin <- indvals[j, 2]
    leftin <- indvals[j, 1]
    num <- rightin - leftin + 1
    vec <- rep(labs[j], num)
    mat <- c(mat, vec)
  }
  mat
}


"fapply" <-
function(specdata, fun, ..., power=F, powcoeffs = c(10, 10))
{
if(!is.spectral(specdata))
stop("object must be of class spectral")
if(power)
specdata <- dbtopower(specdata, powcoeffs[1], powcoeffs[2])
dnames  <- dimnames(specdata)
omat <- NULL
if(is.trackdata(specdata)){
class(specdata$data) <- "spectral"
for(j in 1:nrow(specdata$data)){
vals <- fun(specdata$data[j,], ...)
omat <- rbind(omat, vals)
}
if(ncol(omat) == ncol(specdata))
{
dimnames(omat) <- dnames
if(!is.spectral(omat))
omat <- as.spectral(omat, attr(specdata$data, "fs"))
}
else
dimnames(omat) <- list(dnames[[1]], NULL)
if(power)
omat <- dbtopower(omat, powcoeffs[1], powcoeffs[2], inv=T)
return(as.trackdata(omat, specdata$index, specdata$ftime))
}




else {
for(j in 1:nrow(specdata)){
vals <- fun(specdata[j,], ...)
omat <- rbind(omat, vals)
}
if(ncol(omat) == ncol(specdata))
{
dimnames(omat) <- dnames
if(!is.spectral(omat))
{
class(omat) <- c(class(omat), "spectral")
attr(omat, "fs") <- attr(specdata, "fs")
}
}
else
dimnames(omat) <- list(dnames[[1]], NULL)
if(power)
omat <- dbtopower(omat, powcoeffs[1], powcoeffs[2], inv=T)
return(omat)
}
}


# Local Variables:
# mode:S
# S-temp-buffer-p:t
# End:
