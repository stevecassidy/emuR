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




"fapply" <-
function(specdata, fun, ..., power=FALSE, powcoeffs = c(10, 10))
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
omat <- dbtopower(omat, powcoeffs[1], powcoeffs[2], inv=TRUE)
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
omat <- dbtopower(omat, powcoeffs[1], powcoeffs[2], inv=TRUE)
return(omat)
}
}


# Local Variables:
# mode:S
# S-temp-buffer-p:t
# End:
