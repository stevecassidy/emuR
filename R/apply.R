# ##' apply a function to each part of a trackdata object
# ##' 
# ##' Given an Emu trackdata object, \code{dapply} will apply a given function to
# ##' the data corresponding to each segment of data. The result is a new
# ##' trackdata object.
# ##' 
# ##' \code{dapply} can be used to apply an arbitrary function to trackdata
# ##' extracted from an Emu database. It can be used for example to smooth the
# ##' data (see \code{\link{dsmooth}}) or differentiate it (see
# ##' \code{\link{ddiff}}).
# ##' 
# ##' Trackdata is made up of three components: a matrix of data \code{\$data}, a
# ##' matrix of indexes (\code{\$index}) and a matrix of segment times
# ##' (\code{\$ftime}).  The indexes contain the start and end rows for each
# ##' segment in the trackdata, the time matrix contains the start and end times
# ##' of each data segment.
# ##' 
# ##' The function \code{fun} supplied to \code{dapply} should take one matrix of
# ##' data (corresponding to one segment) and a vector of two times being the
# ##' start and end of the data.  It should return a modified data matrix, which
# ##' can have any number of rows or columns, and a new pair of start and end
# ##' times.  The new start and end times are necessary because the operation
# ##' applied might shorten or interpolate the data and hence change the times
# ##' corresponding to the first and last rows of data.
# ##' 
# ##' @param trackdata An Emu trackdata object
# ##' @param fun A function taking a matrix of data and a vector of times and
# ##' returning a list with components \code{\$data} and \code{\$ftime}.
# ##' @param \dots Additional arguments to be passed to \code{fun}
# ##' @return An Emu trackdata object with components: \item{data}{A matrix of
# ##' data with all segments concatenated by row.} \item{index}{A two column
# ##' matrix of the start and end rows for each segment} \item{ftime}{A two
# ##' column matrix of the start and end times for each segment}
# ##' @seealso \code{\link{dsmooth}} \code{\link{ddiff}}
# ##' @keywords misc
# ##' @examples
# ##' 
# ##' 
# ##' data(dip)
# ##' ## formant data of the first segment in segment list dip
# ##' fm <- dip.fdat[1]
# ##' 
# ##' testfun <- function(data, ftime, n) {
# ##'   ## return only the first n rows of data
# ##'   ## doesn't check to see if there really are n rows...
# ##'   newdata <- data[1:n,]
# ##'   ## calculate a new end time
# ##'   interval <- (ftime[2]-ftime[1])/nrow(data)
# ##'   ftime[2] <- ftime[1] + interval*n
# ##'   ## now return the required list 
# ##'   return( list( data=newdata, ftime=ftime ) )
# ##' }
# ##' 
# ##' fm.first3 <- dapply( fm, testfun, 3 )
# ##' fm.first10 <- dapply( fm, testfun, 10 )
# ##' 
# ##' 
# ##' @export dapply
# 'dapply' <- function (trackdata, fun, ...) 
# {
#   if (version$major >= 5 && oldClass(trackdata) != "trackdata") {
#     stop("argument to dapply is not of class trackdata.")
#   }
#   else if (!is.trackdata(trackdata)) 
#     stop("argument to dapply is not of class trackdata.")
#   if (!is.matrix(trackdata$index)) {
#     trackdata$ftime <- rbind(trackdata$ftime)
#     trackdata$index <- rbind(trackdata$index)
#   }
#   thisrow <- 1
#   newindex <- trackdata$index
#   newdata <- NULL
#   newftime <- trackdata$ftime
#   for (j in 1:nrow(trackdata$index)) {
#     newindex[j, 1] <- thisrow
#     tmp <- fun(trackdata[j]$data, trackdata[j]$ftime, ...)
#     if (is.matrix(tmp$data)) {
#       newdata <- rbind(newdata, tmp$data)
#     }
#     else {
#       newdata <- c(newdata, tmp$data)
#     }
#     newftime[j, ] <- tmp$ftime
#     if (is.matrix(tmp$data)) 
#       thisrow <- thisrow + nrow(tmp$data)
#     else thisrow <- thisrow + length(tmp$data)
#     newindex[j, 2] <- thisrow - 1
#   }
#   x <- list(data = as.matrix(newdata), index = newindex, ftime = newftime)
#   if (version$major >= 5) {
#     oldClass(x) <- "trackdata"
#   }
#   else {
#     class(x) <- "trackdata"
#   }
#   return(x)
# }










##' Function that applies a function to an EMU spectral object
##' 
##' Applies a function to an EMU spectral object.
##' 
##' fapply performs a similar operation to apply except that it is specifically
##' designed for handling EMU spectral objects.
##' 
##' @param specdata A matrix or trackdata object of class spectral
##' @param fun A function to be applied.
##' @param \dots Optional arguments to fun
##' @param power A single element logical vector. If T, convert specdata to
##' power values i.e. apply the function to a * specdata
##' \eqn{\mbox{\textasciicircum}}{^}b or a * specdata\$data
##' \eqn{\mbox{\textasciicircum}}{^}b where a and b powcoeffs defined below.
##' @param powcoeffs A 2 element numeric vector for converting dB values to
##' power values. Defaults to a = 10 and b = 10. See \code{power}.
##' @return If the output has the same dimensions has the input, then an object
##' of the same dimensionality and class is returned. Otherwise it may be a
##' vector or matrix depending on the function that is applied.  ...
##' @section Warning : The function can be very slow if applied to a large
##' trackdata object. In this case, it may be faster to use a for-loop with the
##' desired function around \$data
##' @author Jonathan Harrington
##' @seealso \code{\link{apply}} \code{\link{by.trackdata}}
##' @keywords utilities
##' @examples
##' 
##' # mean value per spectrum, input is a spectral matrix
##' m <- fapply(vowlax.dft.5, sapply, FUN=mean)
##' # as above but after converting dB to powers before
##' # applying the function
##' m <- fapply(vowlax.dft.5, sapply, FUN=mean, power=TRUE)
##' # spectral range
##' r <- fapply(vowlax.dft.5, range)
##' # spectral moments applied to a trackdata object
##' # m is a four-dimensional trackdata object
##' m <- fapply(fric.dft, moments)
##' # 1st 3 DCT coefficients calculated in a spectral matrix
##' # d is a 3-columned matrix
##' d <- fapply(vowlax.dft.5, dct, 3)
##' # dct-smooth with 10 coefficients. d2 is spectral matrix
##' d2 <- fapply(vowlax.dft.5, dct, 10, TRUE)
##' # dct-smooth a trackdata object with 10 coefficients
##' d3 <- fapply(fric.dft[1:4,], dct, 10, TRUE)
##' 
##' 
##' @export fapply
'fapply' <- function (specdata, fun, ..., power = FALSE, 
                      powcoeffs = c(10, 10)) 
{
  if (!is.spectral(specdata)) 
    stop("object must be of class spectral")
  if (power) 
    specdata <- dbtopower(specdata, powcoeffs[1], powcoeffs[2])
  dnames <- dimnames(specdata)
  omat <- NULL
  if (is.trackdata(specdata)) {
    class(specdata$data) <- "spectral"
    for (j in 1:nrow(specdata$data)) {
      vals <- fun(specdata$data[j, ], ...)
      omat <- rbind(omat, vals)
    }
    if (ncol(omat) == ncol(specdata)) {
      dimnames(omat) <- dnames
      if (!is.spectral(omat)) 
        omat <- as.spectral(omat, attr(specdata$data, 
                                       "fs"))
    }
    else dimnames(omat) <- list(dnames[[1]], NULL)
    if (power) 
      omat <- dbtopower(omat, powcoeffs[1], powcoeffs[2], 
                        inv = TRUE)
    return(as.trackdata(omat, specdata$index, specdata$ftime))
  }
  else {
    if(!is.matrix(specdata))
    {
      samfreq = max(trackfreq(specdata))*2
      specdata = as.spectral(rbind(specdata), samfreq)
    }
    
    for (j in 1:nrow(specdata)) {
      vals <- fun(specdata[j, ], ...)
      omat <- rbind(omat, vals)
    }
    if (ncol(omat) == ncol(specdata)) {
      dimnames(omat) <- dnames
      if (!is.spectral(omat)) {
        class(omat) <- c(class(omat), "spectral")
        attr(omat, "fs") <- attr(specdata, "fs")
      }
    }
    else dimnames(omat) <- list(dnames[[1]], NULL)
    if (power) 
      omat <- dbtopower(omat, powcoeffs[1], powcoeffs[2], 
                        inv = TRUE)
    return(omat)
  }
}
