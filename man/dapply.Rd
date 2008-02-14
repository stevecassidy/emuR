\name{dapply}
\alias{dapply}
\title{ apply a function to each part of a trackdata object }
\description{
Given an Emu trackdata object, \code{dapply} will apply a given 
function to the data corresponding to each segment of data. The 
result is a new trackdata object. 
}
\usage{
dapply(trackdata, fun, ...)
}
\arguments{
 \item{trackdata}{ An Emu trackdata object }
 \item{fun}{ A function taking a matrix of data and a vector of times
   and returning a list with components \code{\$data} and \code{\$ftime}.}
 \item{\dots}{ Additional arguments to be passed to \code{fun} }
}
\details{
\code{dapply} can be used to apply an arbitrary function to trackdata
extracted from an Emu database. It can be used for example to smooth the
data (see \code{\link{dsmooth}}) or differentiate it (see
\code{\link{ddiff}}).

Trackdata is made up of three components: a matrix of data \code{\$data},
a matrix of indexes (\code{\$index}) and a matrix of segment times
(\code{\$ftime}).  The indexes contain the start and end rows for each
segment in the trackdata, the time matrix contains the start and end
times of each data segment.

The function \code{fun} supplied to \code{dapply} should take one matrix
of data (corresponding to one segment) and a vector of two times being
the start and end of the data.  It should return a modified data matrix,
which can have any number of rows or columns, and a new pair of start
and end times.  The new start and end times are necessary because the
operation applied might shorten or interpolate the data and hence change
the times corresponding to the first and last rows of data.
}
\value{
  An Emu trackdata object with components:
  \item{data}{A matrix of data with all segments concatenated by row.}
  \item{index}{A two column matrix of the start and end rows for each segment}
  \item{ftime}{A two column matrix of the start and end times for each segment}
}
\seealso{
\code{\link{dsmooth}}
\code{\link{ddiff}}
}

\examples{

data(dip)
## formant data of the first segment in segment list dip
fm <- dip.fdat[1]

testfun <- function(data, ftime, n) {
  ## return only the first n rows of data
  ## doesn't check to see if there really are n rows...
  newdata <- data[1:n,]
  ## calculate a new end time
  interval <- (ftime[2]-ftime[1])/nrow(data)
  ftime[2] <- ftime[1] + interval*n
  ## now return the required list 
  return( list( data=newdata, ftime=ftime ) )
}

fm.first3 <- dapply( fm, testfun, 3 )
fm.first10 <- dapply( fm, testfun, 10 )

}
\keyword{misc}%-- one or more ...
