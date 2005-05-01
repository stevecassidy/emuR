\name{dcut}
\alias{dcut}
\title{Extracts a subset of data from an Emu trackdata object}

\description{
  A general purpose tool for extracting data from track objects
either at a particular time, or between two times. The times
can be values in milliseconds or proportional times between
zero (the onset) and one (the offset). 
}

\usage{
dcut(trackdata, left.time, right.time, single=T, average=T, prop=F)
}

\arguments{
  \item{trackdata}{An Emu trackdata object.}
  \item{left.time}{A numeric vector: either values in 
milliseconds, or proportional times.}
  \item{right.time}{ An optional numeric vector: either values
in milliseconds or proportional times.}
  \item{single}{ If TRUE, one value is returned per segment.
This applies when the requested time falls between
two track frames. When single=TRUE, the preceding value
is returned, unless average=TRUE (see below), in which case
the average value of the two frames is returned.
when the right.time argument is omitted}
  \item{average}{ A single element logical vector - see single above. Applies
only when the right.times argument is omitted and
when single = TRUE }
  \item{prop}{If TRUE times interpreted as proportions, if FALSE, they
  are interpreted as millisecond times.}
}

\details{
This function cuts out part of the data for each segment  in a
trackdata object.  For example cutting out the middle 50\%  or removing
10ms from each segment's data. 
  
If \code{prop=FALSE} the time arguments (\code{left.time} and
\code{right.time}) are interpreted as millisecond times and each should
be a vector with the same length as the number of segments in
\code{trackdata}.  If \code{prop=TRUE} the time arguments should be
single values between zero (the onset of the segment) and one (the
offset).  

If \code{right.time} is ommitted then a single data point correponding
to \code{left.time} for each segment is returned. 

}

\value{
  A trackdata object with the shorter segment data when
  \code{right.time} is ommitted a matrix or vector of data with one row
  per segment.
}
\author{ Jonathan Harrington}
\seealso{emu.track}

\examples{

data(demo.vowels.fm)

# get the formants at the midpoint returns a matrix
f50 <- dcut(demo.vowels.fm, 0.5, prop=T)
# get the formants between the 25\% and 75\% time points
# returning a trackdata object
fcut <- dcut(demo.vowels.fm, 0.25, 0.75, prop=T)
# get  F0 at the midpoint, returns a vector

data(demo.all)
data(demo.all.rms)

rms50 <- dcut(demo.all.rms, 0.5, prop=T)
# get the RMS 50ms on either side of the midpoint

# first generate a vector of times from the segment list
# take care not to extend beyond the original start/end
# times of the segment list
midtime <- (mustart(demo.all)+muend(demo.all))/2
left <- midtime - 5
right <- left + 3

# get these 3ms segments
rms3 <- dcut(demo.all.rms, left, right)

# get the RMS at 5 ms less than the midpoint for each segment
rmsvec <- dcut(demo.all.rms, left)

## make some very short tracks
fm2 <- dcut(demo.vowels.fm, 0.0, 0.1)


}


\keyword{misc}

