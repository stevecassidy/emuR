\" -*-nroff-*-
\name{get.time.element}
\alias{get.time.element}
\title{
Get data for a given time
}
\usage{
get.time.element(time, trackdata)
}
\arguments{
\item{time}{
A time in milliseconds
}
\item{trackdata}{
A trackdata object as returned by \code{track}.
}}
\value{
The element number of \code{trackdata$data} corresponding to \code{time} 
}
\seealso{
track, frames
}
\examples{
# Given a segment list: 
> segs
      start       end                           utt 
E "12489.4" "12586.9" "/home/dbase/dbase1/msadb001"
@ "12916.9" "12982.2" "/home/dbase/dbase1/msadb001"
E "13155.2" "13240.4" "/home/dbase/dbase1/msadb001"


# We extract some formant data: 
> data _ track(segs, "fm[1..3]")


# We can now find which element of data$data corresponds to the start of
# the first "E" segment: 
> get.time.element( 12489.4, data )
[1] 1


# and to the midpoint of the last "E" segment: 
> get.time.element(13197.8, data )
[1] 47
}
\keyword{mu}
% Converted by Sd2Rd version 0.3-3.
