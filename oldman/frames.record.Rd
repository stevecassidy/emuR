\" -*-nroff-*-
\name{frames.record}
\alias{frames.record}
\title{
Find a record in a frames data object.
}
\usage{
frames.record(trackdata, segnum, time)
}
\arguments{
\item{trackdata}{
A trackdata object resulting from a call to \code{track}.
}
\item{segnum}{
The segment number we are interested in.
}
\item{time}{
The time we are interested in. 
}}
\value{
The index into the \code{data} component of \code{trackdata} which contains the
data for this segment at this time.  
}
\seealso{
track, get.time.element, dextract, dextract.lab
}
\keyword{mu}
% Converted by Sd2Rd version 0.3-3.
