\" -*-nroff-*-
\name{frames}
\alias{frames}
\title{
Extract data from the speech database
}
\usage{
function(segs, trackname, justindex = F, justftime = F, 	
         justdata = F, extn = options("extensions"))
}
\arguments{
\item{segs}{
A mu+ segment list. 
}
\item{trackname}{
A valid parameter track name for the current segment list (see \code{tracks}).
}
\item{justindex}{
Only return the indx component of the result
}
\item{justftime}{
Only return the ftime component of the result
}}
\value{
Unless one of \code{justdata}, \code{justindex} or \code{justftime} are specified, the
result is an object of class \code{trackdata} and has the following components: 

\item{data}{
A matrix of data corresponding to the specified parameter track for the
segment list.  The number of dimensions (columns) in the matrix depends
on the track specified, for example, track \code{"fm[1..3]"} (formants 1 to 3)
will have three columns. The data for all segments is concatenated in
rows, the data corresponding to a particular segment can be extracted
using the \code{index} component. 
}
\item{index}{
A matrix with two columns giving the start and end indexes of each segment
within the \code{data} matrix.
}
\item{ftime}{
A matrix with two columns giving the start and end times of each
segment copied from the segment list.  Both the \code{index} and the \code{ftime}
components will have the same number of rows as the original segment
list.
}}
\seealso{
track, tracks
}
\keyword{mu}
% Converted by Sd2Rd version 0.3-3.
