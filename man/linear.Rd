\" -*-nroff-*-
\name{linear}
\alias{linear}
\title{
Perform linear time normalisation on trackdata.
}
\usage{
linear(dataset, n=20)
}
\arguments{
\item{dataset}{
A trackdata object as returned by \code{track}.
}
\item{n}{
The number of points (samples) required for each segment.
}}
\value{
A new trackdata object where the data for each segment has the same
number (\code{n}) of samples.  
}
\details{
The data for each segment is normaised using the \code{approx} function. 
}
\seealso{
approx
}
\keyword{mu}
% Converted by Sd2Rd version 0.3-3.
