\" -*-nroff-*-
\name{dsmooth}
\alias{dsmooth}
\title{
Smooth the data in a trackdata object.
}
\usage{
dsmooth(dataset)
}
\arguments{
\item{dataset}{
A trackdata object as returned from \code{track}.
}}
\value{
The result of applying the \code{smooth} function to each column of the data
for each segment in the trackdata object. 
}
\details{
This function uses the \code{dapply} function to apply \code{smooth} to the data
for each segment. 
}
\seealso{
smooth, dapply
}
\keyword{~keyword}
% Converted by Sd2Rd version 0.3-3.
