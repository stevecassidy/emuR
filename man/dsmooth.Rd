\name{dsmooth}
\alias{dsmooth}
\title{
Smooth the data in a trackdata object.
}
\description{Smooths each dataset in a trackdata object using a running
  mean smoother.}
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
\keyword{misc}
% Converted by Sd2Rd version 0.3-3.
