\" -*-nroff-*-
\name{hamming}
\alias{hamming}
\title{
Generate a Hamming window
}
\usage{
hamming(pts=512)
}
\arguments{
\item{pts}{
The width of the window.
}}
\value{
A vector which represents a Hamming window of width \code{pts}, with
elements between 0 and 1. A signal vector should be multiplied by this
vector to apply the hamming window. 
}
\examples{
}
\keyword{misc}
% Converted by Sd2Rd version 0.3-3.
