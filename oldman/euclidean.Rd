\" -*-nroff-*-
\name{euclidean}
\alias{euclidean}
\title{
Find the inter-euclidean distance for a data matrix
}
\usage{
euclidean(data, m=1, n=ncol(data))
}
\arguments{
\item{data}{
A vector or matrix of numerical data. 
}
\item{m}{
The first column of data to be used in the distance calculation.
}
\item{n}{
The last column of data to be used in the distance calculation.
}}
\value{
Calculates the euclidean distance between successive rows of the matrix
based on  columns m:n. 
}
\seealso{
steady
}
\examples{
> euclidean(cbind(c(1,2,3,4), c(2,3,2,2)))
[1] 1.414214 1.414214 1.000000
}
\keyword{mu}
% Converted by Sd2Rd version 0.3-3.
