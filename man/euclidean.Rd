\name{euclidean}
\alias{euclidean}
\alias{euclidean.metric}

\title{
Find the inter-euclidean distance for a data matrix
}
\usage{
euclidean(data, m=1, n=ncol(data))
}
\description{
Finds the inter-euclidean distance for a data matrix
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
   }
}
\value{
Calculates the euclidean distance between successive rows of the matrix
based on  columns m:n. 
}
\seealso{
steady
}
\examples{
  euclidean(cbind(c(1,2,3,4), c(2,3,2,2)))
}
\keyword{misc}
% Converted by Sd2Rd version 0.3-3.
