\" -*-nroff-*-
\name{discrim}
\alias{discrim}
\title{
Canonical Discriminant Analysis
}
\usage{
discrim(data, labs, dims=length(unique(labs)) - 1, transform=NULL, verbose=F)
}
\arguments{
\item{data}{
A multidimensional data matrix. 
}
\item{labs}{
A vector of labels, parallel to \code{data}.
}
\item{dims}{
The number of transformed dimensions to be returned. The maximum is one
less than the number of dimensions in \code{data}.
}
\item{transform}{
The result of a previous call to discrim. This can be used to transform
a second data set according to the eigenvector/eigenvalues determined
from a training data set.  
}
\item{verbose}{
If TRUE, a commentary is printed. 
}}
\value{
If \code{transform} is supplied the data is transformed according to the
eigenvector/eigenvalues in that tranform and a matrix of data is
returned.  If no transform is supplied, the result has the following
components: 

\item{data}{
The transformed data, a matrix with the same number of rows as \code{data}
and \code{dims} columns. 
}
\item{variates}{
The eigenvectors of the transformation. 
}
\item{values}{
The eigenvalues of the transformation
}}
\details{
(Taken from the unix man page for cda): 


Canonical discriminant analysis takes a set of grouped points in a
high dimesional space and determines the components such that points
within a group form tight clusters.  These points are called the
canonical variates and are labeled 0-(n-1) where n is the
dimensionality of the space (i.e.  the number of hidden units). The
original points can be projected on to these vectors. The result is a
low dimensional plot which has clustered the points belonging to each
group.


 
The method for finding the  canonical  variates  is  divided into 3
steps: 


 
(1) finding the within-sum-of-squares and  the  between-sum-of-squares
matrices 


 
(2) finding the eigenvectors (x)  [the  canonical  variates] and
eigenvalues (@) [the canonical values] which satisfy the equation:
      Bx =  @Wx 
where B is the between-sum-of-squares matrix and W is the
within-sum-of-squares matrix. The canonical variate with the greatest
canonical value corresponds to the direction in which the ratio of the
between-group distance to the within-group distance is maximised.


 
(3) projecting each of the initial  input  points  onto  the canonical
variates. 


}
\references{
The unix man page for \code{cda}. 
}
\seealso{
\code{\link{prcomp}}
}
\examples{
}
\keyword{mu}
% Converted by Sd2Rd version 0.3-3.
