\name{mahal.dist}
\alias{mahal.dist}
\title{
Calculate mahalanobis distances
}
\usage{
mahal.dist(data, train, labels = NULL)
}
\description{
Calculates mahalanobis distances

}
\arguments{
\item{data}{
A matrix of numerical data points.
}
\item{labels}{
A vector of labels..
}

\item{train}{
A gaussian model as returned by the \code{train} function.
}}
\value{
A matrix of distances with one column for every class (label) in the
gaussian model.  
}
\details{
The \code{train} function finds the centroids and covariance matrices for a
set of data and corresponding labels: one per unique label.  This
function can be used to find the mahalanobis distance of every data
point in a dataset to each of the class centroids.  The columns of the
resulting matrix are marked with the label of the centroid to which
they refer.  The function \code{mahal} should be used if you want to find
the closest centroid to each data point. 
}
\seealso{
train, mahal, bayes.lab, bayes.dist
}
\keyword{misc}
% Converted by Sd2Rd version 0.3-3.
