\" -*-nroff-*-
\name{train}
\alias{train}
\title{
Train a Gaussian Model
}
\usage{
train(x, lab)
}
\arguments{
\item{x}{
A data vector or matrix.
}
\item{lab}{
A vector of labels parallel to \code{x}. If missing, all data is
assumed to be from the same class.
}}
\value{
A structure with the following components: 

\item{label}{
The unique labels in \code{lab}.
}
\item{means}{
The means for each dimension per unique label. 
}
\item{cov}{
The combined covariance matrixes for each unique label. The
matrixes are joined with \code{rbind}.  If the input data is
one-dimensional, this is just the standard deviation of the data. 
}
\item{invcov}{
The combined inverse covariance matrixes for each unique label. The
matrixes are joined with \code{rbind}.  If the input data is
one-dimensional, this is just the reciprocal of the standard deviation
of the data.
}}
\details{
This function is used to train a gaussian model on a data set. The
result can be passed to either the \code{mahal} or \code{bayes.lab} functions to
classify either the training set (\code{x}) or a test set with the same
number of dimensions.  Train simply finds the mean and inverse
covariance matrix/standard deviation for the data corresponding to each
unique label in labs.
}
\seealso{
mahal, bayes.lab, mahalplot, bayes.plot
}
\keyword{mu}
% Converted by Sd2Rd version 0.3-3.
