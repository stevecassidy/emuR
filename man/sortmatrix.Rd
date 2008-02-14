\name{sortmatrix}
\alias{sortmatrix}
\title{
Sort matrix by label
}
\usage{
sortmatrix(mat, labs=dimnames(mat)[[2]])
}
\description{
Sorts matrix by label
}
\arguments{
\item{mat}{
A mu+ sement matrix.
}
\item{labs}{
A label vector which has the same number of columns as \code{mat}.
}}
\value{
Returns a sorted matrix by label, created from \code{mat}.
}
\seealso{
label, phon
}
\keyword{misc}
% Converted by Sd2Rd version 0.3-3.
