\name{confusion}
\alias{confusion}
%- Also NEED an `\alias' for EACH other topic documented here.
\title{ Calculate a confusion matrix }
\description{
  Given two vectors of labels, one reference vector and one
  auto-generated,  calculate a confusion matrix summarising the errors
  made in the auto-generated vector. 
}
\usage{
confusion(labx, laby, perc=F)
}
\arguments{
 \item{labx}{ Reference set of labels }
 \item{laby}{ Auto-generated set of labels }
 \item{perc}{ Calculate percentage values (boolean) }
}
\details{

}
\value{
  A square matrix showing confusions between the two vectors.
}
\references{  }
\author{ Steve Cassidy }

\seealso{  \code{\link{classify}} }

\examples{
# generate two random sequences of integers
a <- abs(round(rnorm(100)*5))
b <- abs(round(rnorm(100)*5))
# simple confusion matrix
confusion(a,b)
# percentage style
confusion(a,b,perc=T)

}
\keyword{ emu }%-- one or more ...
