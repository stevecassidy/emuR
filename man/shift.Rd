\name{shift}
\alias{shift}

\title{ Function to shift the elements of a vector. }
\description{
   The function makes use of the function 'fitler'
to delay or advance a signal by k points. 
}
\usage{
shift(x, delta = 1, circular = T)
}
.
\arguments{
  \item{x}{ A numeric vector }
  \item{delta}{ A single element numeric vector. Defines
the number of points by which the signal should be shifted. }
  \item{circular}{ Logical. If T, the signal is wrapped
around itself so that if delta = 1, x[n] becomes x[1]. Otherwise,
if delta is positive, 
the same number of zeros are prepended to the signal }
}
\details{
   The function makes use of the function 'filter' for
linear filtering to carry out the shifting.

}
\value{The signal shifted by a certain number of points.
  ...
}

\author{ Jonathan Harrington }

\seealso{ filter  }
\examples{
vec = 1:10
shift(vec, 2)
shift(vec, -2)
shift(vec, 2, circular=F)


}
\keyword{emu}
