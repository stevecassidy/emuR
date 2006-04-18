\name{(+,-,*,/.trackdata}
\alias{-.trackdata}
\alias{+.trackdata}
\alias{/.trackdata}
\alias{*.trackdata}

\title{ Arithmetic operations on track data objects}
\description{
  Arithmetic operations on track data objects are performed in the same way as arithmetic operations on other objects.
}
\usage{
x + y
x - y
x / y
x * y
}

\arguments{
	\item{x}{track data object}
	\item{y}{single value or a vector, matrix or track data object of the same dimension as x}
}

\value{
  track data object
}

\seealso{
\code{\link{Arithmetic}}
}

\examples{
#Subtraction of the first formant of the first segment from second formant
vowlax.fdat[1,2] - vowlax.fdat[1,1]
}
\keyword{emu}

