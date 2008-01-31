\name{+,-,*,/.trackdata}
\alias{-.trackdata}
\alias{+.trackdata}
\alias{/.trackdata}
\alias{*.trackdata}
\alias{-}
\alias{+}
\alias{/}
\alias{*}

\title{ Arithmetic operations on track data objects}
\description{
  These are methods of the generic arithmetic functions that perform arithmetic operations on track data objects in the same way as arithmetic operations on other objects.
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

\author{Jonathan Harrington}

\seealso{
   \code{\link{Arithmetic}}
   \code{\link{trackdata}}
}

\examples{
#Subtraction of the first formant of the first segment from second formant
#vowlax.fdat[1,2] - vowlax.fdat[1,1]
}

\keyword{methods}
