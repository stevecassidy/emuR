\name{is.spectral}
\alias{is.spectral}

\title{ Function to test whether the object is of class "spectral"}
\description{
  Returns T or F depending on whether the object is of class "spectral"
}
\usage{
is.spectral(dat)
}

\arguments{
  \item{dat}{An R object }
}

\value{
  A single element logical vector: T or F
}

\author{ Jonathan Harrington}

\seealso{ 
\code{\link{as.spectral}}
}

\examples{

is.spectral(vowlax.dft.5)
is.spectral(fric.dft)
is.spectral(fric.dft$data)
is.spectral(vowlax.dft.5[1,])
is.spectral(fric.dft[1,1])


}


\keyword{attribute}
