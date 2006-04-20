\name{plafit}
\alias{plafit}

\title{ Calculate the coefficients of a parabola}
\description{
  This function calculates the coefficients of a parabola.
}
\usage{plafit(x)}

\arguments{
  \item{x}{ a vector of numbers }
}
\details{
  This function calculates the coefficients of a parabola.
}

\author{Jonathan Harrington}

\examples{
# Calculate the values of the parabola for the coeefficients of the second formants of the dataset vowlax.
plafit(vowlax.fdat[1,2]$data)

# Finds the three coefficients of a parabola that is fitted to 10 random numbers.
r = runif(10)
r
plafit(r)

}
\keyword{emu}
