\name{plafit}
\alias{plafit}

\title{ Calculate the coefficients of a parabola}
\description{
  This function calculates the coefficients of a parabola.
}
\usage{plafit(x)}

\details{
  This function calculates the coefficients of a parabola.
}

\examples{
# Calculates the values of the parabola for the coeefficients of the second formants of the dataset vowlax.
plafit(vowlax.fdat[1,2]$data)
    c0     c1     c2 
1768.0  -82.5   37.5

# Finds the three coefficients of a parabola that is fitted to 10 random numbers.
r = runif(10)
plafit(r)

}
\keyword{datasets}
