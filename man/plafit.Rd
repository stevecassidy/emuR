\name{plafit}
\alias{plafit}

\title{ Calculate the coefficients of a parabola}
\description{
  Fit a second ordered polynomial to a vector of values
}
\usage{plafit(wav, fit = FALSE, n = 101)}

\arguments{
  \item{wav}{ a vector or single column matrix of numeric values to which the 2nd order polynomial is to be fitted. }
 \item{fit}{ if F, return the coefficients of
the polynomial; if T, the values of the polynomial
are returned to the same length as the vector wav. }
 \item{n}{ in fitting the polynomial, linear time normalisation
is first applied to the input vector wav to 101 points.
The polynomial is fitted under the assumption that
these points extend linearly in time between t = -1 and t = 1
with t = 0 occurring at the temporal midpoint. }
}

\details{
The function fits a parabola (2nd order polynomial) following the method of van Bergem, Speech Communication, 14, 1994, 143-162.
The algorithm fixes the parabola at the onset, midpoint, and offset
of the vector i.e. such htat the fitted parabola and original vector have the same values at these points.
}

\value{
The function returns the coefficients of
c0, c1, c2 in the parabola  y = c0 + c1t + c2t^2
where t extends between -1 and 1. The function
can also be used to derive the values 
of the parabola as a function of time from the coefficients.
}

\author{Jonathan Harrington}

\examples{
# fit a polynomial to a segment of fundamental frequency data
plafit(vowlax.fund[1,]$data)

# return the fitted values of the polynomial
plafit(vowlax.fund[1,]$data, fit=T)

# obtain coefficients for the first three segments
# of f0 data in the trackdata object vowlax.fund
by(vowlax.fund[1:3,], plafit, simplify=T)
}




\seealso{ 
   \code{\link{dct}}
}


\keyword{math}

