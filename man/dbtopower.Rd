\name{dbtopower}
\alias{dbtopower}

\title{ Function for inter-converting between decibels and a linear scale }
\description{
   The function converts from decibels to a linear scale 
}
\usage{
dbtopower(specdata, const = 10, base = 10, inv = FALSE)
}

\arguments{
  \item{specdata}{ A numeric object or an object of class trackdata }
  \item{const}{ A single element numeric vector. Defaults to 10 }
  \item{base}{ A single element numeric vector. Defaults to 10 }
  \item{inv}{ Logical. If T, then the conversion is
from a logarithmic to an anti-logarithmic form, otherwise the other way round }
}
\details{
   The function returns base^(specdata/const) if 
inv=F, otherwise,  const * log(dat, base=base). If the object
to which this function is applied is of class 'trackdata'
then this function is applied to $data. 
}
\value{
  An object of the same class.

}

\author{ Jonathan Harrington }

\seealso{ 
\code{\link{dbtopower}}
\code{\link{plot.spectral}}
}

\examples{

# convert 10 dB to a power ratio
vec = dbtopower(10)
# convert dB-data to a power ratio and back to decibels
res = dbtopower(vowlax.dft.5)
res = dbtopower(res, inv=TRUE)
}

\keyword{math}
