\name{fapply}
\alias{fapply}

\title{ Function that applies a function to an EMU spectral object}
\description{
 Applies a function to an EMU spectral object.
}
\usage{
fapply(specdata, fun, ..., power = F, powcoeffs = c(10, 10))
}

\arguments{
  \item{specdata}{ A matrix or trackdata object of class spectral }
  \item{fun}{ A function to be applied. }
  \item{\dots}{ Optional arguments to fun }
  \item{power}{ A single element logical vector. If T, convert specdata to
power values i.e. apply the function to a * specdata^b or a * specdata$data^b
where a and b powcoeffs defined below. }
  \item{powcoeffs}{ A 2 element numeric vector for converting dB values to power values.
Defaults to a = 10 and b = 10. See \code{power}}
}
\details{
  fapply performs a similar operation to apply except that it
is specifically designed for handling EMU spectral objects.
}
\value{
  If the output has the same dimensions has the
input, then an object of the same dimensionality and
class is returned. Otherwise it may be a vector or matrix
depending on the function that is applied.
  ...
}
\author{ Jonathan Harrington }

\section{Warning }{The function can be very slow
if applied to a large trackdata object. In this case,
it may be faster to use a for-loop with the desired
function around $data}

\seealso{
\code{\link{apply}}
\code{\link{by.trackdata}}
}
\examples{
# mean value per spectrum, input is a spectral matrix
m <- fapply(vowlax.dft.5, mean)
# as above but after converting dB to powers before
# applying the function
m <- fapply(vowlax.dft.5, mean, power=T)
# spectral range
r <- fapply(vowlax.dft.5, range)
# spectral moments applied to a trackdata object
# m is a four-dimensional trackdata object
m <- fapply(fric.dft, moments)
# 1st 3 DCT coefficients calculated in a spectral matrix
# d is a 3-columned matrix
d <- fapply(vowlax.dft.5, dct, 3)
# dct-smooth with 10 coefficients. d2 is spectral matrix
d2 <- fapply(vowlax.dft.5, dct, 10, T)
# dct-smooth a trackdata object with 10 coefficients
d3 <- fapply(fric.dft[1:4,], dct, 10, T)

}
\keyword{emu}
