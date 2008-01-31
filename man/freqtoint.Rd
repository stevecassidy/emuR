\name{freqtoint}
\alias{freqtoint}
\title{ Function to find the column number corresponding to frequencies of a spectral object }
\description{
  Find the column number corresponding to frequencies of
a spectral object.
}
\usage{
freqtoint(trackdata, j)
}
\arguments{
  \item{trackdata}{ A spectral object}
  \item{j}{ A vector of frequencies }
}
\details{
  This function is used in conjunction with object oriented
programming of EMU spectral objects. It should not in general
be called from inside a function. Its principal use
is to determine the column number(s) corresponding to
frequencies for spectral trackdata objects or spectral matrices
or the element number for spectral vectors.
}


\author{Jonathan Harrington}


%\seealso{ 
%\code{\link{[.spectral}}
%}

\examples{
freqtoint(fric.dft,1000:2000)
# all frequencies except 1000-2000
freqtoint(vowlax.dft.5, -(1000:2000))
# all frequencies except 1000 Hz
freqtoint(e.dft, -1000)
# the d.c. offset - i.e. column 1
freqtoint(vowlax.dft.5, 0)
# all freqs except the d.c. offset - i.e. not column 1
freqtoint(vowlax.dft.5, -1)



}

\keyword{math}


