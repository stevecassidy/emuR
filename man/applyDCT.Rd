\name{applyDCT}
\alias{applyDCT}
\title{ Apply a Discrete Cosine Transform to trackdata }
\description{
  Applies the Discrete Cosine Transform to the  data  for each segment in a
  trackdata object. DCT coefficients measure mean, slope and kertosis
  of a data track. 
}
\usage{
applyDCT(trackdata, numOcoef = 3, prop = 1)
}
\arguments{
  \item{trackdata}{ A trackdata object as returned by \code{emu.track} }
  \item{ncoef}{ The number of DCT coefficients to generate for each track }
  \item{prop}{ The proportion of each track to analyse, a number
    between zero and  one. }
}
\value{
  A matrix with one row per segment of data in the original trackdata
  object  and \code{ncoef} columns for each column in the original data.
}
\references{ K. Rao and P. Yip, Discrete cosine transform : algorithms,
  advantages, applications, Academic Press, Boston, MA, 1990.}
\author{Catherine Watson}

\seealso{ \code{\link{DCT}} }

\examples{
library(emu)
segs <- emu.query("demo",  "*", "Phonetic=vowel")
fm <- emu.track(segs, "fm")
dcs <- applyDCT(fm, 3, 1)
}
\keyword{ ~kwd1 }% at least one, from doc/KEYWORDS
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
