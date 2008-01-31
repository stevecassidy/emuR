\name{EPG contact indices}
\alias{epgai}
\alias{epgci}
\alias{epgdi}
\title{ Electropalatographic contact indices }
\description{
  epgai(), epgci(), epgdi() return the anteriority index,
the centrality index, the dorsopalatal index respectively
as a trackdata object or a vector
}
\usage{epgai (epgdata, weights = c(1, 9, 81, 729, 4921))
    epgci (epgdata, weights = c(1, 11, 120, 1320)) 
    epgdi (epgdata)  }

\arguments{
  \item{epgdata}{ An eight-columned EPG-compressed trackdata object,
or an eight columned matrix of EPG-compressed trackdata, or
a 3D palatographic array that is the output of palate() }
  \item{weights}{A vector of five values  that are applied to
EPG rows 1-5 respectively in epgai(). A vector of
four values that are applied to columns 1 and 8, 
to columns 2 and 7, columns 3 and 6, columns 4 and 5
respectively. Defaults to the values given in Recasens & Pallares (2001). }
}

\details{
  These are exact implementations of the formulae
for calculating the EPG anteriority, EPG centrality, 
and EPG dorsopalatal indices as described in Recasens & Pallares (2001).
}

\value{
These functions return a trackdata object if
they are applied to an eight-columned EPG-compressed trackdata object,
otherwise a one-columned matrix.}

\references{ GIBBON, F. AND NICOLAIDIS, K. (1999). Palatography. 
In W.J. Hardcastle & N. Hewlett (eds). Coarticulation. 
(pp. 229-245). Cambridge University Press: Cambridge.

RECASENS, D. & PALLARES, M. (2001) Coarticulation, assimilation and 
blending in Catalan consonant clusters. Journal of Phonetics, 29, 273-301. }

\examples{
#  Anteriority index: trackdata
ai <- epgai(coutts.epg)
#  Dorsopalatal index, one-columned matrix
di <- epgdi(dcut(coutts.epg, 0.5, prop=T))
# Next to examples: Centrality  index, one-columed matrix
ci <- epgci(palate(coutts.epg))
ci <- epgci(palate(dcut(coutts.epg, 0.5, prop=T)))

}

\author{ Jonathan Harrington }


\seealso{ 
\code{\link{epgcog}}
\code{\link{epggs}}
\code{\link{palate}} 
}

%\keyword{emu}
\keyword{math}