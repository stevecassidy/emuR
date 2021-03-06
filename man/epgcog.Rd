\name{epgcog}
\alias{epgcog}
\title{ Electropalatographic centre of gravity }
\description{
  Calculate the centre of gravity in palatographic data.
}
\usage{epgcog (epgdata, weights = seq(7.5, 0.5, by = -1), rows = 1:8, 
    columns = 1:8, row1 = NULL) }

\arguments{
  \item{epgdata}{ An eight-columned EPG-compressed trackdata object,
or an eight columned matrix of EPG-compressed trackdata, or
a 3D palatographic array that is the output of palate() }
  \item{weights}{ A vector of 8  values  that are applied to
EPG rows 1-8 respectively. Defaults to 7.5, 7.0, 6.5...0.5. }
 \item{rows}{ Calculate EPG-COG over selected row number(s).
rows = 5:8, columns = 3:6 is an implementation
of posterior centre of gravity, as defined by Gibbon & Nicolaidis (1999,p. 239). See examples below. }
 \item{columns}{ Calculate EPG-COG over selected column number(s). }
 \item{row1}{ an optional single valued
numeric vector  to allow a separate weighting of
the electrodes in row1. For example, if row1=4/3, 
then all the electrodes in row1 are multiplied by 
that value, before EPG-COG is calculated.
Defaults to NULL (no weighting). }}

\details{
  The centre of gravity is a key function in palatographic
research and gives an value per palate that is
indicative of the overall location of contacts
along the anterior-posterior dimension. The formula
is an implementation of the ones discussed
in Hardcastle et al. (1991), Gibbon et al (1993),
and Gibbon & Nicolaidis (1999).
}

\value{
These functions return a trackdata object if
they are applied to an eight-columned EPG-compressed trackdata object,
otherwise a one-columned matrix.}

\references{
 GIBBON, F., HARDCASTLE, W. and NICOLAIDIS, K. (1993)
Temporal and spatial aspects of lingual
coarticuation in /kl/ sequences: a cross-linguistic
investigation.
Language & Speech, 36, 26t1-277.

GIBBON, F. AND NICOLAIDIS, K. (1999). Palatography. 
In W.J. Hardcastle & N. Hewlett (eds). Coarticulation. 
(pp. 229-245). Cambridge University Press: Cambridge.

HARDCASTLE, W, GIBBON, F. and NICOLAIDIS, K. (1991)
EPG data reduction methods and thier
implications for studies of lingual coarticulation.
Journal of Phonetics, 19, 251-266.
}

\examples{
#  COG: trackdata
cog <- epgcog(coutts.epg)
#  cog, one-columned matrix
cog <- epgcog(dcut(coutts.epg, 0.5, prop=TRUE))
# posterior cog for Fig. 10.5, p. 239 in Gibbon & Nicolaidis (1999)
r = array(0, c(8, 8, 2))
r[6,c(1, 8),1] <- 1
r[7,c(1, 2, 7, 8), 1] <- 1
r[8, ,1] <- 1
r[4, c(1, 2, 8), 2] <- 1
r[5, c(1, 2, 7, 8), 2] <- 1
r[6, c(1, 2, 3, 7, 8), 2] <- 1
r[7:8, , 2] = 1
class(r) <- "EPG"
epgcog(r, rows=5:8, columns=3:6)
}

\author{ Jonathan Harrington }


\seealso{ 
\code{\link{epgai}}
\code{\link{epgsum}}
\code{\link{palate}} 
}

%\keyword{emu}
\keyword{math}