\name{epggs}
\alias{epggs}
\title{ Plot a grey-scale image of palatographic data. }
\description{
  The function plots a grey-scale image of palatographic data
such that the greyness in cell r, c is in proportion to the
frequency of contacts in cells of row r and columns c
of all palatograms in the object passed to this function.
}
\usage{epggs (epgdata, gscale = 100, gridlines = T, gridcol = "gray", 
    gridlty = 1, axes = T, xlab = "", ylab = "", ...)  }

\arguments{
  \item{epgdata}{ An eight-columned EPG-compressed trackdata object,
or an eight columned matrix of EPG-compressed trackdata, or
a 3D palatographic array that is the output of palate() }
  \item{gscale}{a single valued numeric vector that defines the granularity
of the greyscale. Defaults to 100.  }
 \item{gridlines}{ if T (default) grid lines over the
palatographic image are drawn are drawn. }
 \item{gridlty}{ A single-valued numeric vector that defines the linetype for plotting the grid. }
 \item{...}{ graphical parameters can be given as arguments to 'epggs'. }}

\details{
  The function plots a grey-scale image of up to 
62 values  arranged over an 8 x 8
grid with columns 1 and 8 unfilled for row 1. 
If cell row r column c is contacted for all 
palatograms in the object that is passed to this function,
the corresponding cell is black; if none of
of the cells in row r column c are contacted,
then the cell is white (unfilled).
}


\examples{
# greyscale image across the first two segments 'just relax'
# with title
epggs(coutts.epg[1:2,], main="just relax")
readline("press enter to continue")

# as above but with dotted gridlines in blue
epggs(coutts.epg[1:2,], main="just relax", gridlty=2, gridcol="blue")
readline("press enter to continue")

# as the first example, but with greyscale set to 2
epggs(coutts.epg[1:2,], 2, main="just relax")
readline("press enter to continue")

# get palatograms for "S" from the polhom.epg database
temp = polhom.l == "S"
# greyscale image of all "S" segments at their temporal midpoint
epggs(dcut(polhom.epg[temp,], 0.5, prop=T))
readline("press enter to continue")

# greyscale image of all "S" segments from their onset to offset
epggs(polhom.epg[temp,])
readline("press enter to continue")

# the same but derived from palates
p <- palate(polhom.epg[temp,])
epggs(p)
}

\author{ Jonathan Harrington }


\seealso{ 
\code{\link{epgai}}
\code{\link{epgcog}}
\code{\link{epgplot}}
\code{\link{palate}} 
}

\keyword{emu}