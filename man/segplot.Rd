\name{segplot}
\alias{segplot}
\title{
Segment plot
}
\description{
Generate a plot of a track for each segment in a segment list,
multiple plots are  arranged on a page  in a grid.
}
\usage{
segplot(segs, param, labels, ylim, ylab = "", main = "", zeros = F, 
	 pdat = 0.5, onset = 0, offset = 0, col = T, linetype = F
	 smoothing = F, mfrow, addlines, differ )
}
\arguments{
  \item{segs}{An emu segment list.}
  \item{param}{One of the valid parameter tracks for the database.}
  \item{ylim}{Limit the y-axis range for all plots.}
  \item{labels}{
    A vector of labels to be used as the main title of each plot. If this
    is not provided the segment labels will be used. 
  }
  \item{ylab}{
    A label for every y-axis.
  }
  \item{main}{
    A main title for the plot.
  }
  \item{zeros}{
    If TRUE and there is a \code{prob\_voice} track, the regions
    with probability of voicing less than \code{pdat} will not be plotted.  
  }
  \item{pdat}{
    The probability of voicing threshold for \code{zeros}. 
  }
  \item{onset}{
    The plot for each segment will start this much to the left of the
    start of the segment. 
  }
  \item{offset}{
    The plot for each segment will end this much to the right of the end
    of the segment.  
  }
  \item{colour}{
    If \code{TRUE}, different colours will be used in the plot.
  }
  \item{linetype}{
    If \code{TRUE} different linetypes will be used in the plot. Useful for
    printing. 
  }
  \item{smoothing}{
    If smoothing is TRUE, a non-linear smoother (\code{smooth()}) is
    applied to the data. 
  }
  \item{mfrow}{
    Partitions the plot into sub-plots, eg if there are six segments in
    the segment list, mfrow might be set to c(2,3).  If the \code{mfrow}
    parameter is not supplied, the program estimates a reasonable layout
    for the plots.  
  }
  \item{addlines}{
    A matrix or vector of time values.  If \code{addlines} is a vector of
    time values, then a vertical line is drawn on plot \code{j} at the
    time given in \code{addlines[j]}. If \code{addlines} is a matrix, then
    a series of vertical lines are drawn on plot \code{j}, for the times
    given in \code{addlines[j,i]} for each column \code{i}.}
  \item{differ}{
    An integer, carries out nth order differentiation where n is the value
    of \code{differ}. Defaults to no differentiation. 
  }
}
\section{Side Effects}{
  If overlay is FALSE, one subplot is made per segment and the subplots
  are arranged in rows, each labelled with the   segment label. If
  overlay is TRUE, one large plot is made and the data   for all
  segments is overlaid.  In this case the x-axis is not labelled 
  since each segment will have a different start time and duration.
}
\seealso{
  \code{\link{emu.query}}, \code{\link{emu.track}}, \code{\link{uttplot}},
  \code{\link{dplot}} 
}
\examples{

## assumes a database called demo is available on your system and that
## the Emu system is installed. 

# find all Phonetic vowels in the database
#segs <- emu.query("demo", "*", "Phonetic=E")

#segplot( segs[1:10,], "F0" )

}
\keyword{misc}

