\" -*-nroff-*-
\name{segplot}
\alias{segplot}
\title{
Segment plot
}
\usage{
segplot(segs, param, labels, ylim, ylab = "", main = "", zeros = F, 
	 pdat = 0.5, onset = 0, offset = 0, col = T, linetype = F
	 smoothing = F, mfrow, addlines, differ )
}
\arguments{
\item{segs}{
A mu+ segment list.
}
\item{param}{
One of the valid parameter tracks for this segment list. Use tracks()
to find the available tracks.
}
\item{ylim}{
Limit the y-axis range for all plots. 
}
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
If the track being plotted is \code{"F0"}, then if zeros is TRUE the regions
with probability of voicing less than \code{pdat} will not be plotted. 
}
\item{pdat}{
The probability of voicing threshold for \code{zeros}. 
}
\item{onset}{
The plot for each segment will start this much to the left of the start
of the segment. 
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
If smoothing is TRUE, a non-linear smoother (\code{smooth()}) is applied to
the data. 
}
\item{mfrow}{
Partitions the plot into sub-plots, eg if there are six segments in the
segment list, mfrow might be set to c(2,3).  If the \code{mfrow} parameter is
not supplied, the program estimates a reasonable layout for the plots. 
}
\item{addlines}{
A matrix or vector of time values.  If \code{addlines} is a vector of time
values, then a vertical line is drawn on plot \code{j} at the time given in
\code{addlines[j]}. If \code{addlines} is a matrix, then a series of vertical lines
are drawn on plot \code{j}, for the times given in \code{addlines[j,i]` (`i}=1, 2,
3\dots{})
}
\item{differ}{
An integer, carries out nth order differentiation where n is the value
of \code{differ}. Defaults to no differentiation. 
}}
\section{Side Effects}{
Generates multiple plots, one for each segment in the segment list, on
the specified parameter.  If overlay is FALSE, one subplot is made per
segment and the subplots are arranged in rows, each labelled with the
segment label. If overlay is TRUE, one large plot is made and the data
for all segments is overlaid.  In this case the x-axis is not labelled
since each segment will have a different start time and duration.
}
\seealso{
track, uttplot, dplot
}
\examples{
> segs
      start       end                           utt 
E "12489.4" "12586.9" "/home/dbase/dbase1/msadb001"
@ "12916.9" "12982.2" "/home/dbase/dbase1/msadb001"
E "13155.2" "13240.4" "/home/dbase/dbase1/msadb001"
> segplot(segs, "fm[1..3]")
}
\keyword{mu}
% Converted by Sd2Rd version 0.3-3.
