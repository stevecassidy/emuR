\name{randomise.segs}
\alias{randomise.segs}
\title{
Randomise or Reverse items in a segment list
}
\usage{
randomise.segs(segs, bwd=F, rand=F)
}
\arguments{
\item{segs}{
A MU+ segment list.
}
\item{bwd}{
Reverse the order of the segment lists rows.
}
\item{rand}{
Randomise the order of the segment lists rows.
}}
\value{
A segment list containing the original elements, but the elements now in a
different row.
}
\section{Side Effects}{
If the segment list has only one element, the list will not differ from the
original.
}
\seealso{
phon
}
\keyword{mu}
% Converted by Sd2Rd version 0.3-3.
