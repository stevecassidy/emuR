\name{mustart}
\alias{mustart}
\title{
Extract times and durations from a MU+ segment list
}
\usage{
mustart(segs)
muend(segs)
mudur(segs)
}
\arguments{
\item{segs}{
A MU+ segment list.
}}
\value{
A vector containing one element per segment in the segment list.  
\code{mustart()} and \code{muend()} extract the start and end times of each
segment in a  segment list.  \code{mudur()} extracts the duration of each segment.


}
\seealso{
phon, mkdb
}
\keyword{mu}
% Converted by Sd2Rd version 0.3-3.
