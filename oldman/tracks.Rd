\" -*-nroff-*-
\name{tracks}
\alias{tracks}
\title{
Find the available tracks
}
\usage{
tracks(utterance, duplicates=FALSE, extn=options("extensions"))
}
\arguments{
\item{utterance}{
An utterance name or an utterance vector.  If a vector is provided, the
first element is taken as the utterance to be examined. 
}
\item{duplicates}{
If duplicates is TRUE, duplicate entries will not be removed from the
result. The default behaviour is to remove duplicates. 
}
\item{extn}{
A set of file extensions in which to look for parameter tracks. 
}}
\value{
A vector of parameter names corresponding to the parameters that may be
extracted from the current utterance. 
}
\seealso{
track, frames
}
\examples{
tracks("/mu/mu/MUDEMO/msajc001", extn=Extns.SSFF )
}
\keyword{mu}
% Converted by Sd2Rd version 0.3-3.
