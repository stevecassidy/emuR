\name{rate}
\alias{rate}
\title{
Find the sample rate of a track file.
}
\usage{
rate(utterance, track="samples", extn=options("extensions"))
}
\arguments{
\item{utterance}{
The utterance you wish find the sample rate for.
}
\item{track}{
The parameter track you want to know about. 
}
\item{extn}{
The file extensions to look for the track in.
}}
\value{
Returns the sample rate of the specified track in Hz. 
}
\seealso{
track, frames, play
}
\keyword{mu}
% Converted by Sd2Rd version 0.3-3.
