\" -*-nroff-*-
\name{play}
\alias{play}
\title{
Playback of any segment list
}
\usage{
play(playdata, rate, internalspeaker=F, pausedata=NULL, pauselen=1)
}
\arguments{
\item{playdata}{
A vector of sampled data values, which is output from \code{track}.
}
\item{rate}{
The sampling rate at which playback takes place. 
}
\item{internalspeaker}{
If TRUE play uses the internal speaker,
If FALSE (Default), the external speaker is used.
}
\item{pausedata}{
Specifies a vector specifying the intervals at which pauses are to be made (in
milliseconds).
}
\item{pauselen}{
Specifies the duration of the pauses in seconds.
}}
\section{Side Effects}{
The sampling rate at which you select for playback, could have an
effect on the naturality of the output. For natural sound, the playback
sampling rate will need to be the same as the recording sampling
rate. A higher playback rate will increase the speed of the output. A
lower rate will decrease it.
}
\seealso{
rate, track, talk, uttplot
}
\examples{
This example would introduce a one second pause at 50, 100, 150 ms relative to
the start of the file created with track.
> play (vals$data, 20000, pausdata=c(50, 50, 50), pauselen=1)
}
\keyword{mu}
% Converted by Sd2Rd version 0.3-3.
