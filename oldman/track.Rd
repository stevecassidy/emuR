\" -*-nroff-*-
\name{track}
\alias{track}
\title{
Extract track data for a segment list
}
\usage{
track(segs, param, cut=NULL, index=T)
}
\arguments{
\item{segs}{
A mu+ segment list. 
}
\item{param}{
One of the available parameter names (use \code{tracks()} to find which
parameters are available for the current database).
}
\item{cut}{
If this argument, a number between 0 and 1, is specified, each segment
is sampled at the corresponding point. For example, cut=0.5 will return
one sample per segment taken from the segment mid-point. 
}
\item{index}{
If TRUE, an index to the returned data vector is constructed consisting
of start and end positions for each segment within the data vector and
the starting and ending times of each segment.
}}
\value{
If \code{index} is \code{FALSE}, a matrix of data is returned consisting of the
concatenation of the parameter tracks for each segment.  The matrix may
have more than one column if the requested parameter is
multi-dimensional (for example the first three formants \code{"fm[1..3]"}).  If
the \code{cut} argument was used, each row of the matrix will correspond to
one segment.  If \code{index} is TRUE the return value has the following three
components:

\item{data}{
The concatenated matrix of data.
}
\item{index}{
A matrix with two columns giving the start and end indexes of each segment
within the \code{data} matrix.
}
\item{ftime}{
A matrix with two columns giving the start and end times of each
segment copied from the segment list.  Both the \code{index} and the \code{ftime}
components will have the same number of rows as the original segment
list.
}}
\seealso{
tracks, frames
}
\examples{
# First we find out which tracks are available for the current Utterances:


> tracks()
[1] "samples"    "F0"         "prob_voice" "rms"        "ac_peak"   
[6] "k1"         "fm[1..4]"   "bw[1..4]"  


# Now we extract the "samples" track (the raw sample speech data) for our
# segment list: 


my.samp <- track(my.segs, "samples")


# Next we extract the first three formants at the mid point of each
# segment:


my.form <- track(my.segs, "fm[1..3]", cut=0.5)
}
\keyword{mu}
% Converted by Sd2Rd version 0.3-3.
