\" -*-nroff-*-
\name{norm}
\alias{norm}
\title{
Normalise speech data
}
\usage{
norm(data, speakerlabs, type="gerst", rescale=F)
}
\arguments{
\item{data}{
A matrix of data. Can be either an n-columned matrix or a trackdata
object as returned by \code{track}.
}
\item{speakerlabs}{
A parallel vector of speaker labels.
}
\item{type}{
The type of extrinsic normalisation to be performed on data. type can
be \code{"nearey"}, \code{"cen"}, \code{"lob"}, \code{"gerst"} (default), for normalisation
according to Nearey, centroid method, Lobanov, or Gerstman.
}
\item{rescale}{
Currently only works for Lobanov normalisation. The normalised values are
multiplied by the standard deviation and then the mean is added, where the
standard deviation and mean are across all original speakers' unnormalised
data.
}}
\value{
Normalised values of data are retuned, having the same structure as data.
}
\details{
Types of normalisation:
\code{"nearey"}, Nearey : Find the log of each data element and subtract
from each the mean of the logarithmic data. 
\code{"cen"}, centroid: Find the mean of the data column and subtract it from each
data element in that column. 
\code{"lob"}, Lobanov: Find the mean and standard deviation of the data. Subtract
the mean from each data element and devide each result by the standard
deviation. 
"gerst", Gerstman: Subtract from the data the minimun formant value then devide
by the formant range. 
}
\seealso{
track
}
\keyword{mu}
% Converted by Sd2Rd version 0.3-3.
