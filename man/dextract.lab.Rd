\name{dextract.lab}
\alias{dextract.lab}
\title{
Extract a subset of data from a trackdata object
}
\description{
Extract a subset of data from a trackdata object according to the 
label in the parallel label vector.
}
\usage{
dextract.lab(trackdata, labs, labtype=unique(labs))
}
\arguments{
\item{trackdata}{
A trackdata object returned from \code{track}.
}
\item{labs}{
A vector of labels parallel to \code{trackdata$index}, i.e. one for each
segment in the trackdata. 
}
\item{labtype}{
A vector of labels for which data is to be extracted.
}}
\value{
A trackdata object which is a subset of \code{trackdata} containing only the
data for those labels in \code{labtype}.  The result has the same components
as the input \code{trackdata}:

\item{data}{
A vector or matrix of numerical data.
}
\item{index}{
A two column matrix giving the start and end indeces into the data
vector for each segment.
}
\item{ftime}{
A two column matrix giving the start and end times for each segment.
}}
\seealso{
track, dextract, get.time.element, frames.time
}
\examples{
data(emu.voweldata)
# get just the data for the e: and E vowels
eE.data <- dextract.lab(vdata, label(vsegs), c("e:", "E"))
}
\keyword{misc}

