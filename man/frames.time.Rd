\name{frames.time}
\alias{frames.time}
\title{
Find the time and position of a data element.
}
\usage{
frames.time(dataset, datanum)
}
\description{}
\arguments{
\item{dataset}{
A dataset returned by \code{track} or \code{frames}.
}
\item{datanum}{
An integer, an index into the \code{data} component of \code{dataset}. 
}}
\value{
The segment number which contains the element \code{datanum} of \code{dataset$data}.
}
\details{
The dataset returned from \code{track} or \code{frames} consists of a matrix of
data (the \code{data} component) and two index components (\code{index} and
\code{ftime}). The data for all segments is concatenated together in
\code{$data}.  This function can be used to find out which segment a
particular row of \code{$data} corresponds to. 
}
\seealso{
track, frames
}
\keyword{misc}
% Converted by Sd2Rd version 0.3-3.
