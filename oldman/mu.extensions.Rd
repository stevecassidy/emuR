\" -*-nroff-*-
\name{mu.extensions}
\alias{mu.extensions}
\title{
Change the database file extensions
}
\usage{
mu.extensions(option)
}
\arguments{
\item{option}{
One of \code{"ESPS"}, \code{"SSFF"} or \code{"ACCOR"}. 
}}
\value{
None
}
\section{Side Effects}{
Sets the \code{"extensions"} option (see \code{options("extensions")} for current
setting) which determines which file extensions mu+ uses to search for
signal tracks in the database. 
}
\seealso{
mu.options
}
\examples{
## Use ESPS file extensions:
mu.extensions("ESPS")
}
\keyword{mu}
% Converted by Sd2Rd version 0.3-3.
