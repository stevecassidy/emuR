\name{dim.trackdata}
\alias{dim.trackdata}

\title{ A function which returns dimension attributes of track data objects}
\description{
  The function returns the dimension attributes of a track data object.
}
\usage{dim.trackdata(x)}

\arguments{
\item{x}{track data object}
}

\details{
   The function returns the dimension attributes of a track data object as the number of segments x number of tracks.
c(nrow(x$index), ncol(x$data))
}
\author{Jonathan Harrington}

\examples{
> dim.trackdata(isol.fdat)
[1] 13  4

}
\keyword{datasets}
