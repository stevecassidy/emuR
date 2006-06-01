\name{Start and end times for segment lists and trackdata ojects}
\alias{start.emusegs}
\alias{end.emusegs}
\alias{start.trackdata}
\alias{end.trackdata}

\title{ Start and end times for EMU segment lists and trackdata objects }

\description{
Obtain start and end times for EMU segment lists and
trackdata objects
}

\usage{start.emusegs(x)
end.emusegs(x)
start.trackdata(x)
end.trackdata(x) }

\arguments{
  \item{x}{ a segment list or a trackdata object }
}

\details{
The function returns the start and/or end
times of either a segment list or a trackdata object.
The former refers to the boundary times of
segments, the latter the start and end times
at which the tracks from segments occur.
start.emusegs and end.emusegs give
exactly the same output as mustart and muend
respectively.
}

\value{
A vector of times.}

\examples{
# start time of a segment list
start(polhom)
# duration of a segment list
end(polhom) - start(polhom)
# duration from start time of segment list
# and start time of parallel EPG trackdata
start(polhom) - start(polhom.epg)

}

\author{ Jonathan Harrington }


\seealso{ 
\code{\link{tracktimes}}
}

\keyword{emu}