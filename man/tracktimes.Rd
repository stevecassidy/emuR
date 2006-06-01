\name{tracktimes}
\alias{tracktimes}
\title{ Get the track times from EMU trackdata objects }
\description{
  The function obtains the times at which track values occur.
}

\usage{tracktimes(trackdata) }

\arguments{
  \item{trackdata}{ An EMU trackdata object, or a matrix 
of track values obtained at a single time point using dcut() }
}

\details{
  Every $data value in a trackdata object is
associated with a time at which it occurs
in the utterance. This function returns those
times.
}

\examples{
# track time values for a trackdata object
times <- tracktimes(vowlax.fdat)
# track time values for a matrix of trackdata values
# at  the temporal midpoint
tracktimes(dcut(vowlax.fdat[1:3,], 0.5, prop=T))}

\author{ Jonathan Harrington }


\seealso{ 
\code{\link{start.trackdata}}
\code{\link{end.trackdata}}
\code{\link{start.emusegs}}
\code{\link{end.emusegs}}
}

\keyword{emu}