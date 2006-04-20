\name{isol.fdat}
\alias{isol.fdat}
\docType{data}
\title{ Formant track data of the Emu segment list isol}
\description{
  A track list of the ausisol06 database that is part of the Emu system. It is the result of get formant data for the segment list isol (data(isol)).
}
\usage{data(isol)}

}
\details{
stop.d = emu.query("ausisol06", "*", "[Phoneme=d & Start(Word, Phoneme)=1]")
isol = emu.requery(stop.d, "Phoneme", "Phoneme", sequence=1)
isol.fdat = emu.track(isol, "fm")
}

\seealso{
   \code{\link{segmentlist}}
   \code{\link{trackdata}}
}
\keyword{emu}
\keyword{datasets}