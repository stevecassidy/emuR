\name{isol.l}
\alias{isol.l}
\docType{data}
\title{ Vector of phoneme labels from segment list isol}
\description{
  For isol see data(isol).
}
\usage{data(isol)}

}
\details{
stop.d = emu.query("ausisol06", "*", "[Phoneme=d & Start(Word, Phoneme)=1]")
isol = emu.requery(stop.d, "Phoneme", "Phoneme", sequence=1)
isol.fdat = emu.track(isol, "fm")
}

\seealso{
   \code{\link{segmentlist, trackdata}}
}
