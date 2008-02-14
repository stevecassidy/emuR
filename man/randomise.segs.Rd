\name{randomise.segs}
\alias{randomise.segs}
\title{
  Randomise or Reverse items in a segment list
}
\usage{
randomise.segs(segs,  rand=TRUE, bwd=FALSE)
}
\description{
  Randomises or Reverses items in a segment list

}
\arguments{
\item{segs}{
An Emu segment list.
}
\item{bwd}{
  If T, reverse the order of the segment list.
}
\item{rand}{
  If T, randomise the order of the segment lists (default).
}}
\value{
  A segment list containing the original elements in random or reversed
  order. This is useful if the segment list is to be used as the source
  for a set of stimuli in a perception experiment.
}
\seealso{
  \code{\link{emu.query}}
}
\examples{
data(vowlax)
## assumes a database called demo is available on your system and that
## the Emu system is installed. 

# all Phonetic vowels in the database
segs <- vowlax

# randomise the segment list
rsegs <- randomise.segs( segs )

}
\keyword{misc}
