\name{mudur}
\alias{mudur}
\alias{mustart}
\alias{muend}
\title{
Extract times and durations from an Emu segment list
}
\usage{
mustart(segs)
muend(segs)}
mudur(segs)}

\arguments{
  \item{segs}{An Emu segment list.}
}

\value{ A vector containing one element per segment in the segment list.
\code{mustart} \code{muend} extract the start and end times of each
segment in a segment list.  \code{mudur} extracts the duration of each
segment.


}
\references{ See the Emu documentation at:
  \url{http://www.shlrc.mq.edu.au/emu} }

\seealso{\code{\link{emu.query}}}
\examples{
## assumes a database called demo is available on your system and that
## the Emu system is installed. 

# find all Phonetic vowels in the database
segs <- emu.query("demo", "*", "Phonetic=vowel")

# plot a histogram of segment durations
durations <- mudur( segs )
hplot( durations, label(segs )

}
\keyword{misc}

