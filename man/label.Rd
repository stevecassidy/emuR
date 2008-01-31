\name{Extract label and utterances from emu segment list}
\alias{label.emusegs}
\alias{utt.emusegs}

\title{Get labels / utterances from segment list}
\description{
  label: extracts the labels from the segment list.
  utt: extracts the utterances from the segment list.

}
\usage{
   label.emusegs(segs)
   utt.emusegs(segs)
}
\arguments{
 \item{segs}{ segment list}
}

\value{
   label / utterance vector
   
}

\author{Jonathan Harrington}

\seealso{
   \code{\link{segmentlist}}
}

\examples{
   data(dip)
   #dip is a segment list - first ten segments only
   dip[1:10,]
   \dontshow{readline(prompt = "Press return to advance.")}

   #extract labels from the segment list
   dips.labs = label(dip)
   dips.labs 
   
}

\keyword{datagen}
