\name{label}
\alias{label.emusegs}
\alias{label}
\alias{utt.emusegs}
\alias{utt}


\title{Get labels / utterances from segment list}
\description{
  label: extracts the labels from the segment list.
  utt: extracts the utterances from the segment list.

}
\usage{
    \method{label}{emusegs}(segs)
    \method{utt}{emusegs}(x)
}
\arguments{
 \item{segs}{ segment list}
 \item{x}{ segment list}

}

\value{
   label / utterance vector
   
}

\author{Jonathan Harrington}

\seealso{
   \code{\link{segmentlist} \link{start} \link{end}}
}

\examples{
   data(dip)
   #dip is a segment list - first ten segments only
   dip[1:10,]
   

   #extract labels from the segment list
   dips.labs = label(dip)
   dips.labs 
   
}

\keyword{methods}
