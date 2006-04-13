\name{label}
\alias{label}
\title{Get labels from segment list}
\description{
  label extracts the labels from the segment list.
}
\usage{
   label(segs)
}
\arguments{
 \item{segs}{ segment list}
}
}
\value{
   label vector
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

\keyword{emu}
