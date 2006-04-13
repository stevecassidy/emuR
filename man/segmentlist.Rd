\name{segmentlist}
\alias{segmentlist}

\title{Segment list}
\description{
   A segment list is the result of emu.query() or read.emusegs().
}
   
\format{
  multi-columned matrix
  one row per segment
  \item{first column}{label}
  \item{second column}{segment onset time}
  \item{third column}{segment offset time}
  \item{fourth column}{utterance name}  
}

   
\seealso{ 
     \code{\link{emu.query}},  
     \code{\link{demo.vowels}}
}

\examples{
   data(demo.vowels)
   
   #demo.vowels is a segment list
   demo.vowels
}
\keyword{emu}
\keyword{classes}
