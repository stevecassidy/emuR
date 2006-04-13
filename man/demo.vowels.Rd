\name{demo.vowels}
\alias{demo.vowels}
\title{Emu segment List}
\usage{data(demo.vowels)}
\description{
   Segment list of the demo database that is part of the Emu system.
   It is the result of a database query, that searched all vowel segments at level Phonetic.
}
\format{
  First Column labels
  Second start time of the segment
  Third  end time of the segment
  Fourth utterance name of the utterance the segment was found

}

\details{
  A segment list is created via emu.query() or by using the EMU Query Tool. 
}

\seealso{
   \code{\link{demo.all}}
   \code{\link{segmentlist}}
}

\examples{
   packpath = .path.package(package = "emu", quiet = FALSE)
     sepa = .Platform$file.sep
     nfile = "demo-vowels.seg"
     filepath = paste(packpath,sepa,"demo",sepa,nfile,sep = "")
    
 demo.vowels <- read.emusegs(filepath)
}

\keyword{emu}
\keyword{datasets}