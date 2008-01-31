\name{demo.all}
\alias{demo.all}
\title{Emu segment list}
\usage{data(demo.all)}
\description{
   Segment list of the demo database that is part of the Emu system.
   It is the result of a database query, that searched all segments at level Phonetic.
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
   \code{\link{demo.vowels}}
   \code{\link{segmentlist}}
}

\examples{
    packpath = .path.package(package = "emu", quiet = FALSE)
      sepa = .Platform$file.sep
      nfile = "demo-all.seg"
      filepath = paste(packpath,sepa,"demo",sepa,nfile,sep = "")
      
    
    demo.all <- read.emusegs(filepath)
}


\keyword{datasets}
