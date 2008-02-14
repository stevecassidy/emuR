\name{demo.vowels.f0}
\alias{demo.vowels.f0}
\title{F0 track data for segment list demo.vowels}
\usage{data(demo.vowels.f0)}
\description{
   A track list of the demo database that is part of the Emu system.
   It is the result of get F0 data for the segment list demo.vowels (see data(demo.vowels)).
}
\format{
 An object with \$index, \$ftime and \$data
 
index: a two columned matrix with the range of the \$data rows that belong to the segment
ftime: a two columned matrix with the times marks of the segment
data: a one columned matrix with the F0 values

}

\details{
  A track list is created via emu.track() or via get data within the EMU Query Tool. 
}

\seealso{
   \code{\link{demo.all.rms}}
   \code{\link{segmentlist}}
   \code{\link{trackdata}}   
}




\keyword{datasets}