\name{demo.all.rms}
\alias{demo.all.rms}
\title{Emu track data for a rms track for segment list demo.all}
\usage{data(demo.all.rms)}
\description{
   A track list of the demo database that is part of the Emu system.
   It is the result of get rms data for the segment list demo.all (data(demo.all)).
}
\format{
 A object with $index, $ftime and $data
 
index: a two columned matrix with the range of the $data rows that belong to the segment
ftime: a two columned matrix with the times marks of the segment
data: a vector with the rms data

}

\details{
  A track list is created via emu.track() or via get data within the EMU Query Tool. 
}

\seealso{
   \code{\link{demo.vowels.fm}}
   \code{\link{segmentlist}}
   \code{\link{trackdata}}
}



\keyword{datasets}