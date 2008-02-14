\name{demo.vowels.fm}
\alias{demo.vowels.fm}
\title{Formant track data for segment list demo.vowels}
\usage{data(demo.vowels.fm)}
\description{
   A track list of the demo database that is part of the Emu system.
   It is the result of get fm data for the segment list demo.vowels (see data(demo.vowels)).
}
\format{
 An object with $index, $ftime and $data
 
index: a two columned matrix with the range of the $data rows that belong to the segment
ftime: a two columned matrix with the times marks of the segment
data: a three columned matrix with the formant values of the first three formants for each segment  

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