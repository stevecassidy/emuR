\name{trackdata}
\alias{trackdata}

\title{Track data object}
\description{
   A track data object is the result of emu.track().
}
   
\format{
  \item{$index}{a two columned matrix; each row keeps the first and last index of the $data rows that belong to one segment}
  \item{$ftime}{a two columned matrix; each row keeps the times marks of one segment}
  \item{$data}{a multi-columned matrix with the real track values for each segment}
}

   
\notes{
   The entire data track is retrieved for each segment in the segment list. The amount of data
   returned will depend on the sample rate and number of columns in the track requested. 
}

\seealso{ 
     \code{\link{emu.track}},  
     \code{\link{demo.vowels.fm}}
     \code{\link{demo.all.rms}}
}

\examples{
   data(demo.vowels.fm)
   data(demo.vowels)
   
   #Formant track data for the first segment of the segment list demo.vowels
   demo.vowels.fm[1]
  
}
\keyword{emu}
\keyword{classes}
