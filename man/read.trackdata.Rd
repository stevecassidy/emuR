\name{read.trackdata}
\alias{read.trackdata}
\title{Load track data from file}
\description{
  read data from a file into a trackdata object, the files
  contain the data and time components of the object, they're
  produced by gettrack (within the EMU System) and \code{\link{write.trackdata}}.
}
\usage{
   read.trackdata(filename, trackname = "data")
}

\arguments{
  \item{filename}{ file name}
  \item{trackname}{track name of the a track that is written in FILE.
                   without trackname, track is defined as data in the returned track data object}
}

\value{
  track data object
}

\author{Jonathan Harrington}

\seealso{\code{\link{write.trackdata}}}

\examples{
   data(dip)
   #Formant track data of the segment list dip (see data(dip)) - first segment only
   dip.fdat[1]
   write.trackdata(dip.fdat, "emu.write.track.example.txt")

   #There is a file emu_write.track.example.txt written to R_HOME/
   #that includes the track data
   
   #Now load the track data into R
   dip.fdat.rl = read.trackdata("emu.write.track.example.txt", "fm")
   dip.fdat.rl[1] 
   
   #Now load the track data into R without argument trackname
   dip.fdat.rl = read.trackdata("emu.write.track.example.txt")
   dip.fdat.rl[1] 
   unlink("emu.write.track.example.txt")
}
%\keyword{emu}
\keyword{IO}
