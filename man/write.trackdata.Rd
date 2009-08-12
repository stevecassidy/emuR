\name{write.trackdata}
\alias{write.trackdata}

\title{ Write track data objects to file }
\description{
   The track data object can be saved to a text file in a format 
   suitable for loading into other applications. Single point data is saved in a simple table. 
   Multiple point per segment data is stored in columns with more than one entry per segment.  
   Use \code{\link{read.trackdata}} to load the file into R. 
}
\usage{
   write.trackdata(trackdata, file)
}

\arguments{
  \item{trackdata}{ track data object or track data object as character}
  \item{file}{ file name }
}

\value{
   a file with the track data is written to the given path
}

\author{ Jonathan Harrington}

\seealso{\code{\link{read.trackdata}} }
\examples{
   data(dip)
   #Formant track data of the segment list dip (see data(dip)) - first segment only
   dip.fdat[1]
   \dontrun{write.trackdata(dip.fdat, "emu.write.track.example.txt")}
   
   #There is a file emu.write.track.example.txt would have been written to R_HOME/
   #that includes the track data
   
   \dontrun{unlink("emu.write.track.example.txt")}

}

\keyword{IO}

