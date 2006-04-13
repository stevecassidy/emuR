\name{cbind.trackdata}
\alias{cbind.trackdata}
\alias{cbind}

\title{ A method of the generic function cbind for objects of class \'trackdata\'}
\description{
   Different track data objects from one segment list are bound by combining the $data
   columns of the track data object by columns. 
}
\usage{
   cbind(...)
}

\arguments{
  \item{\dots}{ track data objects }
}

\details{
   All track data objects have to be track data of the same segment list. 
   Thus $index and $ftime values have to be identically for all track data objects.
   Track data objects are created by emu.track().
   The number of rows of the track data objects must match.
}

\value{
  A track data object with the same $index and $ftime values of the source track data objects and 
  with $data that includes all columns of $data of the source track data objects.
}

\author{Jonathan Harrington}


\seealso{ 
   \code{\link{cbind}},
   \code{\link{rbind.trackdata}}
   \code{\link{trackdata}}
   \code{\link{emu.track}}
}

\examples{
   data(vowlax)
   
   #segment list vowlax - first segment only 
   vowlax[1,]
   
   #F0 track data object for vowlax - first segment only 
   vowlax.fund[1,]
   
   #rms track data object for vowlax - first segment only 
   vowlax.rms[1,]
   \dontshow{readline(prompt = "Press return to advance.")}   
   
   #now combine both track data objects
   fund.rms.lax = cbind(vowlax.fund, vowlax.rms)
   
   #the combined track data object - first segment only
   #The first column keeps vowlax.fund data, the second keeps vowlax.rms data 
   fund.rms.lax[1,]

}

\keyword{emu}
\keyword{methods}
