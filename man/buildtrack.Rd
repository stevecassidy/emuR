\name{buildtrack}
\alias{buildtrack}
\title{ Build trackdata objects from the output of by() }
\description{
  This function can be used to build a list that is output from by(..., simplify=F) into a trackdata object.
}
\usage{
    buildtrack(mylist, ftime = NULL, trackname = "")
}
\arguments{
  \item{mylist}{ a list that ist output from by() }
  \item{ftime}{ ftime }
  \item{trackname}{ name of track data object}
}

\author{ Jonathan Harrington }


\seealso{  \code{\link{by}} }
\examples{
   #vowlax.fdat is a track data objects of formant of the vowlax segment list
   #calculate the difference between adjacent formant values
   p = by(vowlax.fdat[1,2], diff)
   \dontshow{readline(prompt = "Press return to advance.")}
   p
   \dontshow{readline(prompt = "Press return to advance.")}
   
   #now build a track data object out of these values
   m = buildtrack(p)
   \dontshow{readline(prompt = "Press return to advance.")}
   m
}

\keyword{ emu }