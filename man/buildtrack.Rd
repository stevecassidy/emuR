\name{buildtrack}
\alias{buildtrack}
\title{ Build trackdata objects from the output of by() }
\description{
  buildtrack() converts a list that is the output
of by.trackdata() into a trackdata object if
the list components are matrices whose rows are
successive values in time.
}
\usage{
    buildtrack(mylist, ftime = NULL, trackname = "")
}
\arguments{
  \item{mylist}{ a list that ist output from by() }
  \item{ftime}{ ftime }
  \item{trackname}{ name of track data object}
}

\details{
  The default of by.trackdata() is to return a list. If 
each element of the list consists of a matrix
whose rows are values occurring at the times given
by the row dimension names of the matrix, then buildtrack()
can be used to convert the list into a trackdata object.
If the times are not given in the row dimension names,
then these can be supplied as an additional argument to
buildtrack()
}

\author{ Jonathan Harrington }


\seealso{  \code{\link{by}} }
\examples{
   #vowlax.fdat is a track data objects of formant of the vowlax segment list
   #calculate the difference between adjacent formant values
   p = by(vowlax.fdat[1,2],INDICES=NULL, diff)
   
   p
   
   
   #now build a track data object out of these values
   m = buildtrack(p)
   
   m
}

\keyword{manip}