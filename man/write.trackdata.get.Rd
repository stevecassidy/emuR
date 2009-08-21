\name{write.trackdata.get}
\Rdversion{1.1}
\alias{write.trackdata.get}
\title{This is the write.trackdata version for the a trackdata argument of type character}
\description{
It does the same like \code{write.trackdata} but with the additional commands that are necessary to handle the string to get the data.
}
\usage{
write.trackdata.get(trackdata, file)
}
\arguments{
  \item{trackdata}{
     as character value of the track data object variable
}
  \item{file}{
     file name
}
}
\author{
Tina John
}
\note{
For larger objects this function is faster than \code{write.trackdata}
}

\seealso{
\code{\link{write.trackdata}}
}

\keyword{IO}
