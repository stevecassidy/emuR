\name{buildtrack}
\alias{buildtrack}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Build trackdata objects from the output of by() }
\description{
  The buildtrack() function in the emu-R library can be used to build a list that is output from by() into a trackdata object.
}
\usage{
buildtrack(mylist, ftime = NULL, trackname = "")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mylist}{ a list that ist output from by() }
  \item{ftime}{ ftime }
  \item{trackname}{ name of track data object}
}

\author{ Jonathan Harrington }


\seealso{  \code{\link{by}} }
\examples{

p = by(vowlax.fdat[1:3,2], diff)
m = buildtrack(p)
\dontshow{readline(prompt = "Press return to advance.")}
p
}

\keyword{ emu }