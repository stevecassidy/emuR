\name{vowlax}
\alias{vowlax}
\title{Emu segment list}
\usage{data(vowlax)}
\description{
   Segment list of the kielread06 database that is part of the Emu system.
   It is the result of a database query, that searched various German lax monophthongs at level Kanonic.
}
%\format{
%  NA
%
%}
%
\details{ 
vowlax = emu.query("kielread06", "*", "Kanonic=a | E | I | O")

}
%
\seealso{
   \code{\link{segmentlist}}
}
%
%\examples{
%}

\keyword{emu}
\keyword{datasets}