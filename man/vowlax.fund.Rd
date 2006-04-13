\name{vowlax.fund}
\alias{vowlax.fund}
\title{Fundamental frequency track data from segment list vowlax }
\usage{data(vowlax)}
\description{
   A track list of the kielread06 database that is part of the Emu system.
   It is the result of get F0 data for the segment list vowlax (see data(vowlax)).
}
%\format{
%  NA
%
%}
%
\details{ 
vowlax = emu.query("kielread06", "*", "Kanonic=a | E | I | O")
vowlax.fund = emu.track(vowlax, "F0")

}
%
\seealso{
   \code{\link{segmentlist}}
   \code{\link{trackdata}}   
}
%
%\examples{
%}

\keyword{emu}
\keyword{datasets}