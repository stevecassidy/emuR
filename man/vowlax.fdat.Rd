\name{vowlax.fdat}
\alias{vowlax.fdat}
\title{Formant track data of the segment list vowlax}
\usage{data(vowlax)}
\description{
   A track list of the kielread06 database that is part of the Emu system.
   It is the result of get formant data for the segment list vowlax (data(vowlax)).
}
%\format{
%  NA
%
%}
%
\details{ 
   vowlax = emu.query("kielread06", "*", "Kanonic=a | E | I | O")
   vowlax.fdat = emu.track(vowlax, "fm")
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
