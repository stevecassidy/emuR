\name{vowlax.rms.5}
\alias{vowlax.rms.5}
\title{
   Vector of RMS energy values at the temporal midpoint extracted 
   from track data of RMS energy vowlax.rms from segment list vowlax 
}
\usage{data(vowlax)}
\description{
   For vowlax see data(vowlax)
}
%\format{
%  NA
%
%}
%
\details{ 
vowlax = emu.query("kielread06", "*", "Kanonic=a | E | I | O")
vowlax.rms = emu.track(vowlax, "rms")
vowlax.rms.5 = dcut(vowlax.rms, .5, prop=T)

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