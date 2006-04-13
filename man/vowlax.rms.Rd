\name{vowlax.rms}
\alias{vowlax.rms}
\title{Trackdata of RMS energy from segment list vowlax }
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

}
%
\seealso{
   \code{\link{segmentlist}}
   \code{\link{trackdata}}   
}
%
%\examples{
%}


\keyword{datasets}