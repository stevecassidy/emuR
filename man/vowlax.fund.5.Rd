\name{vowlax.fund.5}
\alias{vowlax.fund.5}
\title{
   Fundamental frequency extracted from the temporal midpoint of the fundamental 
   frequency track data vowlax.fund from segment list vowlax
}
\usage{data(vowlax)}
\description{
  For vowlax see data(vowlax).
}
%\format{
%  NA
%
%}
%
%\details{ 
vowlax = emu.query("kielread06", "*", "Kanonic=a | E | I | O")
vowlax.fund = emu.track(vowlax, "F0")
vowlax.fund.5 = dcut(vowlax.fund, .5, prop=T)
}
%
\seealso{
   \code{\link{segmentlist}}
   \code{\link{trackdata}}
   \code{\link{dcut}}
}
%
%\examples{
%}

\keyword{emu}
\keyword{datasets}