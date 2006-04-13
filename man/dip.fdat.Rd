\name{dip.fdat}
\alias{dip.fdat}
\title{Formant track data of the segment list dip}
\usage{data(dip)}
\description{
   A track list of the kielread06 database that is part of the Emu system.
   It is the result of get formant data for the segment list dip (data(dip)).
}

%\format{
%}
%
\details{ 
   dip  = emu.query("kielread06", "*", "[Kanonic=aI | aU | OY]")
   dip.fdat = emu.track(dip, "fm")
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