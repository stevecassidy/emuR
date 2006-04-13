\name{dip.spkr}
\alias{dip.spkr}
\title{Vector of speaker labels of the same length as segment list dip }
\usage{data(dip)}
\description{
   For dip see data(dip)
}
%\format{
%  NA
%
%}
%
\details{ 
   dip  = emu.query("kielread06", "*", "[Kanonic=aI | aU | OY]")
   dip.spkr = substring(utt(dip), 2, 3)
}

\seealso{
   \code{\link{segmentlist}}
}
%
%\examples{
%}

\keyword{emu}
\keyword{datasets}