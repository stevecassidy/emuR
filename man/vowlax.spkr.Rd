\name{vowlax.spkr}
\alias{vowlax.spkr}
\title{Vector of speaker labels of the same length as segment list s.vow }
\usage{data(vowlax)}
\description{
   For s.vow see data(vowlax)
}
%\format{
%  NA
%
%}
%
\details{ 
vowlax = emu.query("kielread06", "*", "Kanonic=a | E | I | O")
vowlax.spkr = substring(utt(vowlax), 2, 3)
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