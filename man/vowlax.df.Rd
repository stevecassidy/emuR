\name{vowlax.df}
\alias{vowlax.df}
\title{Data frame of various parameters and labels of the same length as the segment list vowlax }
\usage{data(vowlax)}
\description{
   For vowlax see data(vowlax))
}
%\format{
%  NA
%
%}
%
\details{ 
   vowlax = emu.query("kielread06", "*", "Kanonic=a | E | I | O")
   vowlax.spkr = substring(utt(vowlax), 2, 3)
   vowlax.l = label(vowlax)
   wordlax.l = emu.requery(vowlax, "Kanonic", "Word", justlabels=T)
   vowlax.fdat = emu.track(vowlax, "fm")
   vowlax.fdat.5 = dcut(vowlax.fdat, .5, prop=T)
   vowlax.fund = emu.track(vowlax, "F0")
   vowlax.fund.5 = dcut(vowlax.fund, .5, prop=T)
   vowlax.rms = emu.track(vowlax, "rms")
   vowlax.rms.5 = dcut(vowlax.rms, .5, prop=T)

   vowlax.df= data.frame(F1=vowlax.fdat.5[,1], F2=vowlax.fdat.5[,2], F3=vowlax.fdat.5[,3], 
   f0=vowlax.fund.5, rms=vowlax.rms.5, dur=mudur(vowlax), phonetic=vowlax.l, word=wordlax.l, speaker=vowlax.spkr)

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