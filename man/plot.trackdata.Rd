\name{plot.trackdata}
\alias{plot.trackdata}

\title{ Plot trackdata }

\description{
     Plot trackdata from a single segment
}
\usage{
plot.trackdata(trackdata, timestart = NULL, ...)
}

\arguments{
  \item{trackdata}{ A single-columned trackdata object from one segment.}
  \item{timestart}{ if NULL, then trackdata is plotted between trackdata$ftime[1] and trackdata$ftime[2]. Otherwise it is
plotted between timestart and trackdata$ftime[2] - trackdata$ftime[1] + timestart}
  \item{\dots}{ Further graphical parameters may also be passed as arguments,
          see 'par'.}
}
\details{
     The function plots a single segment of single-columned trackdata$data as a function of time.}

\author{ Jonathan Harrington}

\seealso{ \code{\link{plot}}, \code{\link{dplot}}}
\examples{

   # Fundamental frequency of the 10th segment
   plot(vowlax.fund[10,])
	\dontshow{readline(prompt = "Press return to advance.")}
   # the same with labels and a different linetype
   plot(vowlax.fund[10,], ylab="f0 Hz", xlab="time (ms)", col=2, type="l")
	\dontshow{readline(prompt = "Press return to advance.")}
   # Fundamental frequency of the 10th segment with a start time of zero.
   plot(vowlax.fund[10,], timestart=0)
	\dontshow{readline(prompt = "Press return to advance.")}
   # The first three formants of the first segment in vowlax.fdat
   ylim = range(vowlax.fdat[1,1:3]$data)
   for(j in 1:3){
      plot(vowlax.fdat[1,j], ylim=ylim, col=j, xlab="", ylab="")
      par(new=T)
   }
	\dontshow{readline(prompt = "Press return to advance.")}
   par(new=F)

   # f0 from the first 10 segments synchronised with a start time of zero.
   ylim = range(vowlax.fdat[1:10,2]$data)
   xlim = c(0, max(vowlax.fdat$ftime[1:10,2] - vowlax.fdat$ftime[1:10,1]))
   for(j in 1:10){
      plot(vowlax.fdat[j,2], timestart=0, ylim=ylim, xlim=xlim,  xlab="", ylab="")
      par(new=T)
   }
   	\dontshow{readline(prompt = "Press return to finish.")}
   par(new=F)

}

\keyword{ emu }
