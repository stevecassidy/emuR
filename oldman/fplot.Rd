\name{fplot}
\alias{fplot}
\title{
A function for plotting spectra
}
\usage{
fplot(fftdata, labs=NULL, which=NULL, colour=T, linetype=F,
      samfreq=20000, nyq=samfreq/2, xlab="Frequency (Hz)", 
      ylab="Intensity (dB)", low=0, high=nyq, dbrange=NULL, 
      axes=T, main="", average=F, smoothing=F, points=20, 
      coeff=F, type="l", super=F, legn="tl", cex=1)
}
\arguments{
\item{fftdata}{
FFT spectral values as returned from \code{muspec} with \code{dbspec=T} (the default) or \code{muslice}.
}
\item{labs}{
A vector of labels parallel to \code{fftdata}.
}
\item{which}{
Select one of the label-types for plotting.
}
\item{colour}{
If colour is TRUE, the spectra and labels will be plotted in different colours.
}
\item{linetype}{
If linetype is TRUE, the spectra will be plotted with different linetypes. This
is useful for plots that will be printed.
}
\item{samfreq}{
Determines the x-axis range, nyquest frequency and the default high frequency range.
}
\item{nyq}{
The upper limit of the spectrum (the nyquist frequency).
}
\item{xlab,}{
Titles for the x and y axes.
}
\item{low,}{
To specify a low and high frequncy range. (Default range is 0-10000 Hz)
}
\item{dbrange}{
Specify a range for the y-axis in db.
}
\item{axes}{
If TRUE axes are drawn on the plot.
}
\item{main}{
A main axis title for the plot.
}
\item{average}{
If TRUE, will compute the average spectra for each different label-type.
}
\item{smoothing}{
If TRUE, will compute cepstrally smoothed spectra (default to 20 cepstral
coefficients), then plot the corresponding smoothed spectra.
}
\item{points}{
Value of the cepstral coefficient, roughly 20 for a sampling rate of 20kHz.
}
\item{coeff}{
If TRUE, the values of the cepstral coefficients will be returned.
}
\item{type}{
The way the points/lines are represented. See Splus \code{plot}. Default is \code{"l"}, a
line plot.
}
\item{super}{
Superimpose FFTs that occur in successive rows of fftdata on the same plot.
}
\item{legn}{
The position to draw the legend, one of \code{"tl"} (top-left), \code{"tr"} 
(top-right), \code{"bl"} (bottom-left), \code{"br"} (bottom-right) or \code{"loc"}
(for locator) which requires you to position the lengend with the mouse.
}
\item{cex}{
The \code{cex} parameter passed to \code{par} to control text size on the plot. 
}}
\value{
If \code{coeff=T}, values of cepstral coefficients will be returned.
}
\section{Side Effects}{
Spectral plots are generated.
}
\seealso{
muslice, muspec
}
\examples{
# e.g. Extract and plot the cepstrally smoothed average spectral values of the
# [T] and [S] segments centered at the midpoint from the demo.utts database.
> segs _ phon(demo.utts, "Phonetic='T/S'", find="Phonetic")
> mvals _ muspec(segs, centering=T, offset=0.5)
> labs _ label(segs)
> fplot(mvals$spec, labs, average=T, smoothing=T)
}
\keyword{mu}
% Converted by Sd2Rd version 0.3-3.
