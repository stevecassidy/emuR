\name{spect.utter}
\alias{spect.utter}
\alias{spectral.tilt}
\title{Calculate spectral tilt for speech data}
\description{
  \code{spectral.tilt} analyzes the rate-of-change in spectral tilt
  across an utterance.  \code{spect.utter} runs \code{spectral.tilt}
  on each  segment in an EMU segment list.
}

\usage{
spect.utter(utt.list, lsweight = F, plotduring = TRUE, window = 12, frameShift = 5, bt = 1000, tp = 5000)
}

\useage{
spectral.tilt(dataset, lsweight = F, plotduring = T, window = 12, frameShift = 5, bt = 1000, tp = 5000)
}
\arguments{
  \item{utt.list}{an EMU segment list}
  \item{dataset}{}
  \item{lsweight}{weights the primary regression with the linear amplitudes - this emphasizes the spectral peaks}
  \item{plotduring}{switches a display of primary regressions on and off}
  \item{window}{length of the FFT window in milliseconds }
  \item{frameShift}{frameshift between FFTs within a single token, also in milliseconds}
  \item{bt}{low-point of the bandpass filter}
  \item{tp}{highpoint of the bandpass filter}
}

\details{

  ERB-rate is used for frequency based on Moore &
  Glasberg's 1990 formula, and amplitude values are converted to dB.

  Analysis technique based on Smits et al. (1996) JASA
  Left edge of the first window in each token is at the start of the token. 
}

\value{
  Returns a list of:
  \item{primary}{returns the tilt values from each FFT window}
  \item{secondary}{returns the secondary tilt value, which is the slope
    value from a regression on the primary tilt values for each token} 
}

\references{Smits et al. (1996) JASA}
\author{Richard Beare}

\note{Written for Marija Tabain, SHLRC, August 2002, to look at changes
  in spectral tilt across vowel-fricative boundaries} 

\section{WARNING}{Note that there is no zero-padding of the data}

\seealso{\code{\link{spectral.tilt}}}

\examples{
library(emu)
segs <- emu.query("demo", "*", "[Phonetic=H -> Phonetic=i]")

# Take 1st 50 ms only
segs[,3] <- segs[,2]+30
# This is to centre the first FFT at the burst release
segs[,2] <- segs[,2]-6

result <- spect.utter(segs)
# plot the secondary tilt values
nplot(result$secondary, labs)

}

\keyword{spectral tilt}
\keyword{emu}
