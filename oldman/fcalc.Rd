\name{fcalc}
\alias{fcalc}
\title{
Arithmetic calculation on spectral data
}
\usage{
fcalc(fftdata, byrow=T, samfreq=20000, nyq=samfreq/2, 
      low=0, high=nyq, fun=sum, \dots)
}
\arguments{
\item{fftdata}{
FFT spectral values as returned from \code{muspec} with \code{dbspec=T} (the default) or \code{muslice}.
}
\item{byrow}{
If TRUE (default), the arithmetic calculation will be done on each row.
If FALSE, they will be done on each column.
}
\item{samfreq}{
Sampling frequency of the data (Default assumes 20 kHz).
}
\item{nyq}{
The upper limit of the spectrum (the Nyquist frequency or half the
sampling frequency).
}
\item{low}{
Starting frequency in Hz over which the function in \code{fun} is to be applied.
}
\item{high}{
Ending frequency in Hz over which the function is to be applied.
}
\item{fun}{
Any Splus arithmetic function to be applied on \code{fftdata}. (Default function is
\code{sum}).
}
\item{...}{
Extra arguments, which must be unique to \code{fcalc}, that are needed for
the \code{fun} function.
}}
\value{
\code{fcalc} returns the results of the calulation in \code{$vals}.
}
\details{
The spectral values (in dB's) are first converted back into energy
values, the function \code{fun} is then applied to the data and the result
is converted back into dB's.
}
\seealso{
muslice, muspec
}
\examples{
Calculate the variance of each spectrum in the 1-4 kHz range
> fcalc(fftdata, low=1000, high=4000, fun=var) 
}
\keyword{mu}
% Converted by Sd2Rd version 0.3-3.
