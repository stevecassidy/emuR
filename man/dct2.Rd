\name{dct}
\alias{dct}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Discrete Cosine Transformation }
\description{
  Obtain the coefficients of the discrete cosine transformation (DCT).
}


\usage{
dct(data, m = NULL, fit = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{ a vector or single column matrix of numeric values to which the 2nd order polynomial is to be fitted. }
  \item{fit}{ if F, return the DCT coefficients; 
if T, the values of the smoothed trajectory are returned
based on summing the cosine waves of the k lowest ordered
DCT coefficients, where k is the argument given below. }
  \item{m}{ The number of DCT coefficients that 
are returned or on which the smoothed trajectory is based.
Defaults to NULL which returns coefficients of
frequencies k = 0, 1,2 .. N-1 where N is the
length of the input signal, wav. If
fit = TRUE and k = NULL, then the the sum
of all the  cosine waves whose amplitudes
are the DCT coefficients are  returned - which
is equal to the original signal. k must be
between 2 and the length of the signal. }
}
\details{
  The function calculates the DCT coefficients for any
vector or single-columned matrix. The function can
also be used to obtain a smoothed trajectory of the
input data by summing the cosine waves derived from
the first few DCT coefficients.

The algorithm first reflects the input signal
about the last data point, N. Thus if the input
signal vec if of length N, the algorithm
creates a vector c(vec, rev(vec[-c(1,N)])).
and the R fft function is applied to this reflected signal.
The DCT coefficients are real part of  what is returned by
fft  i.e.  the amplitudes of
the cosine waves of frequencies k = 0, 1, 2, ...2 *(N-1)
radians per sample. The phase is zero in all cases.
The amplitudes are calculated in such a way such
that if these cosine waves are summed, the
original (reflected) signal is reconstructed.
What is returned by dct() are the amplitudes 
of the cosine waves (DCT coefficients)
up to a frequency of N radians/sample, i.e.
a vector of cosine wave amplitudes
that has the same
length as the original signal and of
frequencies k = 0, 1, 2, ... (N-1).  Alternatively, 
if fit=T,  a smoothed signal of the same
length as the original signal  is obtained 
based on a summation of the lowest ordered DCT coefficients.
This dct() algorithm returns very similar values
to DCT() with inv=F 
written by Catherine Watson and used in Watson & Harrington (1999).
}

\references{ Watson, C. & Harrington, J. (1999).
Acoustic evidence for dynamic formant trajectories
in Australian English vowels. Journal of
the Acoustical Society of America, 106, 458-468.

Zahorian, S., and Jagharghi, A. (1993).
Spectral-shape features versus formants
as acoustic correlates for vowels,
Journal of
the Acoustical Society of America,  94, 19661982. }
\author{ Jonathan Harrington }

\seealso{ 
\code{\link{plafit}}
\code{\link{by}}
}
\examples{
# obtain the first four DCT coefficients 
# (frequencies k = 0, 1, 2, 3) for some 
# first formant frequency data
vec <- vowlax.fdat[1,1]$data
dct(vec, k=4)

# obtain the corresponding smoothed
# trajectory
dct(vec, fit=T, k=4)
}

\keyword{math}

