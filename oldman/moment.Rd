\name{moment}
\alias{moment}
\title{
Calculate the spectral moment.
}
\usage{
moment(specvals, least=T, nyq=10000, low=0, high=nyq)
}
\arguments{
\item{specvals}{
A data matrix representing spectral values, as returned by \code{muspec}.
}
\item{least}{
If TRUE, normalise each spectrum so that its minimum is 0dB.
}
\item{nyq}{
The maximum frequency of the spectrum (the nyquist frequency: half the
sampling frequency).
}
\item{low}{
Set the low end of the spectral range to consider (Hz).
}
\item{high}{
Set the high end of the spectral range to consider (Hz). 
}}
\value{
A list with two components:

\item{first}{
The first spectral moment (spectral centre of gravity) for each row of
the input matrix.
}
\item{second}{
The second spectral moment (spectral variance or moment of inertia) for
each row of the input matrix.
}}
\details{


}
\seealso{
muspec
}
\keyword{mu}
% Converted by Sd2Rd version 0.3-3.
