\name{ddiff}
\alias{ddiff}
\title{ Differentiation of tracks}
\description{
    Differentiates a list, as returned by track, to the nth
    order, readjusting the index and ftime values each time.
    dataset: a list as returned by track
    n: the order of differentiation}
\usage{
   ddiff(dataset, n = 1, smoothing = T)
}
\arguments{
  \item{dataset}{ track data object }
  \item{n}{ the order of differentiation }
  \item{smoothing}{ if TRUE track is smoothed }
}

%\value{
%  ~Describe the value returned
%  If it is a LIST, use
%  \item{comp1 }{Description of 'comp1'}
%  \item{comp2 }{Description of 'comp2'}
%  ...
%}

\author{ Jonathan Harrington}

%\seealso{ ~~objects to See Also as \code{\link{~~fun~~}}, ~~~ }
%\examples{
%
%
%}
\keyword{emu}