\name{ddiff}
\alias{ddiff}
\alias{ddiff.sub}
\title{ Differentiation of tracks}
\description{
    Differentiates a list, as returned by track, to the nth
    order, readjusting the index and ftime values each time.
    }
\usage{
   ddiff(dataset, n = 1, smoothing = TRUE)
}
\arguments{
  \item{dataset}{ track data object - a list as returned by track }
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

\keyword{math}

