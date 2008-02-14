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


\author{ Jonathan Harrington}


\keyword{math}

