\name{dlowess}
\alias{dlowess}

\title{Smooth data in an Emu trackdata object }
\description{
  Smooths each dataset in a trackdata object using a lowess smoother.
}

\usage{
dlowess(dataset, window = 1/3)
}

\arguments{
  \item{dataset}{A trackdata object as returned from `track'}
  \item{window}{The smoother span. This gives the proportion of points in the
          plot which influence the smooth at each value. Larger values
          give more smoothness.}
}

\details{
  This function uses the Emu dapply function to apply a lowess smoother
  to each token in the trackdata object 
}

\value{
  The smoothed trackdata object 
}

\references{ }

\author{ Richard Beare }

\note{Written for Marija Tabain, SHLRC, August 2002, primarily to be
  used with EMA data. It is strongly recommended that the user determine
  a correct window value for each new set of data} 

\seealso{\code{\link{dapply}}, \code{\link{dsmooth}},
  \code{\link{emu.track}}, \code{\link{lowess}}}

\examples{
foo <- emu.query("demo", "*", "Phonetic=a")
foo.track <- emu.track(foo, "fm")
dfoo.track <- dlowess(foo.track, window=1/3)
par(mfrow=c(1,2))
dplot(foo.track[1:10,])
dplot(dfoo.track[1:10,])
}

\keyword{ lowess }
\keyword{ smooth }