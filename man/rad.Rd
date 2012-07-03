\name{rad}
\alias{rad}

\title{ Function to  convert between Hertz and Radians }
\description{
    convert between Hertz and Radians 
}
\usage{
rad(vec, samfreq = 20000, hz = TRUE)
}

\arguments{
  \item{vec}{A numerical vector of frequencies in Hz or radians }
  \item{samfreq}{A single element numerical vector of the sampling frequency. Defaults to 20000 Hz }
  \item{hz}{ Logical. If T, convert from Hz to radians otherwise from radians to hz }
}



\author{ Jonahtan Harrington}

\seealso{
\code{\link{help}}
}

\examples{
# 4000 Hz in radians at a sampling frequency of 8000 Hz
rad(4000, 8000)
# pi/2 and pi/4  radians in Hz at a sampling frequency of 10000 Hz
rad(c(pi/2, pi/4), 10000, FALSE)

}

\keyword{math}

