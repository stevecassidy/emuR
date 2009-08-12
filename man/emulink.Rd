\name{emulink}
\Rdversion{1.1}
\alias{emulink}
\title{
Establishs the link to an Emu installation
}
\description{
Without argument emulink() tries to read a configuration file and tries
to find the necessary package in the stored directory paths.
If this fails it tries to link against default directories. If this fails, the user
is asked to enter the path.
With paths argument the first two steps are ignored. 
}
\usage{
emulink(paths = "")
}
\arguments{
  \item{paths}{The directories seperated by ";" in which
  Emu libraries and tcllib >= 1.8 are installed on the system. Without paths argument link is established with default configurations.
}
}
\details{
The paths are added to the tcl auto_path variable to make the emuR library available that is part of the Emu Speech Database System \url{http://www.emu.sf.net}. 
}
\value{
After success the file path is returned where this configuration was written to.
}
\references{
\url{http://www.emu.sf.net} 
}
\author{
Tina John
}
\note{
Use emulink() without paths argument first.
It is not necessary to link an Emu installation to this libary.
You can use all functions of the library but \code{\link{emu.query}}, \code{\link{emu.requery}} and \code{\link{emu.track}}.
The functionality of \code{\link{emu.query}} and \code{\link{emu.track}} function is provided by the Emu Speech Database System \url{http://www.emu.sf.net} also. Thus export the respective files from the software and import it to R using
\code{\link{read.emusegs}} or \code{\link{read.trackdata}} respectively.
}
\seealso{
\code{\link{emu.query}}
\code{\link{emu.requery}}
\code{\link{emu.track}}
\code{\link{read.emusegs}}
\code{\link{read.trackdata}}
}
\examples{
\dontrun{emulink(".")}
}
\keyword{IO}