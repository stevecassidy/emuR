\" -*-nroff-*-
\name{mu.options}
\alias{mu.options}
\title{
Set up the mu+ options
}
\usage{
mu.options(default=F, \dots)
}
\arguments{
\item{default}{
If T, the default values of all options will be set if they are not
explicitly specified. 
}
\item{muhome}{
A pathname. Records the place where mu+ is installed. 
}
\item{dbdefault}{
A pathname. The default database directory for \code{mkdb}.
}
\item{templatedir}{
A pathname. The location of the \code{labed_template} and associated files. 
}
\item{internalspeaker}{
If T, the internal speaker will be used by default for speech playback.
If F, the external speaker or headphones will be used. 
}}
\value{
None.
}
\details{
This function can be used to set up the default values for these
options or set new values.  These options are stored using the Splus
\code{options} function, consequently their values may be queried or set
using \code{options}.  
}
\seealso{
options, mu.extensions, .First.mu
}
\examples{
# set the default database directory
mu.options(dbdefault="/dbase/english")
# find the value of the muhome option
options("muhome")
}
\keyword{mu}
% Converted by Sd2Rd version 0.3-3.
