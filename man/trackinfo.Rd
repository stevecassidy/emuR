\name{trackinfo}
\alias{trackinfo}
\title{Tracks names and track file extensions}
\description{
   Gets tracks names and track file extensions that are defined for an EMU data base.
}
\usage{
   trackinfo(template)
}
\arguments{
  \item{template}{The name of an EMU data base (name of the template file).}
}
\details{
  Data base must exist for the EMU system.
}
\value{
  2-columned matrix
  \item{first column}{name of track}
  \item{second column}{file extension of the track files}
}

\author{ Jonathan Harrington}

\examples{
   #tracks that are defined for data base kielread06
   trackinfo("kielread06")   
}
\keyword{ emu }
