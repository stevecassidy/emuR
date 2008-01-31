\name{museg.write}
\alias{museg.write}
\title{
Write an Emu segment list to a file
}
\usage{
museg.write(seglist, file)
}
\description{}
\arguments{
\item{seglist}{
An Emu segment list
}
\item{file}{
The name of a file to write the segment list into. 
}}
\value{
None.
}
\section{Side Effects}{
The segment list is written to a file in the standard format, suitable
for input to \code{gettrack} or other Emu utility programs. 
}
\seealso{
  \code{\link{emu.query}}
}
\keyword{misc}

