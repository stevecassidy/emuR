\name{splitmat}
\alias{splitmat}
\title{
Split data according to a set of labels
}
\usage{
splitmat(data, labs, filename="file", labelfile=T)
}
\arguments{
\item{data}{
A vector or matrix of data.
}
\item{labs}{
A label file parallel to \code{data}.
}
\item{filename}{
the object name prefix for the new objects being created, ".n" is
appended to this where n is an integer between 1 and the number of
unique labels in \code{labs}.
}
\item{labelfile}{
  If TRUE, a label vector is created for each new object called "l.file.n".
}}
\value{
  A matrix of object names and the corresponding label.
}
\seealso{
  \code{\link{emu.track}}, \code{\link{emu.query}}
}
\examples{
## assumes a database called demo is available on your system and that
## the Emu system is installed. 

segs <- emu.query( "demo", "*", "Phonetic=S|T" )
# get a data matrix of formant data at the end of the segment
data <- emu.track( segs, "fm", cut=1 )

# create segment lists segs.1 and segs.2 for each label type S and T
splitmat(segs, label(segs), "segs", labelfile=T)

# create two data matrices for each label type
splitmat(data, label(segs), "data", labelfile=T)
}
\keyword{misc}

