\name{splitmat}
\alias{splitmat}
\title{
Split data into objects of phonetic classes
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
\code{file} is the object name you wish to save the phonetic classes under, and
is called "file.n", where n is an integer from 1 to n numnber of phonetic
classes.
}
\item{labelfile}{
If TRUE, a parallel label file (object) is created called "l.file.n".
}}
\value{
Matrix of mu+ object files and the phonetic class that it belongs to.
}
\seealso{
label
}
\examples{
# Create unique objects and labels for [S] and [T] from the demo.utts database.
> segs _ phon(demo.utts, "Phonetic='S/T'", find="Phonetic")
> labs _ label(segs)
> splitmat(segs, labs, filename="segs", labelfile=T)
     filename label-file label-type
[1,] "segs.1" "l.segs.1" "S"
[2,] "segs.2" "l.segs.2" "T"
}
\keyword{mu}
% Converted by Sd2Rd version 0.3-3.
