\" -*-nroff-*-
\name{museg.write}
\alias{museg.write}
\title{
Write a MU+ segment list to a file.
}
\usage{
museg.write(SegList, File)
}
\arguments{
\item{SegList}{
A MU+ segment list, as returned by \code{phon}.
}
\item{File}{
The name of a file to write the segment list into. 
}}
\value{
None.
}
\section{Side Effects}{
The segment list is written to a file in a format suitable for input to
the \code{muspec} program. 
}
\seealso{
phon, muspec
}
\keyword{mu}
% Converted by Sd2Rd version 0.3-3.
