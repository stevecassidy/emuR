\" -*-nroff-*-
\name{mkdb}
\alias{mkdb}
\title{
Generate a utterance list
}
\usage{
mkdb(directory=options()$dbdefault, extn=options()$labextn, pattern="*")
}
\arguments{
\item{directory}{
The directory which contains the label files which will form the
database. Note that you should not include a trailing \code{/} on the path
name. 
}
\item{extn}{
The file extension for label files.
}
\item{pattern}{
A pattern to match the desired utterances (use the same format as you
would on the unix command line, i.e. \code{?} matches any one character, \code{*}
matches any number of characters).
}}
\value{
A mu+ utterance vector. 
}
\details{
This function is used to generate an utterance vector from a given
directory. The utterance vector specifies the database to be searched
by \code{phon} and other functions.  Each element of the vector is the
name of a label file with the file extension removed.  This then refers
to a family of files containing the information about the utterance
(label file, sample data file etc.).  
}
\seealso{
phon
}
\examples{
# Create an utterance list containing all utterances in directory `"dbase"':
utts _ mkdb("dbase")
# restrict the utterance list to those files begining with msa:
msa.utts _ mkdb("dbase", pattern="msa*")
}
\keyword{mu}
% Converted by Sd2Rd version 0.3-3.
