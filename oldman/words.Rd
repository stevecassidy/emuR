\" -*-nroff-*-
\name{words}
\alias{words}
\title{
Returns the words in an utterance
}
\usage{
words(vec, utt)
}
\arguments{
\item{vec}{
Either an utterance name or an integer which is used as an index into
the utterance vector \code{utt}. 
If \code{vec} is a number, this should be a mu+ utterance vector.  
}}
\value{
The words (text) from the utterance.
}
\seealso{
phon, label
}
\examples{
# find the words in the first utterance of my.utts
words(1, my.utts)
# find the words in this utterance
words("/mu/mu/MUDEMO/msadb001")
}
\keyword{mu}
% Converted by Sd2Rd version 0.3-3.
