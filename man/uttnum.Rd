\" -*-nroff-*-
\name{uttnum}
\alias{uttnum}
\title{
Find an utterance in Utterances
}
\usage{
uttnum(utt, str)
}
\arguments{
\item{utt}{
A mu+ utterance vector.
}
\item{str}{
The full name of the utterance. 
}}
\value{
An integer, corresponding to the index of the utterance in the
Utterances vector.  
}
\seealso{
mkdb
}
\examples{
 uttnum(my.utts, "/mu/mu/MUDEMO/msajc001")
}
\keyword{mu}
% Converted by Sd2Rd version 0.3-3.


