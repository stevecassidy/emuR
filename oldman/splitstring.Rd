\" -*-nroff-*-


\name{splitstring}
\alias{splitstring}
\title{
Split a string into words.
}
\usage{
splitstring(str,char)
}
\arguments{
\item{str}{
A string.
}
\item{char}{
A character to split on
}}
\value{
A vector of strings.  The original \code{str} is split at ever occurrence of
\code{char} to generate a vector of strings. 
}
\examples{
> splitstring("/home/recog/steve/foo", "/")
[1] "home" "recog" "steve" "foo"
}
\keyword{mu}
% Converted by Sd2Rd version 0.3-3.
