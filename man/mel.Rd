\name{mel}
\alias{mel}
\title{
Convert Hz to the mel scale
}
\usage{
mel(x)
}
\arguments{
\item{x}{
The frequency in Hz.
}}
\value{
The corresponding frequency on the mel scale. 
}
\details{
The mel scale frequency is calculated according to the formula: 




mel =	1/log(2) * (log(1 + (Hz/1000))) * 1000


where Hz is the frequency in Hz. 
}
\references{


}
\seealso{
bark
}
\keyword{misc}
% Converted by Sd2Rd version 0.3-3.
