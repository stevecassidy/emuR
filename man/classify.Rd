\name{classify}
\alias{classify}
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
classify(data, train, metric = "bayes")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{ ~~Describe \code{data} here~~ }
  \item{train}{ ~~Describe \code{train} here~~ }
  \item{metric}{ ~~Describe \code{metric} here~~ }
}
\details{
}
\value{
  \item{comp1 }{Description of 'comp1'}
  \item{comp2 }{Description of 'comp2'}
  ...
}
\references{ ~put references to the literature/web site here ~ }
\author{ Jonathan Harrington }
\note{ ~~further notes~~ }


\seealso{  }
\examples{

## The function is currently defined as
function (data, train, metric = "bayes") 
{
    probs <- distance(data, train, metric = metric)
    if (metric == "bayes") {
        best <- apply(probs, 1, max)
    }
    else if (metric == "mahal") {
        best <- apply(probs, 1, min)
    }
    result <- rep("", length(best))
    for (lab in 1:length(train$label)) {
        tmp <- probs[, lab] == best
        result[tmp] <- train$label[lab]
    }
    result
  }
}
\keyword{emu}
