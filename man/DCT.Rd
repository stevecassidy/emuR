\name{dct}
\alias{dct}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Discrete Cosine Transformation }
\description{
  Calculates the coefficients of the discrete cosine transformation
}
\usage{
dct(wav, fit = F, k = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{wav}{ wav must be a vector or a one-columned matrix }
  \item{fit}{ ~~Describe \code{fit} here~~ }
  \item{k}{ number of returned coefficients }
}
\details{
  ~~ If necessary, more details than the __description__  above ~~
}
\value{
  ~Describe the value returned
  If it is a LIST, use
  \item{comp1 }{Description of 'comp1'}
  \item{comp2 }{Description of 'comp2'}
  ...
}
\references{ ~put references to the literature/web site here ~ }
\author{ Jonathan Harrington }
\note{ ~~further notes~~ }

 ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{ ~~objects to See Also as \code{\link{~~fun~~}}, ~~~ }
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function(wav, fit=F,  k=NULL)
{

if(!is.vector(wav) & !is.matrix(wav) )
stop("input signal must be a vector or a one-columned matrix")
if(is.matrix(wav) )
{
if(ncol(wav)!=1)
stop("input signal must be a vector or a one-columned matrix")
  }
if(is.vector(wav))
nz <- names(wav)
if(is.matrix(wav))
nz <- dimnames(wav)[[1]]
N <- length(wav)
if(is.null(k))
k <- N
if(k < 2 | k > N)
stop("k must be between 2 and the length of the input signal")

# program begins here
wav <- c(wav, rev(wav[-c(1, N)]))
Nref <- length(wav)
coeff <- Re(fft(wav, inverse=T))/Nref

if(fit)
{
p <- 1:k
r <- c(Nref: (Nref-k+2))
p <- c(p, r)
coeff[-p] <- 0
coeff <- Re(fft(coeff, inverse=T))
  }
coeff <-coeff[1:N]
names(coeff) <- nz
if(!fit)
coeff <- coeff[1:k]
coeff
  }
}
\keyword{ ~kwd1 }% at least one, from doc/KEYWORDS
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
