\name{DCT}
\alias{DCT}
\title{Calculate the Discrete Cosine Transform. }
\description{
  Calculate the Discrete Cosine Transform on time series data.
}
\usage{
DCT(data, inv = T)
}
\arguments{
  \item{data}{ A vector of numeric data }
  \item{inv}{ If TRUE, compute the inverse transform, default TRUE  }
}
\details{
  
   Forward DCT calculates the discrete cosine transform of the time varying
   signal b_n
   \eqn{
                 N-1
 G(k) = 2*e(n)/N*SUM b_n*cos[pi*(2k+1)*n/2N] k=0..N-1(3)
                 n=0
   }

   Inverse DCT reconstructs a time domain waveform from the cosine
   waveform G(k), a smoothed version of the orginal b_n can be 
   obtained if less than  N-1 cosines are used.

   \eqn{   
       N-1
 b_n = SUM e(n)*G(k)*cos[pi*(2k+1)*n/2N] n=0..N-1(2)
       k=0
  }
  
   where
   \eqn{
 e(n) = 1/sqrt(2) if n=0
      = 1, otherwize
 }
}
\value{
  A vector with the same number of elements as the  original data. 
}
\references{ K. Rao and P. Yip, Discrete cosine transform : algorithms,
  advantages, applications, Academic Press, Boston, MA, 1990. }
\author{Catherine Watson }
\seealso{ \code{\link{applyDCT}} }

\examples{
# inverse transform should be the identity transform 
# expect result = 1:10 (approx)
DCT(DCT(1:10,F),T)

# Simple transform, expect
# [1]  7.778175e+00 -4.036036e+00 -9.898332e-16 -4.323021e-01 -4.884981e-16
# [6] -1.414214e-01 -4.824093e-15 -5.718538e-02  1.021405e-15 -1.603587e-02
DCT(1:10,F)

}
\keyword{misc}% at least one, from doc/KEYWORDS
