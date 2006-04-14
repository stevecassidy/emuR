\name{vowlax.right}
\alias{vowlax.right}

\docType{data}
\title{ Vector of labels after the vowels in vowlax}
\description{
  For vowlax, see data(vowlax) 
}
\usage{data(vowlax)}

\details{
  vowlax = emu.query("kielread06", "*", "Kanonic=a | E | I | O")
  vowlax.right = emu.requery(vowlax, "Kanonic", "Kanonic", sequence=1, justlabels=T)

}
\seealso{
  \code{\link{vowlax}}
}
\keyword{datasets}
\keyword{emu}
