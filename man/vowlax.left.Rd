\name{vowlax.left}
\alias{vowlax.left}

\docType{data}
\title{ Vector of labels in front of the vowels in vowlax}
\description{
  For vowlax, see data(vowlax) 
}
\usage{data(vowlax)}

\details{
  vowlax = emu.query("kielread06", "*", "Kanonic=a | E | I | O")
  vowlax.left = emu.requery(vowlax, "Kanonic", "Kanonic", sequence=-1, justlabels=T)

}
\seealso{
  \code{\link{vowlax}}
}
\keyword{datasets}
\keyword{emu}
