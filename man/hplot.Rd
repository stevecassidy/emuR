\" -*-nroff-*-
\name{hplot}
\alias{hplot}
\title{
Plot superimposed histograms
}
\usage{
hplot(vec, labs, xlab = "", ylab = "", main = "", 
	 colour = T, xlim = NULL, axes = T, legn="tl")
}
\arguments{
\item{vec}{
A vector of numerical data.
}
\item{labs}{
A vector of labels parallel to \code{vec}.
}
\item{xlab}{
An x-axis label. 
}
\item{ylab}{
A y-axis label.
}
\item{main}{
A main title for the plot.
}
\item{colour}{
If colour is TRUE, different colours will be used for each unique
label. If it is FALSE, different shading patterns will be used. 
}
\item{xlim}{
The range of the x-axis.
}
\item{axes}{
If FALSE, no axes will be plotted. 
}
\item{legn}{
Where to put the legend. If \code{FALSE} no legend will be drawn, otherwise
this can be one of \code{"tl"}, \code{"br"}, \code{"bl"} or \code{"tr"} (for top-left, bottom-right
etc.) or \code{"loc"} (for locator) which requres you to position the legend
with the mouse. 
}}
\value{
None.
}
\section{Side Effects}{
Generates  multiple  overlaid histograms, one for each unique label in
\code{labs}. 
}
\seealso{
nplot, eplot
}
\keyword{mu}
% Converted by Sd2Rd version 0.3-3.
