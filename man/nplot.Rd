\" -*-nroff-*-
\name{nplot}
\alias{nplot}
\title{
Plot normal data curve
}
\usage{
nplot(values, labs, xlab="", ylab="probability density", 
      main="", colour=T, linetype=F, plot.lab=T, 
      xlim, logprob=F)
}
\arguments{
\item{values}{
A vector of numeric values. 
}
\item{labs}{
A vector of label, parallel to \code{values}. 
}
\item{xlab}{
An y-axis label
}
\item{ylab}{
A y-axis label
}
\item{main}{
A main title for the plot. 
}
\item{colour}{
If colour is TRUE, the curves and labels will be plotted in different
colours. 
}
\item{linetype}{
If linetype is TRUE, the curves will be plotted with different
linetypes.  This is useful for plots that will be printed. 
}
\item{plot.lab}{
If plot.lab is TRUE, labels will be plotted above each normal curve.
}
\item{xlim}{
A vector of the minimum and maximum values to plot on the x-axis. 
}
\item{logprob}{
If TRUE, the log-normal curve will be plotted. 
}}
\section{Side Effects}{
Generates a plot on the current device which shows the distribution of
the data in \code{values} for each unique label in \code{labs}. 
}
\details{
The data is split according to the unique labels in \code{labs} and the mean
and variance of each data set is calculated. The probability density
for each data set is then plotted.  
}
\seealso{
hplot, dnorm
}
\keyword{misc}
\keyword{misc}
% Converted by Sd2Rd version 0.3-3.
