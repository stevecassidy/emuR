\name{eplot}
\alias{eplot}
\title{
Ellipse plot
}
\usage{
function(data, labs, chars, formant = F, scaling = "linear", 
	 nsdev = 2.447747, dopoints = F, classify = F, 
	 doellipse=F, centroid = F, main="", xlab="", ylab="", axes = T,
	 xlim, ylim, colour, linetype, \dots)
}
\arguments{
\item{data}{
A two-columned matrix of data
}
\item{labs}{
A vector of labels, parallel to \code{data}.
}
\item{chars}{
A vector of labels, parallel to \code{data}. If this argument is specified
these labels will be plotted rather than the labels in \code{labs}.
}
\item{formant}{
If TRUE, then the data is negated so that, for formant data, the plot
is made in the -F2/-F1 plane.  This gives a similar layout to the
common vowel quadrilateral.
}
\item{scaling}{
Either "mel" or "bark" for mel or bark scaling of the data. 
}
\item{nsdev}{
Defines the length of the major and minor axes of the ellipses in terms
of the standard deviation of the data. 
}
\item{dopoints}{
If TRUE, character labels (from \code{labs} or \code{chars}) are plotted for each
data point.
}
\item{doellipse}{
If T, ellipses are drawn on the plot. If F, no ellipses are drawn and, if
'dopoints' is also F, 'centroids' is set to T. 
}
\item{centroid}{
If T, a single character label is plotted at the center of each ellipse.
}
\item{classify}{
If classify is TRUE, a gaussian (mahalanobis distance) classification
is carried out on the data and the confusion matrix is returned.
}
\item{main}{
A main title for the plot.
}
\item{xlab,}{
Titles for the x and y axes.
}
\item{axes}{
If TRUE axes are drawn on the plot.
}
\item{xlim,}{
The bounds of the plot on the x and y axes, each should be a two
element vector giving the minimum and maximum values to plot. 
}
\item{colour}{
If colour is TRUE, the ellipses and labels will be plotted in different
colours. 
}
\item{linetype}{
If linetype is TRUE, the ellipses will be plotted with different
linetypes.  This is useful for plots that will be printed. 


This function will also accept any of the standard graphical parameters
which can be passed to \code{par()}, for example \code{cex} which specifies the
size of characters to be used.  


}}
\value{
A confusion matrix, if the classify argument is TRUE, otherwise NULL. 
}
\section{Side Effects}{
A plot is generated consisting of an ellipse defining the distribution
of each class within the given labels.  The mean and covariance of each
class is estimated from the data and plotted. Labels may be plotted for
each data point or at the class mean for each class.  
}
\seealso{
train(), mahal()
}
\examples{
eplot(formantdata, vowellabs, centroids=T)
}
\keyword{mu}
% Converted by Sd2Rd version 0.3-3.
