\name{ellipse}
\alias{ellipse}
\title{
Calculate ellipse coordinates
}
\usage{
ellipse(x, y, rx, ry, orient, incr=360/100)
}
\arguments{
\item{x}{
X coordinate of center
}
\item{y}{
y coordinate of center
}
\item{rx}{
Radius in the x direction
}
\item{ry}{
Radius in the y direction
}
\item{orient}{
Orientation, in radians. The angle of the major axis to the x axis.
}
\item{incr}{
The increment between points, in degrees. 
}}
\value{
A matrix of x and y coordinates for the ellipse. 
}
\seealso{
eplot()
}
\keyword{mu}
% Converted by Sd2Rd version 0.3-3.
