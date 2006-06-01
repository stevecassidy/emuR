"epggs" <-
function(epgdata, gscale=100, gridlines=T, gridcol="gray", gridlty=1, axes=T, xlab="", ylab="", ...)
{
# function to plot a 3D greyscale EPG imageb
# p is palate data, returned by palate() or EPG-trackdata
# plots greyscale image of contacts
# such that
# the darker the square, the greater the
# proportion of contacts. Thus a black square
# means that a contact was always on
# for all palatograms in p; a white
# square means that it was always off.

if(!inherits(epgdata, "EPG")) p <- palate(epgdata)
else p <- epgdata
# in case there is only one palate
if(length(dim(p) )==2)
{
p <- array(p, c(8, 8, 1))
class(p) <- "EPG"
}
n = dim(p)[3]
sump  = (apply(p, c(1,2), sum))/n 
image(1:8, 1:8, t(1-sump[8:1,]), col = gray(0:gscale/gscale), axes=F, xlab=xlab, ylab=ylab, ...)
if(axes)
{
axis(side=1)
axis(side=2, at=c(1, 3, 5, 7), lab=as.character(c(8, 6, 4, 2)))
}
if(gridlines)
grid(8, 8, col = gridcol, lty=gridlty)
}

