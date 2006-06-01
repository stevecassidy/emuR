"plot.trackdata" <-
function (trackdata, timestart=NULL, xlim=NULL, ylim=NULL, labels=NULL, contig=T, ...)
 {

N <- nrow(trackdata$data)
n <- nrow(trackdata)
# can't plot labels if xlim is specified
if(!is.null(xlim))
labels <- NULL

if(!is.null(labels))
{
if( length(labels) != nrow(trackdata) )
stop("if labels are supplied, there must be one label per segment")
label.times <- apply(trackdata$ftime, 1, mean)
boundary.times <- c(trackdata$ftime[,1], trackdata$ftime[n])
}


if(n > 1 & contig)
{
inds <- cbind(1, N)
ftime <- cbind(trackdata$ftime[1,1], trackdata$ftime[n,2])
trackdata <- as.trackdata(trackdata$data, inds, ftime)
}

if(!is.null(xlim))
{
if(nrow(trackdata)!=1)
stop("can't specify xlim if there's more than one segment")
}

left <- trackdata$ftime[1]
right <- trackdata$ftime[2]
times <- seq(left, right, length=nrow(trackdata$data))
if(!is.null(timestart))
{
times <- times - left + timestart
if(!is.null(labels))
{
label.times <- label.times - left + timestart
boundary.times <- boundary.times - left + timestart
}
}
data <- trackdata$data
if(nrow(trackdata)==1)
{
for(k in 1:ncol(data)){
if(is.null(xlim))
xlim <- range(times)
if(is.null(ylim))
ylim <- range(data)
plot(times, data[,k], xlim=xlim, ylim=ylim,...)
par(new=T)
}
par(new=F)
if(!is.null(labels))
{
if(length(boundary.times) > 2)
abline(v=boundary.times)
mtext(labels, at=label.times)
}
}
else
{
if(is.null(labels))
labels <- rep("", nrow(trackdata))

for(j in 1:nrow(trackdata)){
plot(trackdata[j,], timestart=timestart, xlim=xlim, ylim=ylim, labels=labels[j], contig=T, ...)
}
}
 }

