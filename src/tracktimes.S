"tracktimes" <-
function(trackdata)
{
if(is.trackdata(trackdata))
# return the times at which the frames
# of trackdata occur as a numerical vector
times <- as.numeric(dimnames(trackdata$data)[[1]])
else if(is.vector(trackdata))
times <- as.numeric(names(trackdata))
else if(is.matrix(trackdata))
times <- as.numeric(dimnames(trackdata)[[1]])
else times <- NULL
times
}







