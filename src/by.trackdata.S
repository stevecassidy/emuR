"by.trackdata" <-
function(trackdata, fun,  ..., nolist=F)
{
n = 1:nrow(trackdata$index)
indices = rep(n, trackdata$index[,2]-trackdata$index[,1]+1)
result <- o <- by(trackdata$data, indices, fun, ...)


if(nolist)
{
if( ncol(trackdata$data)==1  )
result <- c(unlist(o))
else
{
result <- NULL
for(j in 1:length(o)){
result <- rbind(result, o[[j]])
}
}

}



result
}

