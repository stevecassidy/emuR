`trapply` <-
function(trackdata, fun, ..., simplify=FALSE, buildtrack=FALSE)
{

if(!simplify & !buildtrack)
result <- list(NULL)
else
result <- NULL


for(j in 1:nrow(trackdata)){
if(!simplify & !buildtrack)
result[[j]] <- fun(trackdata[j,]$data, ...)
else
{
if(!buildtrack)
result <- rbind(result, fun(trackdata[j,]$data, ...))
else
result <- c(result, fun(trackdata[j,]$data, ...))
}
}

if(simplify & !buildtrack)
{
if (ncol(result) == 1)
result <- c(result)
}

if(buildtrack)
{
trackdata$data <- cbind(result)
result <- trackdata
}

result

}

