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

if(buildtrack)
{
   if (nrow(cbind(result)) != nrow(trackdata$data)) 
	   warning("Can not turn the result into a trackdata object. Data length differs as a result of FUN.")
   else {
	   trackdata$data <- cbind(result)
	   result <- trackdata
	   return(result)
   }
}


if(simplify)
{
   if (ncol(result) == 1) {
      result <- c(result)
      return(result)
   }
}


return(result)

}

