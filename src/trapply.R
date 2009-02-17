`trapply` <-
  function (trackdata, fun, ..., simplify = FALSE, returntrack = FALSE) 
{
  if(returntrack)
    simplify <- FALSE
                                        # if simplify is F or if returntrack is T, store as a list
  if (!simplify) 
    result <- list(NULL)
  else result <- NULL
  for (j in 1:nrow(trackdata)) {
    if (!simplify) 
      result[[j]] <- fun(trackdata[j, ]$data, ...)
    else 
      result <- rbind(result, fun(trackdata[j, ]$data, 
                                  ...))        
  }
  if (simplify) {
    if (ncol(result) == 1) 
      result <- c(result)
  }
  if (returntrack)
    result <- buildtrack(result)
  result
}

