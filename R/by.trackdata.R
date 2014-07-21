`by.trackdata` <- function (data, INDICES = NULL, FUN, ..., simplify = FALSE) 
{
  #there might be a problem with data$data therefore data is replaced by abitrary datam
  # data=m.fdat.int[,1]; FUN=targtime; simplify=TRUE;INDICES = NULL
  
  datam = data
  
  orgindices = INDICES
  fun = FUN
  arr.indices = function(orgindices, datam) {
    indices = NULL
    if (is.null(orgindices) || is.vector(orgindices)) {
      n = 1:nrow(datam$index)
      if (is.null(orgindices)) {
        indices = rep(n, datam$index[, 2] - datam$index[, 
                                                        1] + 1)
      }
      if (length(orgindices) == dim(datam$ftime)[1]) {
        indices = NULL
        for (ind in 1:dim(datam$ftime)[1]) {
          indices = c(indices, rep(orgindices[ind], datam$index[ind, 
                                                                2] - datam$index[ind, 1] + 1))
        }
      }
      if (length(orgindices) == dim(datam$data)[1]) {
        indices = orgindices
      }
      return(indices)
    } else {
      warning("Can not arrange INDICES!")
      return(orgindices)
    }
  }
  indices = NULL
  if (is.list(orgindices)) {
    for (var in 1:length(names(orgindices))) {
      orgindices[[var]] = arr.indices(orgindices[[var]],datam)
    }
    indices = orgindices
  } else {
    indices = arr.indices(orgindices,datam)
  }
  result <- o <- by(I(datam$data), indices, fun, ...)
  if (simplify) {
    if (is.null(attributes(summary(o))$dim)) 
      result <- c(unlist(o))
    else {
      result <- NULL
      for (j in 1:length(o)) {
        result <- rbind(result, o[[j]])
      }
    }
  }
  result
}


