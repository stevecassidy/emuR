"rbind.trackdata" <- function (...) 
{
  mat <- NULL
  for (j in list(...)) {
    if (is.matrix(j$data)) 
      mat$data <- rbind(mat$data, j$data)
    else mat$data <- c(mat$data, j$data)
    mat$index <- rbind(mat$index, j$index)
    if (!is.null(j$ftime)) 
      mat$ftime <- rbind(mat$ftime, j$ftime)
  }
  diffinds <- mat$index[, 2] - mat$index[, 1] + 1
  right <- cumsum(diffinds)
  first.left <- diffinds - 1
  left <- right - first.left
  mat$index <- cbind(left, right)
  if (version$major >= 5) {
    oldClass(mat) <- "trackdata"
  }
  else {
    class(mat) <- "trackdata"
  }
  mat
}

