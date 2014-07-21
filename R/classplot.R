`classplot` <- function(model, xlim, ylim, N = 100,  
                        pch=15, col=NULL, legend=TRUE, 
                        position="topright", bg="gray90",  ...)
{
  if(any(class(model) %in% "svm"))
  {
    priorlabels <- model$levels
    if(ncol(model$SV)!=2)
      stop("data must be two-dimensional")
  }
  else if(any(class(model) %in% c("lda", "qda")))
  {
    priorlabels <- rownames(model$means)
    if(ncol(model$means)!=2)
      stop("data must be two-dimensional")
  }
  pnts <- cbind(sort(rep(1:N/N, N)), rep(1:N/N, N))
  pnts[, 1] <- pnts[, 1] * (xlim[2] - xlim[1]) + xlim[1]
  pnts[, 2] <- pnts[, 2] * (ylim[2] - ylim[1]) + ylim[1]
  if(any(class(model) %in% c("lda", "qda")))
    blabs <- as.character(predict(model, pnts)$class)
  else if(any(class(model) %in% "svm"))
    blabs <- as.character(predict(model, pnts))
  k <- 1
  if(is.null(col))
    colours <- mu.colour(priorlabels, TRUE, FALSE)$colour
  else
    colours <- col
  plot(pnts, xlim=xlim, ylim=ylim, ...)
  for (j in priorlabels) {
    temp <- muclass(blabs, j)
    points(pnts[temp, ],pch=pch, col = colours[k])
    k <- k + 1
  }
  if(legend)
    legend(position, legend=priorlabels, col=colours, fill=colours, bg=bg)
}

