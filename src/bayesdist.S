#############################################################################
#                                                                           #
#   copyright            : (C) 2000 SHLRC, Macquarie University             #
#   email                : Steve.Cassidy@mq.edu.au			    #
#   url			 : http://www.shlrc.mq.edu.au/emu		    #
#									    #
#   This program is free software; you can redistribute it and/or modify    #
#   it under the terms of the GNU General Public License as published by    #
#   the Free Software Foundation; either version 2 of the License, or       #
#   (at your option) any later version.                                     #
#									    #
#############################################################################


"mahal" <- function(data, train)
{
  if (emu.options("deprecated.warnings"))
    cat("mahal is deprecated, use classify with metric=\"mahal\"\n")
  classify( data, train, metric="mahal" )
}

"bayes.lab" <- function(data, train)
{
  if (emu.options("deprecated.warnings"))
    cat("bayes.lab is deprecated, use classify with metric=\"bayes\"\n")
  classify( data, train, metric="bayes" )
}

"bayes.dist"<-
  function(data, train, labels=NULL)
{
  if (emu.options("deprecated.warnings"))
    cat("bayes.dist is deprecated, use distance with metric=\"bayes\"\n")
  distance( data, train, labels, metric="bayes")
}

"mahal.dist" <-
  function( data, train, labels=NULL )
{
  if (emu.options("deprecated.warnings"))
    cat("mahal.dist is deprecated, use distance with metric=\"mahal\"\n")
  distance( data, train, labels, metric="mahal")
}


## generalise bayes.dist and mahal.dist
"distance" <-
  function( data, train, labels=NULL, metric="bayes" )
{
  ## data is a set of data points
  ## train is the result of the train fn and contains
  ##  $mean - the centroids
  ##  $cov  - the covariance matrices
  ##  $invcov - the inverse covariance matrix
  ##  $label - the corresponding labels
  ## labels - an optional set of labels corresponding to data
  ## metric - one of "bayes" or "mahal" for bayesian or mahalanobis distance
  
  if(!is.matrix(data)) data <- cbind(data)

  ncols <- length(train$label)
  ndims <- ncol(data)
  probs <- matrix(0, nrow = nrow(data), ncol = ncols)

  for(lab in 1:ncols) {
    tmp <- (lab - 1) * ndims + 1
    tmp1 <- tmp + ndims - 1
    cov <- train$cov[tmp:tmp1,  ]
    invcov <- train$invcov[tmp:tmp1,  ]
    if ( metric == "bayes" ) {
      probs[, lab] <- bayesian.metric(data, train$means[lab,  ], cov, invcov)
    } else if ( metric == "euclidean" ) {
      probs[, lab] <- euclidean.metric(data, train$means[lab,  ])
    } else if ( metric == "mahal" ) {
      probs[, lab] <- mahalanobis.metric(data, train$means[lab,  ], invcov)
    }            
  }
  dimnames(probs) <- list(labels, train$label)
  probs
}

"euclidean.metric" <- function( data, mean )
{
  return 
}


"bayesian.metric" <- 
  function( data, mean, cov, invcov )
{
  # calcuate the gaussian classification metric for multivariate data
  # given mean vector and covariance matrix
  det <- -log(as.numeric(prod(eigen(cov)$value)))
  x.u <- t(data) - mean
  pow <- t(x.u) %*% invcov
  ## this is really pow %*% x.u if x.u were just one point but it isn't
  ## it's many points so we can't just matrix multiply
  return( det - apply(pow * t(x.u), 1, sum) )
}

"mahalanobis.metric" <-
  function(data, mean, invcov)
{
  x.u <- t(data) - mean
  pow <- t(x.u) %*% invcov
  ## this is really pow %*% x.u if x.u were just one point but it isn't
  ## it's many points so we can't just matrix multiply
  return( apply(pow * t(x.u), 1, sum) )
}


"classify" <- function(data, train, metric="bayes")
{
  probs <- distance(data, train, metric=metric )

  ## what's best depends on the metric, bayes is a prob. so max is best
  ## mahal is a distance so min is best
  if( metric=="bayes" ) {
    best <- apply(probs, 1, max)
  } else if ( metric=="mahal" ) {
    best <- apply(probs, 1, min)
  }
  
  result <- rep("", length(best))
  for(lab in 1:length(train$label)) {
    tmp <- probs[, lab] == best
    result[tmp] <- train$label[lab]
  }
  result
}


bayesplot <- function(data, train, N = 10, ellipse = F, 
		      labs = NULL, xlab="", ylab="", colour=T, ...)
{
  ## data is the original data, used for scaling
  ## train is the stuff you get from train()
  rx <- range(data[, 1])
  ry <- range(data[, 2])  
  ## make a set of points covering the plane 0,1
  points <- cbind(sort(rep(1:N/N, N)), rep(1:N/N, N))
  ## Now scale them to the data
  points[, 1] <- points[, 1] * (rx[2] - rx[1]) + rx[1]
  points[, 2] <- points[, 2] * (ry[2] - ry[1]) + ry[1]
  ## now classify each point
  blabs <- classify(points, train, metric="bayes")
  plot(points, type = "n", xlim = rx, ylim = ry, xlab=xlab, ylab=ylab)
  ulabs <- unique(blabs)
  k <- 1
  colours <- mu.colour( ulabs, colour, F )$colour
  for(j in ulabs) {
    temp <- muclass(blabs, j)
    text(points[temp,  ], blabs[temp], col = colours[k])
    k <- k + 1
  }
  if(ellipse && !is.null(labs) ) {
    par(new = T)
    eplot(data, labs, xlim = rx, ylim = ry, colour=colour, ...)
  }
}

"train"<- function(x, lab=rep("x",nrow(x)))
{
  mat <- NULL
  if(is.matrix(x)){
    summeanvals <- NULL
    sumcovvals <- NULL
    sumcovvals.inv <- NULL
    for(j in unique(lab)) {
      temp <- lab == j
      # can only do this if there's more than one row
      if( sum( temp ) == 1 ) {
        ## what to do??
        ## can't do anything sensible so barf and tell them why
        stop( "\n\tData passed to train has only one entry for one of the labels.\n\tA gaussian model can't be generated for this data." )
      }
      values <- x[temp,]
      meanvals <- apply(values, 2, mean)
      covvals <- var(values, values)
      covvals.inv <- solve(covvals)
      summeanvals <- rbind(summeanvals, meanvals)
      sumcovvals <- rbind(sumcovvals, covvals)
      sumcovvals.inv <- rbind(sumcovvals.inv, covvals.inv)
    }
    mat$label <- unique(lab)
    mat$means <- summeanvals
    mat$cov   <- sumcovvals
    mat$invcov <- sumcovvals.inv
  }  else { # the one dimensional case
    mat <- NULL
    mat$means <- NULL
    mat$cov <- NULL
    for(j in unique(lab)) {
      cat("data for ", j, " \n" )
      temp <- lab == j			
      mat$means <- c(mat$means, mean(x[temp]))
      mat$cov <- c(mat$cov, sqrt(var(x[temp])))
    }
    mat$label <- unique(lab)
    mat$invcov <- 1/mat$cov   # in fact this won't be used in the 1d case
  }
  mat
}


# Local Variables:
# mode:S
# S-temp-buffer-p:t
# End:
