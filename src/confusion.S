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


"confusion"<- function(labx, laby, perc = F)
{
	mat <- NULL
	px <- sort(unique(labx))
	py <- sort(unique(laby))
	numx <- match(labx, px)
	numy <- match(laby, py)
	mat <- matrix(0, length(px), length(py))

	for(j in 1:length(labx)) {
	  mat[numx[j], numy[j]] <- mat[numx[j], numy[j]] + 1
	}

	dimnames(mat) <- list(px, py)
	if(perc)
		mat <- percent(mat)
	mat
}

"percent"<- function(data)
{
  total <- percent.sub(data)
  mat <- NULL
  for(j in 1:nrow(data)) {
    temp1 <- round(data[j,  ]/total[j] * 100, dig = 1)
    mat <- rbind(mat, temp1)
  }
  dimnames(mat) <- list(dimnames(data)[[1]], dimnames(data)[[2]])
  mat
}

"percent.sub"<- function(conf)
{
  ## adds up the elements of the rows of a confusion matrix
  mat <- NULL
  for(j in 1:nrow(conf)) {
    mat <- c(mat, sum(conf[j,  ]))
  }
  mat
}

"perform" <- function(data)
{
  ## calculates total score in a confusion matrix, data
  k <- 0
  for(j in 1:nrow(data)) {
    k <- k + data[j, j]
  }
  total <- sum(data)
  wrong <- total - k
  correct <- (k/total) * 100
  wrong <- wrong/total * 100
  labcol <- c("correct %", "incorrect %")
  m <- cbind(correct, wrong)
  dimnames(m) <- list(NULL, labcol)
  m
}

# Local Variables:
# mode:S
# S-temp-buffer-p:t
# End:

