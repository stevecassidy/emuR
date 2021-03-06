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

"Slope.test" <- function(...)
{
  ## compiled by Jonathan Harrington and Marija Tabain (October 1997)
  ## this function tests whether the intercepts and slopes
  ## of two or more (straight-line) regressions are significantly
  ## different
  ## matrices are to be compared on slope and intercept;
  ## arrange y in col 1, x in col 2 in each case.
  ## see E. Pedhazur, Multiple Regression in Behavioral Research
  ## p.436-450, 496-507. 
  Slope.sub <- function(...)
    {
      ## combine the matrices, and find out how many rows there are altogether
      omat <- NULL
      omat$numcats <- length(list(...))
      for(j in list(...)) {
	numrows <- nrow(j)
	omat$y <- c(omat$y, j[, 1])
	omat$x <- c(omat$x, j[, 2])
	omat$numrows <- c(omat$numrows, numrows)
      }
      ## set up category vectors of 1, 0, 0, .... -1
      vec <- rep(0, omat$numcats - 1)
      omat$mat <- NULL
      for(j in 1:length(vec)) {
	zeros <- vec
	zeros[j] <- 1
	zeros <- c(zeros, -1)
	zeros <- rep(zeros, omat$numrows)
	omat$mat <- cbind(omat$mat, zeros)
      }
      omat
    }

  ## main function begins here
  omat <- Slope.sub(...)	
  ## number of category vectors and the (1) continuous vector for intercept
  k1 <- omat$numcats	# the (1) continuous vector for intercept
  k2 <- 1	        # number of category vectors, product 
                        # vectors and (1)continuous vector 

  ## for slope
  k3 <- 1 + ((omat$numcats - 1) * 2)	
  ## number of category vectors and (1) continuous vector for slope
  k4 <- omat$numcats	## length of y and of x
  N <- sum(omat$numrows)

  for(j in list(...)) {
    ## find the F-ratio, degrees of freedom, r-squared values, slope and intercept
    ## for the separate matrices
    firstvals <- summary.lm(lm(j[, 1] ~ j[, 2]))
    first.pf <- pf(firstvals$fstatistic[1], firstvals$fstatistic[2],
		   firstvals$fstatistic[3])
    first.out <- c(firstvals$r.squared, firstvals$fstatistic, 
		   first.pf, firstvals$coefficients[, 1])
    omat$separate <- rbind(omat$separate, first.out)
  }

  dimnames(omat$separate)[[2]] <- c("r-sq", "F ratio", "df", "df", 
				    "prob. line fits data", "intercept", "slope")	

  ## multiply the category vectors by the x-values 
  prodvals <- omat$x * omat$mat
  z123 <- lm(omat$y ~ omat$x + omat$mat + prodvals)
  z12 <- lm(omat$y ~ omat$x + omat$mat)
  z2 <- lm(omat$y ~ omat$x)	## r-squared vals
  r12 <- summary.lm(z12)$r.squared
  r2 <- summary.lm(z2)$r.squared	## F-ratios
  fval.in.num <- (r12 - r2)/(k1 - k2)
  fval.in.den <- (1 - r12)/(N - k1 - 1)
  fratio.in <- fval.in.num/fval.in.den
  s123 <- summary.aov(z123)
  fratio.slope <- s123$"F Value"[3]	

  ## calculate probabilities and degrees of freedom
  prob.in <- pf(fratio.in, k1 - k2, N - k1 - 1)
  prob.slope <- pf(fratio.slope, k3 - k4, N - k3 - 1)
  first <- c(fratio.in, prob.in, k1 - k2, N - k1 - 1)
  second <- c(fratio.slope, prob.slope, k3 - k4, N - k3 - 1)
  outtemp <- rbind(first, second)
  col.lab <- c("intercept", "slope")
  row.lab <- c("F ratio", "Probability of them being DIFFERENT", "df", 
	       "df")
  dimnames(outtemp) <- list(col.lab, row.lab)
  omat$combined <- outtemp
  omat
}
