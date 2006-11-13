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

