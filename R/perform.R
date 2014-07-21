


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

