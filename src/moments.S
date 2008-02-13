"moments" <-
function(count, x, minval=FALSE)

{
# compute moments. x is a numeric class
# count is the frequency with which that 
# particular class occurs
# This function gives exactly the same
# results as those for the mean, variance
# skewness and kurtosis in example Table 3.13.1
# p. 87, Snedecor & Cochran, 'Statistical Methods'
# 6th Edition, 1975. Let the arguments count and x
# equal f and U respectively in their example
# the centre of gravity with minval = F.
# the first two moments in this function
# also give the same results as in Harrington & Cassidy.
if(minval)
count <- count - min(count)
if(missing(x))
{
if(is.spectral(count))
x <- trackfreq(count)
else
x <- 0:(length(count)-1)
}
k <- 1
mom1 <- sum((x - 0)^k * count) / sum(count)
# the variance
k <- 2
mom2 <- sum((x - mom1)^k * count) / sum(count)

# third moment
k <- 3
mom3 <- (sum((x - mom1)^k * count) / sum(count)) / (mom2 * sqrt(mom2))

# fourth moment
k <- 4
# peaked distributions show positive kurtosis
# flat-topped distributions show negative kurtosis
mom4 <- (sum((x - mom1)^k * count) / sum(count)) / mom2^2 - 3
c(mom1, mom2, mom3, mom4)
}

