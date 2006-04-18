"plafit" <-
function(wav,fit=F, n=101)
{
if(!is.vector(wav) & !is.matrix(wav) )
stop("input signal must be a vector or a one-columned matrix")
if(is.matrix(wav) )
{
if(ncol(wav)!=1)
stop("input signal must be a vector or a one-columned matrix")
}
if(is.vector(wav))
nz <- names(wav)
if(is.matrix(wav))
nz <- dimnames(wav)[[1]]
if(n %% 2 != 1)
n <- n + 1
N <- length(wav)
a <- approx(wav, n=n)$y
times <- seq(-1, 1, length=n)
c0 <- a[times==0]
c1 <- 0.5 * (a[n] - a[1])
c2 <- 0.5 * (a[n] + a[1]) - c0
if(fit)
{
y <- c0 + c1 * times + c2 * (times^2)
result <- approx(y, n=N)$y
names(result) <- nz
}
else
{
result <- c(c0, c1, c2)
names(result) <- c("c0", "c1", "c2")
}
result
}

