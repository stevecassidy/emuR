"dct" <-
function(wav, fit=F,  k=NULL)
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
N <- length(wav)
if(is.null(k))
k <- N
if(k < 2 | k > N)
stop("k must be between 2 and the length of the input signal")

# program begins here
wav <- c(wav, rev(wav[-c(1, N)]))
Nref <- length(wav)
coeff <- Re(fft(wav, inverse=T))/Nref

if(fit)
{
p <- 1:k
r <- c(Nref: (Nref-k+2))
p <- c(p, r)
coeff[-p] <- 0
coeff <- Re(fft(coeff, inverse=T))
}
coeff <-coeff[1:N]
names(coeff) <- nz
if(!fit)
coeff <- coeff[1:k]
coeff
}

