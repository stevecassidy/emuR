# -*- S -*-

"prcomp" <- function( data ) 
{
  ## principal components analysis for R to mimic the S version
  ## returns list of:
  ##       sdev - the eigenvalues
  ##       rotation - the eigenvector matrix
  ##       x - the transformed data
  covar <- cov( data )
  eigen <- eigen( covar )

  list( sdev=eigen$values, rotation=eigen$vectors, x=NULL )
}
