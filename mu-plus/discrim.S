##########################################################################
#									 #
#	  This file is part of the MU+ Speech Database System		 #
#			  (c) Copyright 1994,				 #
#	     Speech Hearing and Language Research Centre,		 #
#	       Macquarie University, Sydney, Australia.			 #
#									 #
#	Copying and redistribution of this software is subject to the	 #
#	conditions of the MU+ licence.  Any modifications of this code 	 #
#	remain the property of Macquarie University.			 #
#									 #
##########################################################################

"discrim"<-
function(data, labs, dims = length(unique(labs)) - 1, 
	 transform = NULL, verbose = F)
{
  ## An interface to the cda program, canonical discriminant analysis
  ## finds the eigenvectors/eigenvalues for a dataset to maximise the 
  ## separation between groups
  ## data is a matrix of multidimensional data
  ## labs is the group labels for data
  ## dims is the number of dimensions you want returned
  ## transform is the result of a previous call to discrim, this 
  ## transform will be applied to data

  if(missing(transform))
    maxdims <- length(unique(labs)) - 1
  else
    maxdims <- ncol(transform$variates)

  if(dims > maxdims) {
    cat("Discrim can only give ", maxdims, 
	" transformed dimensions for ", maxdims + 1, 
	" unique labels\n")
    dims <- maxdims
  }

  if(missing(transform)) {
    ## we must do the cda on data/labs

    datafile <- tempfile("discrim")
    on.exit(unlink(datafile), add = T)

    if(verbose) cat("Writing data...")
    write(t(data), file = datafile, ncolumns = ncol(data))
    if(verbose) cat("done\n")

    labfile <- tempfile("discrim")
    on.exit(unlink(labfile), add = T)

    if(verbose) cat("Writing labels...")
    write(labs, file = labfile, ncolumns = 1)
    if(verbose) cat("done\n")

    resultfile <- tempfile("discrim")
    on.exit(unlink(resultfile), add = T)

    command <- paste(options()$muhome, "/bin/cda -f ", 
		     datafile, " -g ", labfile, " -a ", dims, 
		     " > ", resultfile, sep="")
    if(verbose) cat(command, "\n")

    variates.name <- paste(datafile, ".canonical_variates", sep = "")
    values.name <- paste(datafile, ".canonical_values", sep = "")
    on.exit(unlink(variates.name), add = T)
    on.exit(unlink(values.name), add = T)

    unix(command)

    result <- NULL
    result$data <- matscan(resultfile, dims)
    result$variates <- matscan(variates.name, ncol(data))
    result$values <- matscan(values.name, ncol(data))
    return(result)
  }
  else {
    ## we tranform the data with the existing values/variates
    ## need to write out xx.canonical_variates/values and the 
    ## data file to be transformed
    datafile <- tempfile("discrim")
    on.exit(unlink(datafile), add=T)

    ## in fact it doesn't matter what's in the data file, it 
    ## only serves to give the names of the variates/values files
    write( c(1,2,3), file=datafile)
    
    variates.name <- paste(datafile, ".canonical_variates", sep = "")
    values.name <- paste(datafile, ".canonical_values", sep = "")
    on.exit(unlink(variates.name), add = T)
    on.exit(unlink(values.name), add = T)

    if(verbose) cat("Writing variates...")
    write( t(transform$variates), file=variates.name, 
	  ncolumns=ncol(transform$variates))
    if(verbose) cat("done\n")

    if(verbose) cat("Writing values...")
    write( t(transform$values), file=values.name, 
	  ncolumns=ncol(transform$values))
    if(verbose) cat("done\n")
    
    newdatafile <- tempfile("discrim")
    on.exit( unlink(newdatafile), add = T)

    if(verbose) cat("Writing data...")
    write(t(data), file=newdatafile, ncolumns=ncol(data))
    if(verbose) cat("done\n")

    labfile <- tempfile("discrim")
    on.exit(unlink(labfile), add = T)

    if(verbose) cat("Writing labels...")
    write(labs, file = labfile, ncolumns = 1)
    if(verbose) cat("done\n")

    resultfile <- tempfile("discrim")
    on.exit(unlink(resultfile), add = T)

    command <- paste(options()$muhome, "/bin/cda -f ", 
		     datafile, " -g ", labfile, " -i ", newdatafile,
		     " -a ", dims, " > ", resultfile, sep="")
    if(verbose) cat(command, "\n")

    unix(command)

    result <- matscan(resultfile, dims)
    return(result)
  }
}


# Local Variables:
# mode:S
# S-temp-buffer-p:t
# End:
