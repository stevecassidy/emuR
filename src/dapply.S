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


"dapply"<- function(trackdata, fun, ...)
{
  ## data is a list as returned by track(), a vector
  ## or a matrix of data. Returns the output of fun for
  ## each segment in data
  ## fun must take a matrix or vector of data and an ftime
  ## vector and return an object with components $data and $ftime
  ## dapply must ensure that the resulting data component is
  ## still a matrix, even if the function returns a vector.

  if( version$major >= 5  && oldClass(trackdata)!="trackdata") {
      stop("argument to dapply is not of class trackdata.")
  } else if(class(trackdata)!="trackdata")
    stop("argument to dapply is not of class trackdata.")


  if(!is.matrix(trackdata$index)){
    trackdata$ftime <- rbind(trackdata$ftime)
    trackdata$index <- rbind(trackdata$index)
  }

  
  thisrow <- 1
  newindex <- trackdata$index
  newdata <- NULL
  newftime <- trackdata$ftime

  for(j in 1:nrow(trackdata$index)) {
    newindex[j,1] <- thisrow

    tmp <- fun(trackdata[j]$data, trackdata[j]$ftime, ...)

    if(is.matrix(tmp$data)){
      newdata <- rbind(newdata, tmp$data)
    } else {
      newdata <- c(newdata, tmp$data)
    }
    
    newftime[j,] <- tmp$ftime

    if(is.matrix(tmp$data))
      thisrow <- thisrow + nrow(tmp$data)
    else
      thisrow <- thisrow + length(tmp$data)
    newindex[j,2] <- thisrow - 1

  }

  x <- list(data=as.matrix(newdata), index=newindex, ftime=newftime)
  if( version$major >= 5 ) {
    oldClass(x) <- "trackdata"
  } else {
    class(x) <- "trackdata"
  }
  return(x)
}



# Local Variables:
# mode:S
# S-temp-buffer-p:t
# End:
