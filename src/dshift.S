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



"dshift" <- function(dataset, vec)
{
	## subtracts a vector from the corresponding
	## entry in dataset - a list as returned by track()
        lengths <- (dataset$index[, 2] - dataset$index[, 1]) + 1
        vals <- rep(vec, lengths)
        dataset$data <- dataset$data - vals
        dataset
}
 



# Local Variables:
# mode:S
# S-temp-buffer-p:t
# End:
