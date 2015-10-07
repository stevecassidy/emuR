require(data.table)
require(dplyr)

##' create emuRtrackdata object
##' 
##' Joins emuRsegs object with a trackdata object 
##' to create an emuRtrackdata object that basically
##' is a \code{\link{data.table}} (and \code{\link{data.frame}}) object.
##' @param sl seglist of class emuRsegs
##' @param td trackdata object generated from sl
##' @return emuRtrackdata object
##' @import data.table
create_emuRtrackdata <- function(sl, td){
  
  ########################
  # check parameters
  
  # check correct classes
  if(!inherits(sl, "emuRsegs") && inherits(td, "trackdata")){
    stop("sl is not of class 'emuRsegs' or td arguments is not of class 'trackdata'")
  }
  
  # check same number of items
  if(dim(td$index)[1] != nrow(sl)){
    stop("td and sl objects don't have the same number of elements (dim(td$index)[1] != nrow(sl))")
  }
  
  
  nframes = 1 + apply(td$index, 1, diff)
  inds = rep(1:nrow(td), nframes)
  # expand seglist
  expSl = sl[inds,]
  
  times = tracktimes(td)
  start.time = rep(start(td), nframes)
  n.time = times - start.time
  rownames(td$data) = NULL
  res = data.table(sl_rowIdx = inds, expSl, times_rel = n.time, times_orig = times, td$data)
  class(res) <- c("emuRtrackdata", class(res))
  return(res)
}


# cut_td <- function(td, ...) UseMethod("cut_td")

##' First try of a simple cut implementation 
##' 
##' lets try it using dplyr... 
##' 
##' this function is intended to mimic some of the dcut behaviour
##' 
##' @param td emuRtrackdata object
##' @param left.time Either: a numeric vector of the same length as there are
##' obsverations in trackdata. Or: a single value between 0 and 1. In the first
##' case, the left time boundary of trackdata[n,] is cut at left.time[n], in
##' the second case, and if prop=T, it is cut at that proportional time.
##' @param right.time Either: a numeric vector of the same length as there are
##' obsverations in trackdata. Or: a single value between 0 and 1. In the first
##' case, the right time boundary of trackdata[n,] is cut at right.time[n], in
##' the second case, and if prop=T, it is cut at that proportional time.
##' @param single If TRUE, one value is returned per segment. This applies when
##' the requested time falls between two track frames. When single=TRUE, the
##' preceding value is returned, unless average=TRUE (see below), in which case
##' the average value of the two frames is returned. when the right.time
##' argument is omitted
##' @param average A single element logical vector - see single above. Applies
##' only when the right.times argument is omitted and when single = TRUE
##' @param prop If TRUE left.time and right.time are interpreted as
##' proportions, if FALSE, they are interpreted as millisecond times
##' @return A trackdata object if both 'left.time' and 'right.time' are
##' specified, otherwise a matrix if 'right.time' is unspecified and the
##' trackdata object has multiple columns of data or a vector if right.time' is
##' unspecified and the trackdata object has a single column of data.
##' @return emuRtrackdata object
##' @import dplyr
# cut_td.emuRtrackdata = function(td, left.time, right.time, 
#                                 single = TRUE, average = TRUE, prop = FALSE){
#   
#   if (prop && missing(right.time)) {
#     res = td %>% 
#       group_by(sl_rowIdx) %>% 
#       mutate(times_propDiff = 1 + (times_orig - min(times_orig)) / (max(times_orig) - min(times_orig)) - left.time) %>%
#       filter(times_propDiff == min(times_propDiff)) %>%
#       dplyr::select(-times_propDiff)
#   }
#   return(as.data.table(res))
# }


#######################
# FOR DEVELOPMENT
# library('testthat')
# test_file('tests/testthat/test_emuRtrackdata.R')
