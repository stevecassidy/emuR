require(data.table)
require(dplyr)

##' create emuRtrackdata object
##' 
##' Joins \code{\link{emuRsegs}} and \code{\link{trackdata}} objects
##' to create an emuRtrackdata object that is a sub-class of
##' a \code{\link{data.table}} (and \code{\link{data.frame}}) object. This object 
##' can be viewed as a flat version of a \code{\link{trackdata}} object that also 
##' contains all the information of a \code{\link{emuRsegs}} object. It is meant to
##' ease integration with other packages as it is based on the well known 
##' \code{\link{data.table}} and \code{\link{data.frame}} objects.
##' @param sl seglist of class \code{\link{emuRsegs}}
##' @param td \code{\link{trackdata}} object generated from sl
##' @return emuRtrackdata object
##' @import data.table
##' @examples
##' \dontrun{
##' 
##' ##################################
##' # prerequisite: loaded "ae" emuDB 
##' # (see ?load_emuDB for more information)
##' 
##' # query emuDB (to get object of class emuRsegs)
##' sl = query(dbName = "ae", 
##'            query = "Phonetic == i:")
##'            
##' # get formats for SEGMENTs in sl (to get object of class trackdata)
##' td = get_trackdata(dbName = "ae", 
##'                    seglist = sl,
##'                    onTheFlyFunctionName = "forest")
##' 
##' # create emuRtrackdata object
##' create_emuRtrackdata(sl = sl, td = td)
##' 
##' }
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


##' Function to extract an emuRtrackdata at a single time point of to create another EMU-trackdata object between two times
##' 
##' lets try it using dplyr... 
##' 
##' this function is intended to mimic some of the dcut behaviour
##' 
##' @param emuRtrackdata emuRtrackdata object
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
# cut.emuRtrackdata = function(emuRtrackdata, leftTime, rightTime, 
#                              single = TRUE, average = TRUE, prop = FALSE){
#   
#   if (missing(rightTime)) {
#     if(length(leftTime == 1)){
#       res = emuRtrackdata %>% 
#         group_by(sl_rowIdx) %>% 
#         mutate(times_propDiff = 1 + (times_orig - min(times_orig)) / (max(times_orig) - min(times_orig)) - leftTime) %>%
#         filter(times_propDiff == min(times_propDiff)) %>%
#         dplyr::select(-times_propDiff)
#     }else{
#       
#     }
#   }
#   return(as.data.table(res))
# }


#######################
# FOR DEVELOPMENT
# library('testthat')
# test_file('tests/testthat/test_emuRtrackdata.R')
