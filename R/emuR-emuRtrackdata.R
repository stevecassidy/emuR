##' create emuRtrackdata object
##' 
##' Joins \code{\link{emuRsegs}} and \code{\link{trackdata}} objects
##' to create an \code{\link{emuRtrackdata}} object that is a sub-class of
##' a \code{\link{data.frame}} object. This object 
##' can be viewed as a flat version of a \code{\link{trackdata}} object that also 
##' contains all the information of a \code{\link{emuRsegs}} object. It is meant to
##' ease integration with other packages as it is based on the well known 
##' \code{\link{data.frame}} object.
##' @param sl seglist of class \code{\link{emuRsegs}}
##' @param td \code{\link{trackdata}} object generated from sl
##' @return emuRtrackdata object
##' @export
##' @examples
##' \dontrun{
##' 
##' ##################################
##' # prerequisite: loaded ae emuDB 
##' # (see ?load_emuDB for more information)
##' 
##' # query emuDB (to get object of class emuRsegs)
##' sl = query(emuDBhandle = ae, 
##'            query = "Phonetic == i:")
##'            
##' # get formats for SEGMENTs in sl (to get object of class trackdata)
##' td = get_trackdata(emuDBhandle = ae, 
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
  if(!inherits(sl, "emuRsegs") || !inherits(td, "trackdata")){
    stop("emuRtrackdata could not be created: sl is not of class 'emuRsegs' or td arguments is not of class 'trackdata'")
  }
  
  # check same number of items
  if(dim(td$index)[1] != nrow(sl)){
    stop("emuRtrackdata could not be created: td and sl objects don't have the same number of elements (dim(td$index)[1] != nrow(sl))")
  }
  
  
  nframes = 1 + apply(td$index, 1, diff)
  inds = rep(1:nrow(td), nframes)
  # expand seglist
  expSl = sl[inds,]
  
  times = tracktimes(td)
  start.time = rep(start(td), nframes)
  n.time = times - start.time
  rownames(td$data) = NULL
  resTmp = data.frame(sl_rowIdx = inds, expSl, times_orig = times, times_rel = n.time)
  # calculate normalized time (between 0-1)
  resTmp = resTmp %>% dplyr::group_by_("sl_rowIdx") %>% dplyr::mutate_( times_norm = ~(times_rel / max(times_rel)))
  # add data
  res = data.frame(resTmp, td$data)
  
  class(res) <- c("emuRtrackdata", class(res))
  return(res)
}

##' Normalize length of object
##'
##' @param x object to normalize length for
##' @param ... further arguments to be passed to the next method.
##' @return object of same class as x which lenght has been normalized
##' @export normalize_length
"normalize_length" <- function(x, ...) {
  UseMethod("normalize_length")
}


##' Normalize length of segments contained in an emuRtrackdata object
##'
##' @param x object of class \code{\link{emuRtrackdata}}
##' @param N specify length of normalized segments (each segment in resulting
##' object will consist of \code{N} rows).
##' @param ... further arguments (none used by this method)
##' @return emuRtrackdata
##' @seealso \code{\link{emuRtrackdata} \link{emuRsegs}}
##' @export
"normalize_length.emuRtrackdata" <-function(x, N = 21, ...){
  eRtd.norm=NULL
  for (i in unique(x$sl_rowIdx)){
    eRtd = x[x$sl_rowIdx == i,]
    xynew = approx(eRtd$times_norm, eRtd$T1, n = N)
    # create data.frame of correct length (all relevant entries are replaced)
    eRtd.normtemp = eRtd[1:N,]
    # fill with values of first row (only rel. for redundant columns such as sl_rowIdx, labels)
    eRtd.normtemp[1:N,] = eRtd.normtemp[1,] 
    eRtd.normtemp$times_norm = xynew$x
    num = grep("T",colnames(eRtd))
    numlength = length(num)
    for (y in 1:numlength){
      # interpolate T1, T2, ...
      eRtd.normtemp[,num[y]] = approx(eRtd$times_norm,eRtd[,num[y]], n = N)$y
    }
    # recalculate times_orig & rimes_rel 
    eRtd.normtemp$times_orig = seq(unique(eRtd.normtemp$start), unique(eRtd.normtemp$end),length.out = N)
    eRtd.normtemp$times_rel = seq(0,unique(eRtd.normtemp$end) - unique(eRtd.normtemp$start), length.out = N)
    # TODO to avoid rbind performance problems: preallocate data.frame of 
    # length: N * length(eRtd.norm) and place values directly into it
    eRtd.norm = rbind(eRtd.norm, eRtd.normtemp)

  }
  class(eRtd.norm) <- c("emuRtrackdata", class(eRtd.norm))
  # fix rownames
  row.names(eRtd.norm) = seq(1, nrow(eRtd.norm))
    
  return(eRtd.norm)
}


##' Print emuRtrackdata object
##' @param x object to print
##' @param ... additional params
##' @export
"print.emuRtrackdata" <-  function(x, ...) 
{
  trackNames = names(x)[stringr::str_detect(names(x), 'T.*')]
  printX = '[.data.frame'(x,c('sl_rowIdx', 'labels', 'start', 'end', 'session', 'bundle', 'level', 'type', 'times_orig', 'times_rel', 'times_norm', trackNames))
  print.data.frame(printX, ...)
  cat("\nNOTE: to reduce the verboseness of the output not all colums of an emuRtrackdata object are printed. Use print.data.frame() to print all columns.\n")
}



#######################
# FOR DEVELOPMENT
# library('testthat')
# test_file('tests/testthat/test_emuRtrackdata.R')
