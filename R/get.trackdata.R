##' Extract trackdata information from a given emuDB object that 
##' corresponds to the entries in a segment list.
##' 
##' This is the emuR equivalent of the depricated \code{emu.track()} 
##' function. It utilizes the wrassp package for signal processing and 
##' SSFF/audio file handling. It reads time relevant data from a given 
##' segmentlist, extracts the specified trackdata and places it into a 
##' trackdata object (analogous to the depricated emu.track).
##' 
##' @title get.trackdata(): get trackdata from emuDB object
##' @param dbObj emuDB object that the trackdata will be extracted from 
##' @param seglist seglist obtained by querying the emuDB object
##' @param ssffTrackName the name of track that one wishes to extract (see 
##' \code{dbObj$DBconfig$ssffTracks} for the defined ssffTracks of the 
##' dbObj). If the parameter \code{onTheFlyFunctionName} is set then 
##' this corresponds to the column name af the AsspDataObj (see
##' wrasspOutputInfos$<wrassp-function-name>$tracks).
##' @param cut An optional cut time for segment data, ranges between 
##' 0 and 1, a value of 0.5 will extract data only at the segment midpoint.
##' @param npoints An optional number of points to retrieve for each segment or event. 
##' For segments this requires a cut= argument and data is extracted around the cut time. 
##' For events data is extracted around the event time. If npoints is an odd number the 
##' samples are centered around the cut-time-sample, if not they are sqewed to the
##' right by one sample.
##' @param onTheFlyFunctionName name of wrassp function to do on-the-fly calculation 
##' @param onTheFlyParas a list parameters that will be given to the function 
##' passed in by the onTheFlyFunctionName parameter. This list can easily be 
##' generated using the \code{formals} function and then setting the according 
##' parameter one wishes to change.     
##' @param onTheFlyOptLogFilePath path to log file for on-the-fly function
##' @param nrOfAllocationRows If the size limit of the data matrix is reached 
##' a further nrOfAllocationRows more rows will be allocated (this will lead 
##' performance drops). 
##' @param verbose show progress bars and other infos
##' @return If \code{dcut} is not set (the default) an object of type trackdata 
##' is returned. If \code{dcut} is set and \code{npoints} is not, or the seglist 
##' is of type event and npoints is not set a data.frame is returned
##' @author Raphael Winkelmann
##' @seealso \code{\link{formals}}, \code{\link{wrasspOutputInfos}}, \code{\link{trackdata}}
##' @keywords misc
##' @import wrassp
##' @export
"get.trackdata" <- function(dbObj, seglist = NULL, ssffTrackName = NULL, cut = NULL, 
                            npoints = NULL, onTheFlyFunctionName = NULL, onTheFlyParas = NULL, 
                            onTheFlyOptLogFilePath = NULL, nrOfAllocationRows = 10000, verbose = TRUE){
  UseMethod("get.trackdata")
}

##' @export
"get.trackdata.emuDB" <- function(dbObj = NULL, seglist = NULL, ssffTrackName = NULL, cut = NULL, 
                                  npoints = NULL, onTheFlyFunctionName = NULL, onTheFlyParas = NULL, 
                                  onTheFlyOptLogFilePath = NULL, nrOfAllocationRows = 10000, verbose = TRUE){
  
  #########################
  # parameter checks
  
  # check if all values for minimal call are set
  if( is.null(dbObj) || is.null(seglist) || is.null(ssffTrackName)) {
    stop("dbObj, seglist and ssffTrackName have to all be set!\n")
  }
  
  # check if cut value is correct
  if(!is.null(cut)){
    if(cut < 0 || cut > 1){
      stop('Bad value given for cut argument. Cut can only be a value between 0 and 1!')
    }
    if(emusegs.type(seglist) == 'event'){
      stop("Cut value should not be set if emusegs.type(Seglist) == 'event'!")
    }
  }
  
  # check if npoints value is correct
  if(!is.null(npoints)){
    if(is.null(cut) && emusegs.type(seglist) != 'event'){
      stop('Cut argument hast to be set or seglist has to be of type event if npoints argument is used.')
    }
  }
  
  # check if onTheFlyFunctionName is set if onTheFlyParas is
  if(is.null(onTheFlyFunctionName) && !is.null(onTheFlyParas)){
    stop('onTheFlyFunctionName has to be set if onTheFlyParas is set!')
  }
  
  # check if both onTheFlyFunctionName and onTheFlyParas are set if onTheFlyOptLogFilePath is 
  if( !is.null(onTheFlyOptLogFilePath) && (is.null(onTheFlyFunctionName) || is.null(onTheFlyParas))){
    stop('Both onTheFlyFunctionName and onTheFlyParas have to be set for you to be able to use the onTheFlyOptLogFilePath parameter!')
  }
  
  #########################
  # get track definition
  if(is.null(onTheFlyFunctionName)){
  trackDefFound = sapply(dbObj$schema$ssffTracks, function(x){ x$name == ssffTrackName})
  trackDef = dbObj$schema$ssffTracks[trackDefFound]
  
  # check if correct nr of trackDefs where found
  if(length(trackDef) != 1){
    if(length(trackDef) < 1 ){
      stop('The emuDB object ', dbObj$schema$name, ' does not have any ssffTrackDefinitions called ', ssffTrackName)
    }else{
      stop('The emuDB object ', dbObj$schema$name, ' has multiple ssffTrackDefinitions called ', ssffTrackName, '! This means the DB has an invalid _DBconfig.json')
    }
  }
  }else{
    trackDef = list()
    trackDef[[1]] = list()
    trackDef[[1]]$name = ssffTrackName
    trackDef[[1]]$columnName =  ssffTrackName
  }
  
  ###################################
  #create empty index, ftime matrices
  index <- matrix(ncol=2, nrow=length(seglist$utts))
  colnames(index) <- c("start","end")
  
  ftime <- matrix(ncol=2, nrow=length(seglist$utts))
  colnames(ftime) <- c("start","end")
  
  data <- NULL
  origFreq <- NULL
  
  
  ########################
  # preallocate data (needs first element to be read)
  
  curBndl <- get.bundle.stub(dbObj, seglist$utts[1])
  
  if(!is.null(onTheFlyFunctionName)){
    funcFormals = NULL
    funcFormals$listOfFiles = curBndl$mediaFilePath
    funcFormals$ToFile = FALSE
    curDObj = do.call(onTheFlyFunctionName,funcFormals)
  }else{
    fname <- curBndl$signalpaths[grepl(paste(trackDef[[1]]$fileExtension, '$', sep = ''), curBndl$signalpaths)][[1]] # should mybe check if more then one found...
    curDObj <- read.AsspDataObj(fname)
  }
  tmpData <- eval(parse(text = paste("curDObj$", trackDef[[1]]$columnName, sep = "")))
  if(verbose){
    cat('\n  INFO: preallocating data matrix with:', ncol(tmpData), ',', nrOfAllocationRows, 
        'columns and rows.')
  }
  data <- matrix(ncol = ncol(tmpData), nrow = nrOfAllocationRows) # preallocate
  timeStampRowNames = numeric(nrOfAllocationRows) - 1 # preallocate rownames vector. -1 to set default val other than 0
  
  
  
  
  ###############################
  # set up function formals + pb
  if(!is.null(onTheFlyFunctionName)){
    funcFormals = formals(onTheFlyFunctionName)
    funcFormals[names(onTheFlyParas)] = onTheFlyParas
    funcFormals$ToFile = FALSE
    funcFormals$optLogFilePath = onTheFlyOptLogFilePath
    if(verbose){
      cat('\n  INFO: applying', onTheFlyFunctionName, 'to', length(seglist$utts), 'files\n')
      pb <- txtProgressBar(min = 0, max = length(seglist$utts), style = 3)
    }
  }else{
    if(verbose){
      cat('\n  INFO: parsing', length(seglist$utts), trackDef[[1]]$fileExtension, 'files\n')
      pb <- txtProgressBar(min = 0, max = length(seglist$utts), style = 3)
    }
  }
  
  
  # loop through bundle names
  curIndexStart = 1
  for (i in 1:length(seglist$utts)){
    
    curBndl <- get.bundle.stub(dbObj, seglist$utts[i])
    fname <- curBndl$signalpaths[grepl(paste(trackDef[[1]]$fileExtension, '$', sep = ''), curBndl$signalpaths)][[1]] # should mybe check if more then one found...
    
    ################
    #get data object
    
    if(!is.null(onTheFlyFunctionName)){
      funcFormals$listOfFiles = curBndl$mediaFilePath
      curDObj = do.call(onTheFlyFunctionName, funcFormals)
      if(verbose){
        setTxtProgressBar(pb, i)
      }
    }else{
      curDObj <- read.AsspDataObj(fname)
      if(verbose){
        setTxtProgressBar(pb, i)
      }
    }
    
    # set origFreq 
    origFreq <- attr(curDObj, "origFreq")
    
    # set curStart+curEnd
    curStart <- seglist$start[i]
    if(emusegs.type(seglist) == 'event'){
      curEnd <- seglist$start[i]
    }else{
      curEnd <- seglist$end[i]
    }
    
    
    fSampleRateInMS <- (1 / attr(curDObj, "sampleRate")) * 1000
    fStartTime <- attr(curDObj, "startTime") * 1000
    
    # add one on if event to be able to capture in breakValues 
    if(emusegs.type(seglist) == 'event'){
      if(npoints == 1 || is.null(npoints)){
        timeStampSeq <- seq(fStartTime, curEnd + fSampleRateInMS, fSampleRateInMS)
      }else{
        timeStampSeq <- seq(fStartTime, curEnd + fSampleRateInMS * npoints, fSampleRateInMS)
      }
    }else{
      timeStampSeq <- seq(fStartTime, curEnd, fSampleRateInMS)
    }
    
    ##################################################
    # search for first element larger than start time
    breakVal <- -1
    for (j in 1:length(timeStampSeq)){
      if (timeStampSeq[j] >= curStart){
        breakVal <- j
        break
      }
    }
    
    curStartDataIdx <- breakVal
    curEndDataIdx <- length(timeStampSeq)
    
    
    ################
    # extract data
    tmpData <- eval(parse(text = paste("curDObj$", trackDef[[1]]$columnName, sep = "")))
    
    #############################################################
    # set curIndexEnd dependant on if event/segment/cut/npoints
    if(!is.null(cut) || emusegs.type(seglist) == 'event'){
      if(emusegs.type(seglist) == 'event'){
        cutTime = curStart
        curEndDataIdx <- curStartDataIdx
        curStartDataIdx = curStartDataIdx - 1 # last to elements are relevant -> move start to left
      }else{
        cutTime = curStart + (curEnd - curStart) * cut
      }
      
      sampleTimes = timeStampSeq[curStartDataIdx:curEndDataIdx]
      closestIdx = which.min(abs(sampleTimes - cutTime))
      cutTimeSampleIdx = curStartDataIdx + closestIdx - 1
      
      if(is.null(npoints) || npoints == 1){
        # reset data idxs
        curStartDataIdx = curStartDataIdx + closestIdx - 1
        curEndDataIdx = curStartDataIdx
        curIndexEnd = curIndexStart
      }else{
        # reset data idx
        halfNpoints = (npoints - 1) / 2 # -1 removes cutTimeSample
        curStartDataIdx = cutTimeSampleIdx - floor(halfNpoints)
        curEndDataIdx = cutTimeSampleIdx + ceiling(halfNpoints)
        curIndexEnd = curIndexStart + npoints - 1
      }
      
    }else{
      # normal segments
      curIndexEnd <- curIndexStart + curEndDataIdx - curStartDataIdx
    }
    # set index and ftime
    index[i,] <- c(curIndexStart, curIndexEnd)
    ftime[i,] <- c(timeStampSeq[curStartDataIdx], timeStampSeq[curEndDataIdx])
    
    #############################
    # calculate size of and create new data matrix
    rowSeq <- seq(timeStampSeq[curStartDataIdx], timeStampSeq[curEndDataIdx], fSampleRateInMS) 
    curData <- matrix(ncol = ncol(tmpData), nrow = length(rowSeq))
    
    # check if it is possible to extract curData 
    if(curStartDataIdx > 0 && curEndDataIdx <= dim(tmpData)[1]){
      curData[,] <- tmpData[curStartDataIdx:curEndDataIdx,]
    }else{
      entry= paste(Seglist[i,], collapse = " ")
      stop('Can not extract following segmentlist entry: ', entry, ' start and/or end times out of bounds')
    }
    
    ##############################
    # Check if enough space (expand data matrix if necessary) 
    # then append to data matrix 
    if(length(data)<curIndexEnd){
      if(verbose){
        cat('\n  INFO: allocating more space in data matrix')
      }
      data = rbind(data, matrix(ncol = ncol(data), nrow = nrOfAllocationRows))
      timeStampRowNames = c(timeStampRowNames, numeric(nrOfAllocationRows) - 1)
    }
    
    data[curIndexStart:curIndexEnd,] = curData
    timeStampRowNames[curIndexStart:curIndexEnd] <- rowSeq
    curIndexStart <- curIndexEnd + 1
    
    curDObj = NULL
  }
  
  ########################################
  # remove superfluous NA vals from data
  if(verbose){
    cat('\n  INFO: removing superfluous NA vals from over-allocated data matrix\n')
  }
  data = data[complete.cases(data),]
  data = as.matrix(data) # make sure it is a matrix to be able to set row names
  timeStampRowNames = timeStampRowNames[timeStampRowNames != -1]
  
  if((!is.null(cut) && (npoints == 1 || is.null(npoints))) || (emusegs.type(seglist) == 'event' && (npoints == 1 || is.null(npoints)))){
    resObj = as.data.frame(data)
    colnames(resObj) = paste(trackDef[[1]]$columnName, seq(1:ncol(resObj)), sep = '')    
  }else{
    rownames(data) <- timeStampRowNames
    colnames(data) <- paste("T", 1:ncol(data), sep = "")
    ########################################
    #convert data, index, ftime to trackdata
    resObj <- as.trackdata(data, index=index, ftime, ssffTrackName)
    
    if(any(trackDef[[1]]$columnName %in% c("dft", "css", "lps", "cep"))){
      if(!is.null(origFreq)){
        if(verbose){
          cat('\n  INFO: adding fs attribute to trackdata$data fields')
        }
        attr(resObj$data, "fs") <- seq(0, origFreq/2, length=ncol(resObj$data))
        class(resObj$data) <- c(class(resObj$data), "spectral")
      }else{
        stop("no origFreq entry in spectral data file!")
      }
    }
  }
  
  # close progress bar if open
  if(exists('pb')){
    close(pb)
  }
  
  return(resObj)
  
}

#######################
# FOR DEVELOPMENT

# ae.db = load.database('~/Desktop/ae/')
# class(ae.db) = 'emuDB'
# nSegl = query.database(ae.db, 'Phonetic=n')
# get.trackdata(ae.db, seglist = nSegl, ssffTrackName = 'dft')
#td = get.trackdata(ae.db, nSegl, wrasspOutputInfos[['acfana']]$tracks[1], onTheFlyFunctionName = 'acfana', verbose=F)

