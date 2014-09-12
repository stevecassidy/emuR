"get.trackdata" <- function(dbObj, ...){
  UseMethod("get.trackdata")
  
}

"get.trackdata.emuDB" <- function(dbObj, seglist = NULL, ssffTrackName = NULL,
                                  cut = NULL, npoints = NULL, OnTheFlyFunctionName = NULL, OnTheFlyParas = NULL, 
                                  OnTheFlyOptLogFilePath = NULL, NrOfAllocationRows = 100000, verbose = TRUE){
  
  #########################
  # get track definition
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
  if(!is.null(OnTheFlyFunctionName)){
    funcFormals = NULL
    funcFormals$listOfFiles = seglist$utts[1]
    funcFormals$ToFile = FALSE
    curDObj = do.call(OnTheFlyFunctionName,funcFormals)
  }else{
    curBndl <- get.bundle.stub(dbObj, seglist$utts[1])
    fname <- curBndl$signalpaths[grepl(paste(trackDef[[1]]$fileExtension, '$', sep = ''), curBndl$signalpaths)][[1]] # should mybe check if more then one found...
    curDObj <- read.AsspDataObj(fname)
  }
  tmpData <- eval(parse(text = paste("curDObj$", trackDef[[1]]$columnName, sep = "")))
  if(verbose){
    cat('\n  INFO: preallocating data matrix with:', ncol(tmpData), ',', NrOfAllocationRows, 
        'columns and rows.')
  }
  data <- matrix(ncol = ncol(tmpData), nrow = NrOfAllocationRows) # preallocate
  timeStampRowNames = numeric(NrOfAllocationRows) - 1 # preallocate rownames vector. -1 to set default val other than 0
  
  
  
  
  ###############################
  # set up function formals + pb
  if(!is.null(OnTheFlyFunctionName)){
    funcFormals = formals(OnTheFlyFunctionName)
    funcFormals[names(OnTheFlyParas)] = OnTheFlyParas
    funcFormals$ToFile = FALSE
    funcFormals$optLogFilePath = OnTheFlyOptLogFilePath
    cat('\n  INFO: applying', OnTheFlyFunctionName, 'to', length(Seglist$utts), 'files\n')
    
    pb <- txtProgressBar(min = 0, max = length(Seglist$utts), style = 3)
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
    
    if(!is.null(OnTheFlyFunctionName)){
      setTxtProgressBar(pb, i)
      funcFormals$listOfFiles = Seglist$utts[i]
      curDObj = do.call(OnTheFlyFunctionName,funcFormals)
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
      data = rbind(data, matrix(ncol = ncol(data), nrow = NrOfAllocationRows))
      timeStampRowNames = c(timeStampRowNames, numeric(NrOfAllocationRows) - 1)
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
        cat('\n  INFO: adding fs attribute to trackdata$data fields')
        attr(resObj$data, "fs") <- seq(0, origFreq/2, length=ncol(resObj$data))
        class(resObj$data) <- c(class(resObj$data), "spectral")
      }else{
        stop("no origFreq entry in spectral data file!")
      }
    }
  }
  
  # close progress bar if open
  if(!is.null(OnTheFlyFunctionName) && !verbose){
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

