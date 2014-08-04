##' emu.track
##' 
##' function stub to tell user that a new version is coming
##' 
##' 
##' @keywords internal
##' @export
"emu.track" <- function () 
{
  stop('emu.track is not available in the emuR package. Please use the new emu.track2() function instead.')
}



##' Extract trackdata information for a given segmentlist
##' 
##' A new and improved version of emu.track that utilizes the wrassp package for 
##' signal processing and SSFF file handling.
##' 
##' Reads time relevant data from a given segmentlist, extracts the specified
##' trackdata and places it into a trackdata object (analogos to the depricated emu.track). The
##' segmentlist$utts has to either contain valid paths to the signal files in which case the according
##' SSFF files have to be in the same folder or valid paths to the SSFF files
##' 
##' 
##' @param Seglist seglist obtained by a function of package seglist with
##' option newemuutts=T
##' @param FileExtAndTrackName file extension and trackname separated by a ':' (e.g. fms:fm where fms is the file extension and fm is the track/column name) 
##' @param PathToDbRootFolder is the path to the 
##' @param cut An optional cut time for segment data, ranges between 0 and 1, a value of 0.5 will extract data only at the segment midpoint.
##' @param npoints An optional number of points to retrieve for each segment or event. For segments this requires a cut= argument and data is extracted around the cut time. For events data is extracted around the event time.
##' @param OnTheFlyFunctionName name of wrassp function to do on-the-fly calculation 
##' @param OnTheFlyParas a list parameters that will be given to the function 
##' passed in by the OnTheFlyFunctionName parameter. This list can easily be 
##' generated using the \code{formals} function and then setting the according 
##' parameter one wishes to change.     
##' @param OnTheFlyOptLogFilePath path to log file for on-the-fly function
##' @param NrOfAllocationRows If the size limit of the data matrix is reached a further NrOfAllocationRows more rows will be allocated (this will leed performance drops). 
##' @return If dcut is NOT set (the default) a object of type trackdata is returned. If dcut is set and npoints is NOT, a data.frame is returned
##' @author Raphael Winkelmann
##' @seealso \code{\link{formals}}
##' @keywords misc
##' @import wrassp
##' @export
"emu.track2" <- function(Seglist = NULL, FileExtAndTrackName = NULL, PathToDbRootFolder = NULL,
                         cut = NULL, npoints = NULL, OnTheFlyFunctionName = NULL, OnTheFlyParas = NULL, 
                         OnTheFlyOptLogFilePath = NULL, NrOfAllocationRows = 1000000){
  
  if( is.null(Seglist) || is.null(FileExtAndTrackName)) {
    stop("Argument Seglist and FileExtAndtrackname are required.\n")
  }
  
  ###########################
  # split FileExtAndtrackname
  splitQuery = unlist(strsplit(FileExtAndTrackName, ":"))
  
  fileExt = paste(".",splitQuery[1],sep="")
  colName = splitQuery[2]
  
  ###################################
  # update Seglist paths if neccesary
  if(is.null(OnTheFlyFunctionName)){
    Seglist = getFiles(Seglist, PathToDbRootFolder, fileExt)
  }else{
    Seglist = getFiles(Seglist, PathToDbRootFolder, '.wav')
  }
  
  ####################################
  # check if cut value is correct
  if(!is.null(cut)){
    if(cut < 0 || cut > 1){
      stop('Bad value given for cut argument. Cut can only be a value between 0 and 1!')
    }
  }
  
  ####################################
  # check if npoints value is correct
  if(!is.null(npoints)){
    if(npoints%%2 == 0){
      stop('Bad value given for npoints argument. Npoints has to be an odd number!')
    }
    if(is.null(cut)){
      stop('Cut argument hast to be set if npoints argument is used.')
    }
  }
  
  ###################################
  #create empty index, ftime matrices
  index <- matrix(ncol=2, nrow=length(Seglist$utts))
  colnames(index) <- c("start","end")
  
  ftime <- matrix(ncol=2, nrow=length(Seglist$utts))
  colnames(ftime) <- c("start","end")
  
  data <- NULL
  origFreq <- NULL
  
  
  ########################
  # preallocate data (needs first element to be read)
  if(!is.null(OnTheFlyFunctionName)){
    funcFormals$listOfFiles = Seglist$utts[1]
    curDObj = do.call(OnTheFlyFunctionName,funcFormals)
  }else{
    curDObj <- read.AsspDataObj(Seglist$utts[1])
  }
  tmpData <- eval(parse(text = paste("curDObj$", colName, sep = "")))
  cat('\n  INFO: preallocating data matrix with:', ncol(tmpData), ',', NrOfAllocationRows, 
      'columns and rows.')
  data <- matrix(ncol = ncol(tmpData), nrow = NrOfAllocationRows) # preallocate
  
  #########################
  # set up function formals + pb
  if(!is.null(OnTheFlyFunctionName)){
    funcFormals = formals(OnTheFlyFunctionName)
    funcFormals[names(OnTheFlyParas)] = OnTheFlyParas
    funcFormals$ToFile = FALSE
    funcFormals$optLogFilePath = OnTheFlyOptLogFilePath
    cat('\n  INFO: applying', OnTheFlyFunctionName, 'to', length(Seglist$utts), 'files\n')
    
    pb <- txtProgressBar(min = 0, max = length(Seglist$utts), style = 3)
  }else{
    cat('\n  INFO: parsing', length(Seglist$utts), fileExt, 'files\n')
    pb <- txtProgressBar(min = 0, max = length(Seglist$utts), style = 3)
  }
  
  #########################
  # LOOP OVER UTTS
  curIndexStart = 1
  for (i in 1:length(Seglist$utts)){
    
    fname = Seglist$utts[i]
    
    ################
    #get data object
    
    if(!is.null(OnTheFlyFunctionName)){
      setTxtProgressBar(pb, i)
      funcFormals$listOfFiles = Seglist$utts[i]
      curDObj = do.call(OnTheFlyFunctionName,funcFormals)
    }else{
      curDObj <- read.AsspDataObj(fname)
      setTxtProgressBar(pb, i)
    }
    
    origFreq <- attr(curDObj, "origFreq")
    
    curStart <- Seglist$start[i]
    curEnd <- Seglist$end[i]
    
    
    fSampleRateInMS <- (1 / attr(curDObj, "sampleRate")) * 1000
    fStartTime <- attr(curDObj, "startTime") * 1000
    
    timeStampSeq <- seq(fStartTime, curEnd, fSampleRateInMS)
    
    ###########################################
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
    
    if(!is.null(cut)){
      if(is.null(npoints)){
        cutTime = curStart + (curEnd - curStart) * cut
        print(cutTime)
      }
      #       closestIdx = which(min(abs(timeStampSeq-cutTime)))
    }
    
    ####################
    # set index and ftime
    curIndexEnd <- curIndexStart + curEndDataIdx - curStartDataIdx
    index[i,] <- c(curIndexStart, curIndexEnd)
    ftime[i,] <- c(timeStampSeq[curStartDataIdx], timeStampSeq[curEndDataIdx])
    
    #############################
    # calculate size of and create new data matrix
    tmpData <- eval(parse(text = paste("curDObj$", colName, sep = "")))
    rowSeq <- seq(timeStampSeq[curStartDataIdx], timeStampSeq[curEndDataIdx], fSampleRateInMS) 
    curData <- matrix(ncol = ncol(tmpData), nrow = length(rowSeq))
    colnames(curData) <- paste("T", 1:ncol(curData), sep = "")
    rownames(curData) <- rowSeq
    
    # check if it is possible to extract curData
    if(curStartDataIdx > 0 && curEndDataIdx <= dim(tmpData)[1]){
      curData[,] <- tmpData[curStartDataIdx:curEndDataIdx,]
    }else{
      entry= paste(Seglist[i,], collapse = " ")
      stop('Can not extract following segmentlist entry: ', entry, ' start and/or end times out of bounds')
    }
    
    ##############################
    # Check if enough space (expand data matrix ifnecessary) 
    # then append to data matrix 
    if(length(data)<curIndexEnd){
      cat('\n  INFO: allocating more space in data matrix')
      data = rbind(data, matrix(ncol = ncol(data), nrow = NrOfAllocationRows))
    }
    
    data[curIndexStart:curIndexEnd,] = curData
    curIndexStart <- curIndexEnd + 1
    
    curDObj = NULL
  }
  ########################################
  # remove superfluous NA vals from data
  cat('\n  INFO: removing superfluous NA vals from over-allocated data matrix')
  data = data[complete.cases(data),]
  
  ########################################
  #convert data, index, ftime to trackdata
  myTrackData <- as.trackdata(data, index=index, ftime, FileExtAndTrackName)
  
  if(any(colName %in% c("dft", "css", "lps", "cep"))){
    if(!is.null(origFreq)){
      cat('\n  INFO: adding fs attribute to trackdata$data fields')
      attr(myTrackData$data, "fs") <- seq(0, origFreq/2, length=ncol(myTrackData$data))
      class(myTrackData$data) <- c(class(myTrackData$data), "spectral")
    }else{
      stop("no origFreq entry in spectral data file!")
    }
  }
  
  if(!is.null(OnTheFlyFunctionName)){
    close(pb)
  }
  
  return(myTrackData)
  
}


# FOR DEVELOPMENT
#system.time(emu.track2(new.sWithExpUtts[1:200,], 'dft:dft', path2db, NrOfAllocationRows = 100000))
#td = emu.track2(new.sWithExpUtts, 'dft:dft', path2db)
n = emu::emu.query('ae','*','Phonetic=n')
emu.track2(n, 'fms:fm', '~/emuDBs/ae/', cut=.5)

