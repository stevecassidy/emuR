##' emu.track
##' 
##' stub function to tell user that a new version is coming
##' 
##' 
##' @keywords internal
##' @export
"emu.track" <- function () 
{
  stop('emu.track is not available in the emuR package. Use the new emu.track2() function instead.')
}



##' emu.track2
##' 
##' A new and improved version of emu.track that utilizes the wrassp package for 
##' signal processing and SSFF file handling
##' 
##' Reads time relevant data from a given seglist and extracts the specified
##' trackdata and places it into a trackdata object (analogos to the depricated emu.track). The
##' seglist has to either contain valid paths to the signal files in which case the according
##' SSFF files have to be in the same folder or 
##' 
##' 
##' @param Seglist seglist obtained by a function of package seglist with
##' option newemuutts=T
##' @param FileExtAndTrackName file extension and trackname separated by a ':' (e.g. fms:fm where fms is the file extension and fm is the track/column name) 
##' @param PathToDbRootFilder is the path to the 
##' @param OnTheFlyFunctionName name of wrassp function to do on-the-fly calculation 
##' @param OnTheFlyParas a list parameters that will be given to the function 
##' passed in by the OnTheFlyFunctionName parameter. This list can easily be 
##' generated using the \code{formals} function and then setting the according 
##' parameter one wishes to change.     
##' @param OnTheFlyOptLogFilePath path to log file for on-the-fly function
##' @return an object of type trackdata is returned
##' @author Raphael Winkelmann
##' @seealso \code{\link{formals}}
##' @keywords internal
##' @import wrassp
##' @export
"emu.track2" <- function(Seglist=NULL, FileExtAndTrackName=NULL, PathToDbRootFolder=NULL,
                         OnTheFlyFunctionName = NULL, OnTheFlyParas=NULL, 
                         OnTheFlyOptLogFilePath=NULL){
  
  if( is.null(Seglist) || is.null(FileExtAndTrackName)) {
    stop("Argument Seglist and FileExtAndtrackname are required.\n")
  }
  
  ###########################
  # split FileExtAndtrackname
  splitQuery = unlist(strsplit(FileExtAndTrackName, ":"))
  
  newFileEx = paste(".",splitQuery[1],sep="")
  colName = splitQuery[2]
  
  ###################################
  # update Seglist paths if neccesary
  print(Seglist)
  Seglist = expandBaseNamesToFullPaths(Seglist, PathToDbRootFolder, newFileEx)
  
  #return(Seglist)
  
  ###################################
  #create empty index, ftime matrices
  index <- matrix(ncol=2, nrow=length(Seglist$utts))
  colnames(index) <- c("start","end")
  
  ftime <- matrix(ncol=2, nrow=length(Seglist$utts))
  colnames(ftime) <- c("start","end")
  
  data <- NULL
  origFreq <- NULL
  
  if(!is.null(OnTheFlyFunctionName)){
    funcFormals = formals(OnTheFlyFunctionName)
    funcFormals[names(OnTheFlyParas)] = OnTheFlyParas
    funcFormals$ToFile = FALSE
    funcFormals$optLogFilePath = OnTheFlyOptLogFilePath
    cat('\n  INFO: applying', OnTheFlyFunctionName, 'to', length(Seglist$utts), 'files\n')
    
    pb <- txtProgressBar(min = 0, max = length(Seglist$utts), style = 3)
  }
  #########################
  # LOOP OVER UTTS
  curIndexStart = 1
  for (i in 1:length(Seglist$utts)){
    
    ####################################################
    #split at "." char (fixed=T to turn off regex matching)
    #dotSplitFilePath <- unlist(strsplit(Seglist$utts[i], ".", fixed=T ))
    #dotSplitFilePath[length(dotSplitFilePath)] <- newFileEx
    
    #fname <- paste(dotSplitFilePath, collapse="")
    fname = Seglist$utts[i]
    ################
    # extract path
    # (may no longer need this little bit at all)
    #con = url(fname)
    #fname = summary(con)$description
    #close(con)
    
    ################
    #get data object
    
    if(!is.null(OnTheFlyFunctionName)){
      setTxtProgressBar(pb, i)
      funcFormals$listOfFiles = Seglist$utts[i]
      curDObj = do.call(OnTheFlyFunctionName,funcFormals)
    }else{
      curDObj <- read.AsspDataObj(fname)
    }
    
    if(is.null(data)){
      text=paste("curDObj$",colName,sep="")
      tmpData <- eval(parse(text=paste("curDObj$",colName,sep=""))) #SIC->try indexing
      data <- matrix(ncol=ncol(tmpData), nrow=0)
      tmpData <- NULL
    }
    origFreq <- attr(curDObj, "origFreq")
    
    curStart <- Seglist$start[i]
    curEnd <- Seglist$end[i]
    
    
    fSampleRateInMS <- (1/attr(curDObj, "sampleRate"))*1000
    fStartTime <- attr(curDObj,"startTime")*1000
    
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
    
    ####################
    # set index and ftime
    curIndexEnd <- curIndexStart+curEndDataIdx-curStartDataIdx
    index[i,] <- c(curIndexStart, curIndexEnd)
    ftime[i,] <- c(timeStampSeq[curStartDataIdx], timeStampSeq[curEndDataIdx])
    
    #############################
    # calculate size of and create new data matrix
    tmpData <- eval(parse(text=paste("curDObj$",colName,sep="")))
    
    rowSeq <- seq(timeStampSeq[curStartDataIdx],timeStampSeq[curEndDataIdx], fSampleRateInMS) 
    curData <- matrix(ncol=ncol(tmpData), nrow=length(rowSeq))
    colnames(curData) <- paste("T", 1:ncol(curData), sep="")
    rownames(curData) <- rowSeq
    curData[,] <- tmpData[curStartDataIdx:curEndDataIdx,] 
    
    ##############################
    # Append to global data matrix app
    data <- rbind(data, curData)
    
    curIndexStart <- curIndexEnd+1
    
    curDObj = NULL
  }
  ########################################
  #convert data, index, ftime to trackdata
  myTrackData <- as.trackdata(data, index=index, ftime, FileExtAndTrackName)
  
  if(any(colName %in% c("dft", "css", "lps", "cep"))){
    if(!is.null(origFreq)){
      attr(myTrackData$data, "fs") <- seq(0, origFreq/2, length=ncol(myTrackData$data))
      class(myTrackData$data) <- c(class(myTrackData$data), "spectral")
    }else{
      stop("no origFreq entry in spectral data file!")
    }
  }
  
  if(!is.null(OnTheFlyFunctionName)){
    close(pb$utts)
  }
  
  return(myTrackData)
  
}


##'
##'
##'
##' @author Raphael Winkelmann
##' @export
expandBaseNamesToFullPaths <- function(Seglist=NULL, PathToDbRootFolder=NULL, fileExt=NULL)
{
  Seglist$utts = paste(Seglist$utts, fileExt, sep="")
  showInfo = T  
  for(i in 1:length(Seglist$utts)){
    if(file.exists(Seglist$utts[i])){
      print("This file exists!!!")
      print(Seglist$utts[i])
    }else{
      if(showInfo){
        cat('INFO: Globing for files is slow! Try to pre expand the basenames to your segmentlist\n')
        showInfo = F
      }
      fullPath = list.files(PathToDbRootFolder, pattern=paste(Seglist$utts[i], "$", sep = ""), recursive=T, full.names=T)
      if(length(fullPath != 0)){
        Seglist$utts[i] = fullPath
      }else{
        stop("Following file could not be found anywhere in ", PathToDbRootFolder, " : ", Seglist$utts[i])
      }
    }
  }
  return(Seglist)
}

# FOR DEVELOPMENT
td = emu.track2(fric, FileExtAndTrackName='fms:fm', PathToDbRootFolder='~/Downloads/kiel03/readI/')
