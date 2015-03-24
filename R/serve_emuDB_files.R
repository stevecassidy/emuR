require(httpuv)
require(base64enc)

##' Serve files of an EMU database to the EMU-webApp
##' 
##' Naive file server for the EMU-webApp browser GUI \url{http://ips-lmu.github.io/EMU-webApp/}
##' 
##' This function implements a websocket server that implements the EMU-webApp-websocket-protocol in version 0.0.2 
##' (\url{https://github.com/IPS-LMU/EMU-webApp/blob/master/app/manual/EMU-webApp-websocket-protocol.md}).
##' It is naive in the sense that it strickly serves the apropriate files of a emuDB to the EMU-webApp without
##' checking their validity or consistency.
##' 
##' Start instructions:
##' \itemize{
##'   \item Call this function to start the server.
##' 
##'   \item Start a suitable, up-to-date HTML5 capable Web-Browser (Google Chrome, Firefox,...).
##' 
##'   \item Navigate to the EMU-webApp URL: \url{http://ips-lmu.github.io/EMU-webApp/}.
##' 
##'   \item Press the 'Connect' button in the EMU-webApp and connect to the default URL (unless default values of host or port paramters have been altered).
##' 
##'   \item EMU-webApp should load the bundle list and the first bundle of the given database.
##' }
##' 
##' Stop instructions:
##' \itemize{
##'   \item Stop the server with the 'Clear' button of the web application or the reload button of your browser.
##' 
##'   \item The server can also be interrupted with Ctrl-C if something goes wrong.
##' }
##' 
##' @param path2dbFolder path to emuDB folder
##' @param sessionPattern regex pattern to be used on sessions to be sent to the EMU-webApp
##' @param bundlePattern regex pattern to be used on bundles to be sent to the EMU-webApp
##' @param host host IP to listen to (default: 127.0.0.1  (localhost))
##' @param port the port number to listen on (default: 17890)
##' @param debug TRUE to enable debugging (default: no debugging messages)
##' @param debugLevel integer higher values generate more detailed debug output
##' @return the possibly modified Emu database object
##' @import httpuv jsonlite base64enc
##' @export
##' @author Raphael Winkelmann (adapted form original serve function from Klaus Jänsch)
##' @keywords emuDB EMU-webApp database websocket Emu
##' @examples
##' \dontrun{ 
##' ## serve the files belonging to a emuDB directly to the EMU-webApp (opens default HTTP/websocket port 17890)
##' 
##' serve("/path/to/myDb")
##' }
##' 
##' @export
serve_emuDB_files=function(path2dbFolder, sessionPattern='.*', bundlePattern='.*', 
                           host='127.0.0.1', port=17890, debug=FALSE, 
                           debugLevel=0){
  
  # precheck if path2dbFolder is a emuDB
  if(!file.exists(file.path(path2dbFolder,paste0(basename(path2dbFolder), '_DBconfig.json')))){
    stop("Could not find _DBconfig.json in: ", path2dbFolder, ". This means the folder is no emuDB.")
  }
  
  if(debug && debugLevel==0){
    debugLevel=2
  }
  emuDBserverRunning=FALSE
  bundleCount=0
  dbConfig = NULL
  allTrackDefsNeededByEMUwebApp = NULL
  
  httpRequest = function(req){
    # Only 
    # Rook conform answer  
    body = paste('<p>http protocol not supported, please use ws protocol.</p>')
    list(
      status = 501L,
      headers = list(
        'Content-Type' = 'text/html'
      ),
      body = body
    )
  }
  
  onHeaders<-function(req){
    # following httuv docs we shoul return NULL here to proceed but that terminates the R session!
    #return(NULL)
  }
  
  serverEstablished = function(ws){
    
    #if(debugLevel>0){
    cat("emuR websocket service established\n")
    #}
    
    serverClosed = function(ws){
      
      #if(debugLevel>0){
      cat("emuR websocket service closed\n")
      #}
      emuRserverRunning<<-FALSE
      
    }
    
    sendError = function(ws,errMsg,callbackID){
      status=list(type='ERROR',details=errMsg);
      response=list(callbackID=callbackID,status)
      responseJSON=jsonlite::toJSON(response,auto_unbox=TRUE,force=TRUE,pretty=TRUE)
      result=ws$send(responseJSON)
    } 
    
    serverReceive = function(isBinary,DATA){
      
      if(debugLevel >= 4 ){
        cat("onMessage() call, binary:",isBinary," data: ",DATA,"\n")
        
      }
      D = ""
      if(is.raw(DATA)) {
        D = rawToChar(DATA)
      }else{
        D = DATA
      }
      jr=jsonlite::fromJSON(D,simplifyVector = FALSE)
      if(debugLevel >= 2 ){
        cat("Received command from EMU-webApp: ",jr[['type']],"\n")
        if(debugLevel >= 3){
          jrNms=names(jr)
          
          for( jrNm in jrNms){
            value=jr[[jrNm]]
            cat("param: ",jrNm)
            if(class(value)=='character'){
              cat(": ",jr[[jrNm]])
            }
            cat("\n")
          }
        }
        
      }
      if(!is.null(jr$type)){
        if(debugLevel >= 2 ){
          cat("Received type from EMU-Webapp: ",jr[['type']],"\n")
        }
      }
      ###############################################################
      # EMU-webApp-websocket-protocol version 0.0.2 starts here
      
      if(jr$type == 'GETPROTOCOL'){
        protocolData=list(protocol='EMU-webApp-websocket-protocol',version='0.0.2')
        response=list(status=list(type='SUCCESS'),callbackID=jr$callbackID,data=protocolData)
        responseJSON=jsonlite::toJSON(response,auto_unbox=TRUE,force=TRUE,pretty=TRUE) 
        result=ws$send(responseJSON)
        if(debugLevel >= 2){
          if(debugLevel >=4){
            cat(responseJSON,"\n")
          }
          cat("Handled request GETPROTOCOL\n")
        }
      }else if(jr$type == 'GETDOUSERMANAGEMENT'){
        # R server mode is single user mode 
        response=list(status=list(type='SUCCESS'),callbackID=jr$callbackID,data="NO")
        responseJSON=jsonlite::toJSON(response,auto_unbox=TRUE,force=TRUE,pretty=TRUE) 
        result=ws$send(responseJSON)
        if(debugLevel >= 2){
          if(debugLevel >=4){
            cat(responseJSON,"\n")
          }
          cat("Handled request GETDOUSERMANAGEMENT\n")
        }
        
      }else if(jr$type == 'GETGLOBALDBCONFIG'){
        tcRes = tryCatch({
          # read _DBconfig.json
          DBconfig = jsonlite::fromJSON(readLines(file.path(path2dbFolder,paste0(basename(path2dbFolder), '_DBconfig.json'))), simplifyVector=F)
          
          response=list(status=list(type='SUCCESS'),callbackID=jr$callbackID,data=DBconfig)
          responseJSON=jsonlite::toJSON(response,auto_unbox=TRUE,force=TRUE,pretty=TRUE)
          
        }, error = function(e){
          return(e)
        }, warning = function(w){
          return(simpleError(w)) # turning warning into error
        })
        
        # check if error occured
        if(inherits(tcRes, 'error')){
          m = paste0("An error occured handeling GETBUNDLELIST request: ", tcRes$message)
          response = list(status=list(type='ERROR',message=m),callbackID=jr$callbackID,responseContent='status',contentType='text/json')
          responseJSON=jsonlite::toJSON(response,auto_unbox=TRUE,force=TRUE,pretty=TRUE)
        }else{
          # set dbConfig varibale of parent scope to be able to access it in other requests
          dbConfig <<- DBconfig
          allTrackDefsNeededByEMUwebApp <<- findAllTracksInDBconfigNeededByEMUwebApp(DBconfig)
        }  
        
        # send reply
        result=ws$send(responseJSON)
        
        
        if(debugLevel >= 2){
          if(debugLevel >= 4){
            cat(responseJSON,"\n")
          }
          cat("Handled request GETGLOBALDBCONFIG\n")
        }
        
      }else if(jr$type == 'GETBUNDLELIST'){
        
        tcRes = tryCatch({
          # list all folders
          allFolders = list.dirs(path2dbFolder)
          # extract bundle names from folder list
          name = gsub('_bndl', '', basename(allFolders[grepl('_bndl$', allFolders) & grepl('_ses', allFolders)]))
          # extract session names from folder list
          session = gsub(paste0(".*", .Platform$file.sep, "(.*)_ses.*"),'\\1', allFolders[grepl('_bndl$', allFolders) & grepl('_ses', allFolders)], perl=T)
          # create DF
          bundlesDf = as.data.frame(cbind(name, session), stringsAsFactors = F)
          
          response=list(status=list(type='SUCCESS'),callbackID=jr$callbackID,dataType='uttList',data=bundlesDf)
          responseJSON=jsonlite::toJSON(response,auto_unbox=TRUE,force=TRUE,pretty=TRUE)
          
        }, error = function(e){
          return(e)
        }, warning = function(w){
          return(simpleError(w)) # turning warning into error
        })
        
        # check if error occured
        if(inherits(tcRes, 'error')){
          m = paste0("An error occured handeling GETBUNDLELIST request: ", tcRes$message)
          response = list(status=list(type='ERROR',message=m),callbackID=jr$callbackID,responseContent='status',contentType='text/json')
          responseJSON=jsonlite::toJSON(response,auto_unbox=TRUE,force=TRUE,pretty=TRUE)
        }
        
        # send reply
        result=ws$send(responseJSON)
        
        
        if(debugLevel >= 2){
          if(debugLevel >= 4){
            cat(responseJSON,"\n")
          }
          cat("Handled request GETBUNDLELIST\n")
        }
        
      }else if(jr$type == 'GETBUNDLE'){
        
        bundleName=jr$name   
        sessionName=jr$session
        
        if(debugLevel > 3){
          cat("Requested bundle:",bundleName,",session:",sessionName,"\n")
        }
        tcRes = tryCatch({
          # get mediaFile
          mediaFilePath = file.path(path2dbFolder, paste0(sessionName, '_ses'), paste0(bundleName, '_bndl'), paste0(bundleName, '.', dbConfig$mediafileExtension))
          audioFile <- file(mediaFilePath, "rb")
          audioFileData=readBin(audioFile, raw(), n=file.info(mediaFilePath)$size)
          audioBase64=base64encode(audioFileData)
          close(audioFile)
          
          mediaFile=list(encoding="BASE64",data=audioBase64)
          
          # get annot file
          annotFilePath = file.path(path2dbFolder, paste0(sessionName, '_ses'), paste0(bundleName, '_bndl'), paste0(bundleName, '_annot.json'))
          annotFile = file(annotFilePath, "r+t")
          annotation = jsonlite::fromJSON(readLines(annotFile), simplifyVector=F)
          close(annotFile)
          
          
          
          # get SSFF files
          ssffFiles = list()
          
          for(td in allTrackDefsNeededByEMUwebApp){
            ssffFilePath = file.path(path2dbFolder, paste0(sessionName, '_ses'), paste0(bundleName, '_bndl'), paste0(bundleName, '.', td$fileExtension))
            # if FORMANTS track check open in read+write mode
            if(td$name == "FORMANTS"){
              mf<- file(ssffFilePath, "r+b")
            }else{
              mf<- file(ssffFilePath, "rb")
            }
            mfData=readBin(mf, raw(), n=file.info(ssffFilePath)$size)
            mfDataBase64=base64encode(mfData)
            encoding="BASE64"
            close(mf)
            ssffDatObj=list(encoding=encoding,data=mfDataBase64,fileExtension=td$fileExtension)
            ssffFiles[[length(ssffFiles) + 1]] = ssffDatObj
          }
          
          # combind and send response back
          data=list(mediaFile=mediaFile,ssffFiles=ssffFiles,annotation=annotation)
          
          responseBundle=list(status=list(type='SUCCESS'),callbackID=jr$callbackID,responseContent='bundle',contentType='text/json',data=data)
          responseJSON=jsonlite::toJSON(responseBundle,auto_unbox=TRUE,force=TRUE,pretty=TRUE)
          
        }, error = function(e){
          return(e)
        }, warning = function(w){
          return(simpleError(w)) # turning warning into error
        })
        
        # check if error occured
        if(inherits(tcRes, 'error')){
          m = paste0("An error occured handeling GETBUNDLE request: ", tcRes$message)
          response = list(status=list(type='ERROR',message=m),callbackID=jr$callbackID,responseContent='status',contentType='text/json')
          responseJSON=jsonlite::toJSON(response,auto_unbox=TRUE,force=TRUE,pretty=TRUE)
        }
        
        # send reply
        result=ws$send(responseJSON)
        
        if(debugLevel >= 2){
          if(debugLevel >= 4){
            cat(responseBundleJSON,"\n")
          }
          cat("Handled request GETBUNDLE\n")
        }
        
      }else if(jr$type == 'SAVEBUNDLE'){
        jrData=jr[['data']]
        jrAnnotation=jrData[['annotation']]
        sessionName=jrData[['session']]
        bundleName=jr$data$annotation$name
        
        if(debugLevel > 3){
          cat("Save bundle ",bundleName," from session ",bundleName,"\n");
        }
        
        #                 }
        ssffFiles=jr[['data']][['ssffFiles']]
        tcRes = tryCatch({
          # save SSFF files
          for(ssffFile in ssffFiles){
            # check if FORMANTS track as this is the only track that is currently allowed to be saved
            trackDef = NULL;
            for(td in allTrackDefsNeededByEMUwebApp){
              if(td$fileExtension == ssffFile$fileExtension){
                trackDef = td
              }
            }
            if(trackDef$name == 'FORMANTS'){
              ssffTrackBin=base64decode(ssffFile[['data']]) 
              ssffFilePath = file.path(path2dbFolder, paste0(sessionName, '_ses'), paste0(bundleName, '_bndl'), paste0(bundleName, '.', trackDef$fileExtension))
              writeBin(ssffTrackBin, ssffFilePath)
            }else{
              stop('still have to do propper error handeling! only FORMANTS track can be saved')
            }
          }
          # save annot file
          annotJSON=jsonlite::toJSON(jrAnnotation,auto_unbox=TRUE,force=TRUE,pretty=TRUE)
          annotFilePath = file.path(path2dbFolder, paste0(sessionName, '_ses'), paste0(bundleName, '_bndl'), paste0(bundleName, '_annot.json'))
          writeLines(annotJSON, annotFilePath)
          
          response=list(status=list(type='SUCCESS'),callbackID=jr$callbackID,responseContent='status',contentType='text/json')
          responseJSON=jsonlite::toJSON(response,auto_unbox=TRUE,force=TRUE,pretty=TRUE)
          
        }, error = function(e){
          return(e)
        }, warning = function(w){
          return(simpleError(w)) # turning warning into error
        })
        
        # check if error occured
        if(inherits(tcRes, 'error')){
          m = paste0("An error occured handeling SAVEBUNDLE request: ", tcRes$message)
          response = list(status=list(type='ERROR',message=m),callbackID=jr$callbackID,responseContent='status',contentType='text/json')
          responseJSON=jsonlite::toJSON(response,auto_unbox=TRUE,force=TRUE,pretty=TRUE)
        }
        
        result=ws$send(responseJSON)
        
        if(debugLevel >= 2){
          if(debugLevel >= 4){
            cat(responseJSON,"\n")
          }
          cat("Handled request GETBUNDLELIST\n")
        }
      }else if(jr[['type']]=='DISCONNECTWARNING'){
        response=list(status=list(type='SUCCESS'),callbackID=jr[['callbackID']],responseContent='status',contentType='text/json')
        responseJSON=jsonlite::toJSON(response,auto_unbox=TRUE,force=TRUE,pretty=TRUE)
        result=ws$send(responseJSON)
        emuRserverRunning<<-FALSE
        ws$close()
        cat("emuR websocket service closed by EMU-Webapp\n")
      }
    }
    ws$onMessage(serverReceive)
    ws$onClose(serverClosed)
  }
  
  app=list(call=httpRequest,onHeaders=onHeaders,onWSOpen=serverEstablished)
  sh=tryCatch(startServer(host=host,port=port,app=app),error=function(e) e)
  if(inherits(sh,'error')){
    if(exists('.emuR.server.serverHandle') & !is.null(.emuR.server.serverHandle)){
      cat("Trying to stop orphaned server (handle: ",.emuR.server.serverHandle,")\n")
      stopServer(.emuR.server.serverHandle)
      sh=tryCatch(startServer(host=host,port=port,app=app),error=function(e) e)
      if(inherits(sh,'error')){
        stop("Error starting server (second try): ",sh,"\n")
      }
    }else{
      stop("Error starting server: ",sh,"\n")
    }
  }
  # store handle global for recovery after crash otr terminated R session
  .emuR.server.serverHandle<<-sh
  cat("Navigate your browser to the EMU-Webapp URL: http://ips-lmu.github.io/EMU-webApp/\n")
  cat("Server connection URL: ws://localhost:",port,"\n",sep='')
  cat("To stop the server press EMU-Webapp 'clear' button or reload the page in your browser.\n")
  #cat("EMU-Webapp server handle:",sh,"\n")
  emuRserverRunning=TRUE
  
  while(emuRserverRunning) {
    #cat("emuR websocket service...",emuRserverRunning,"\n")
    service()
    Sys.sleep(0.01)
    
  }
  stopServer(sh)
  # regular shutdown, remove handle 
  .emuR.server.serverHandle<<-NULL
  if(debugLevel>0){
    cat("Closed emuR websocket HTTP service\n")
  }
  #   return(database)
}

## searches for all tracks needed by the EMUwebApp and
## returns their ssffTrackDefinitions
findAllTracksInDBconfigNeededByEMUwebApp <- function(DBconfig){
  allTracks = NULL
  
  # anagestConfig ssffTracks
  for(ld in DBconfig$levelDefinitions){
    allTracks = c(allTracks, ld$anagestConfig$verticalPosSsffTrackName, ld$anagestConfig$velocitySsffTrackName)
  }
  
  for(p in DBconfig$EMUwebAppConfig$perspectives){
    # tracks in signalCanvases$order
    for(sco in p$signalCanvases$order){
      allTracks = c(allTracks, sco)
    }
    # tracks in signalCanvases$assign
    for(sca in p$signalCanvases$assign){
      allTracks = c(allTracks, sca$ssffTrackName)
    }
    # tracks in p$twoDimCanvases$twoDimDrawingDefinitions
    for(tddd in p$twoDimCanvases$twoDimDrawingDefinitions){
      # dots
      for(dot in tddd$dots){
        allTracks = c(allTracks, dot$xSsffTrack, dot$ySsffTrack)
      }
    }
  }
  # uniq tracks
  allTracks = unique(allTracks)
  # remove OSCI and SPEC tracks
  allTracks = allTracks[allTracks != 'OSCI' & allTracks != 'SPEC']
  
  # get corresponding ssffTrackDefinitions
  allTrackDefs = list()
  for(std in DBconfig$ssffTrackDefinitions){
    if(std$name %in% allTracks){
      allTrackDefs[[length(allTrackDefs) + 1]] = std
    }
  }
  
  return(allTrackDefs)
}

#################################
# FOR DEVELOPMENT
# findAllTracksInDBconfigNeededByEMUwebApp(gersC$DBconfig)
