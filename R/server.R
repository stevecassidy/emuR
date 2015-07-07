require(httpuv)
require(base64enc)

getServerHandle <- function() {
  internalVars$serverHandle
}
setServerHandle <- function(sh) {
  internalVars$serverHandle<-sh
}

##' Serve EMU database to EMU-Webapp
##' 
##' @description Server for EMU-Webapp browser GUI \url{http://ips-lmu.github.io/EMU-webApp/}
##' 
##' Start instructions:
##' 
##' Call this function to start the server. Do not forget to (re-)assign the return value to reflect changes made in the Web application: \code{myDb=serve(myDb)}.
##' 
##' Start a suitable HTML5 capable Web-Browser (Google Chrome, Firefox,...).
##' 
##' Navigate to the EMU-Webapp URL: \url{http://ips-lmu.github.io/EMU-webApp/}.
##' 
##' Press the 'Connect' button in the EMU-webApp and connect with default URL.
##' 
##' EMU-webApp should load the bundle list and the first bundle of the given database (object).
##' 
##' Stop instructions:
##' 
##' Stop the server with the 'Clear' button of the webapp or the reload button of your browser.
##' 
##' The server can be interrupted with Ctrl-C if something wents wrong.
##' 
##' @details  Function opens a HTTP/websocket and waits in a loop for browser requests. The R console will be blocked. On successfull connection the server sends the session and bundle list of the given database object. The Web application requests bundle data for editing. If a bundle is modified with the EMU-webApp and the save button is pressed the server modifies the database object and saves the changes to disk. Communication is defined by EMU-webApp-websocket-protocol version 0.0.2
##' @param dbName name of a loaded EMU database
##' @param host host IP to listen to (default: 127.0.0.1  (localhost))
##' @param port the port number to listen on (default: 17890)
##' @param debug TRUE to enable debugging (default: no debugging messages)
##' @param debugLevel integer higher values generate more detailed debug output
##' @return TRUE if the database was modified, FALSE otherwise
##' @import httpuv jsonlite base64enc
##' @export
##' @author Klaus Jaensch
##' @keywords emuDB EMU-webApp database websocket Emu
##' @examples
##' \dontrun{ 
##' ## Load EMU database 'myDb' and serve it to the EMU-webApp (opens default HTTP/websocket port 17890)
##' 
##' load_emuDB("/path/to/myDb")
##' serve('myDb')
##' }
##' 
serve=function(dbName,host='127.0.0.1',port=17890,debug=FALSE,debugLevel=0){
  if(debug && debugLevel==0){
    debugLevel=2
  }
  modified=FALSE
  emuDBserverRunning=FALSE
  bundleCount=0
  dbUUID=get_emuDB_UUID(dbName=dbName)
  database=.load.emuDB.DBI(uuid = dbUUID)
  if(!is.null(dbUUID)){
    bundlesDf=list_bundles(dbUUID = dbUUID)
  }else{
    stop("Emu database ",dbName, " not found!");
  }
  
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
        cat("Received command from EMU-Webapp: ",jr[['type']],"\n")
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
      if(jr$type == 'GETPROTOCOL'){
        protocolData=list(protocol='EMU-webApp-websocket-protocol',version='0.0.2')
        response=list(status=list(type='SUCCESS'),callbackID=jr$callbackID,data=protocolData)
        responseJSON=jsonlite::toJSON(response,auto_unbox=TRUE,force=TRUE,pretty=TRUE) 
        result=ws$send(responseJSON)
        if(debugLevel >= 2){
          cat("Sent protocol. \n")
        }
        
      }else if(jr$type == 'GETGLOBALDBCONFIG'){
        persistFilter=emuR.persist.filters.DBconfig
        sp=marshal.for.persistence(database[['DBconfig']],persistFilter)
        #sp=WS[['server']][['emuDb']][['DBconfig']]
        if(debugLevel >= 4){
          #cat("Send config: ",sp,"\n")
        }
        response=list(status=list(type='SUCCESS'),callbackID=jr$callbackID,data=sp)
        responseJSON=jsonlite::toJSON(response,auto_unbox=TRUE,force=TRUE,pretty=TRUE) 
        result=ws$send(responseJSON)
        if(debugLevel >= 2){
          if(debugLevel >=4){
            cat(responseJSON,"\n")
          }
          cat("Sent config. \n")
        }
        #}
      }else if(jr$type == 'GETDOUSERMANAGEMENT'){
        # R server mode is single user mode 
        response=list(status=list(type='SUCCESS'),callbackID=jr$callbackID,data="NO")
        responseJSON=jsonlite::toJSON(response,auto_unbox=TRUE,force=TRUE,pretty=TRUE) 
        result=ws$send(responseJSON)
        if(debugLevel >= 2){
          cat("Sent user managment: no. \n")
        }
        
        
      }else if(jr$type == 'GETBUNDLELIST'){
        
        response=list(status=list(type='SUCCESS'),callbackID=jr$callbackID,dataType='uttList',data=bundlesDf)
        responseJSON=jsonlite::toJSON(response,auto_unbox=TRUE,force=TRUE,pretty=TRUE)
        
        if(debugLevel >= 5)cat(responseJSON,"\n")
        result=ws$send(responseJSON)
        if(debugLevel >= 2){
          cat("Sent utterance list with length: ",nrow(bundlesDf)," \n")
        }
        
      }else if(jr$type == 'GETBUNDLE'){
        
        bundleName=jr[['name']]
        bundleSess=jr[['session']]
        #cat("data:",jr[['data']],"\n")
        if(debugLevel>2){
          cat("Requested bundle:",bundleName,",session:",bundleSess,"\n")
        }
        err=NULL
        sc=database[['DBconfig']]
        if(debugLevel>3){
          cat("Convert bundle to S3 format",bundleName,"\n")
        }
        b=get.bundle(dbUUID=dbUUID,sessionName=bundleSess,bundleName=bundleName)
        if(is.null(b)){
          # error
          err=simpleError(paste('Could not load bundle ',bundleName,' of session ',bundleSess))
        }
        if(is.null(err)){
          
          bp=database[['basePath']]
          
          mediaFilePath=file.path(bp, paste0(b$session, session.suffix), paste0(b$name, bundle.dir.suffix), b$annotates)
          if(debugLevel>4){
            cat("Mediafile: ",mediaFilePath," for ",b$name,"\n")
          }
          audioFile=tryCatch(file(mediaFilePath, "rb"),error=function(e) err<<-e)
          if(is.null(err)){
            audioFileData=readBin(audioFile, raw(), n=file.info(mediaFilePath)$size)
            if(inherits(audioFileData,'error')){
              err=audioFileData
            }else{
              audioBase64=base64encode(audioFileData)
              mediaFile=list(encoding="BASE64",data=audioBase64)
              close(audioFile)
            }
          }
        }
        if(is.null(err)){          
          ssffTrackNmsInUse=get.ssfftrack.names.used.by.webapp.config(database[['DBconfig']][['EMUwebAppConfig']])
          if(debugLevel >= 4){
            
            cat(length(ssffTrackNmsInUse)," track definitions in use:\n")
            for(sfInU in ssffTrackNmsInUse){
              cat(sfInU," ")
            }
            cat("\n")
          }
          ssffFiles=list()
          # Hash (here: named charcter vector) with SSFF files extension as key and file path as value
          # avoids duplicates in ssff files list
          ssffFilesHash=character(0)
          for(ssffTr in sc[['ssffTrackDefinitions']]){
            if(ssffTr[['name']] %in% ssffTrackNmsInUse){
              fe=ssffTr[['fileExtension']]
              ssffFilesHash[fe]=file.path(bp, paste0(b$session, session.suffix), paste0(b$name, bundle.dir.suffix), paste0(b$name, ".", fe))
              # commented out to not use signalpaths that rely on track table
              # feRe=paste0('[.]',fe,'$')
              # for(sp in b[['signalpaths']]){
                # if(length(grep(feRe,sp))==1){
                  # ssffFilesHash[fe]=sp
                # }
              # }
            }
          }
          
          # build ssff files list
          ssffFileExts=names(ssffFilesHash)
          for(ssffFileExt in ssffFileExts){
            ssffFilePath=ssffFilesHash[ssffFileExt]
            mf=tryCatch(file(ssffFilePath, "rb"),error=function(e) {err<<-e})
            if(is.null(err)){
              mfData=readBin(mf, raw(), n=file.info(ssffFilePath)$size)
              if(inherits(mfData,'error')){
                err=mfData
                break
              }
            }else{
              break
            }
            mfDataBase64=base64encode(mfData)
            encoding="BASE64"
            ssffDatObj=list(encoding=encoding,data=mfDataBase64,fileExtension=ssffFileExt)
            ssffFiles[[length(ssffFiles)+1]]=ssffDatObj
            close(mf)
          }
          if(is.null(err)){
            anno=marshal.for.persistence(b,emuR.persist.filters.bundle)
            data=list(mediaFile=mediaFile,ssffFiles=ssffFiles,annotation=anno)
          }
        }
        
        if(is.null(err)){
          responseBundle=list(status=list(type='SUCCESS'),callbackID=jr$callbackID,responseContent='bundle',contentType='text/json',data=data)
        }else{
          errMsg=err[['message']]
          cat("Error: ",errMsg,"\n")
          responseBundle=list(status=list(type='ERROR',message=errMsg),callbackID=jr[['callbackID']],responseContent='status',contentType='text/json')
          
        }
        responseBundleJSON=jsonlite::toJSON(responseBundle,auto_unbox=TRUE,force=TRUE,pretty=TRUE)
        result=ws$send(responseBundleJSON)
        if(is.null(err) & debugLevel >= 2){
          
          if(debugLevel >=8){
            cat(responseBundleJSON,"\n")
          }
          cat("Sent bundle containing",length(ssffFiles),"SSFF files\n")
        }
        # reset error
        err=NULL
        
      }else if(jr[['type']] == 'SAVEBUNDLE'){
        jrData=jr[['data']]
        jrAnnotation=jrData[['annotation']]
        bundleSession=jrData[['session']]
        bundleName=jrData[['annotation']][['name']]
        if(debugLevel>3){
          #cat("Save bundle ",names(jr$data),"\n");
          cat("Save bundle ",bundleName," from session ",bundleSession,"\n");
        }
        err=NULL
        
        ssffFiles=jr[['data']][['ssffFiles']]
        oldBundle=get.bundle(dbUUID=dbUUID,sessionName=bundleSession,bundleName=bundleName)
        
        # warnings as errors
        warnOptionSave=getOption('warn')
        options('warn'=2)
        responseBundle=NULL
        
        # do we really need the old bundle in DBI version ?
        if(is.null(oldBundle)){
          # error
          err=simpleError(paste('Could not load bundle ',bundleSession,bundleName))
        }else{
          for(ssffFile in ssffFiles){
            #cat("SSFF file: ",names(ssffFile),"  name: ",ssffFile[['ssffTrackName']],"\n")
            for(ssffTrackDef in database[['DBconfig']][['ssffTrackDefinitions']]){
              ssffTrackExt=ssffTrackDef[['fileExtension']]
              if(ssffTrackExt==ssffFile[['fileExtension']]){
                extPatt=paste0('[.]',ssffTrackExt,'$')
                # TODO store signal paths in a better way!
                for(sp in oldBundle[['signalpaths']]){
                  if(grepl(extPatt,sp)){
                    # store
                    if(debugLevel>3){
                      cat("Writing SSFF track to file: ",sp,"\n")
                    }
                    ssffTrackBin=base64decode(ssffFile[['data']])
                    ssffCon=tryCatch(file(sp,'wb'),error=function(e){err<<-e})
                    if(is.null(err)){
                      res=tryCatch(writeBin(ssffTrackBin,ssffCon))
                      if(inherits(res,'error')){
                        err=res
                        break
                      }
                      modified<<-TRUE
                    }
                  }
                }
              }
              if(!is.null(err)){
                break
              }
            } 
          }
          bundleData=jr[['data']][['annotation']]
          bundle=as.bundle(bundleData=bundleData)
          bundle[['session']]=bundleSession

          # if we do not have an (error) response already
          if(is.null(err)){
            # try to store annotation, dummy error function, use result type to detect errors
            res=tryCatch(store.bundle.annotation(bundle=bundle,dbUUID = dbUUID),error=function(e) e)
            if(inherits(res,'error')){
              err=res
            }else{
              # annotation saved
              # set modified flag
              modified<<-TRUE
              
            }
          }
        }
        if(is.null(err)){
          responseBundle=list(status=list(type='SUCCESS'),callbackID=jr$callbackID,responseContent='status',contentType='text/json')
        }else{
          m=err[['message']]
          cat('Error: ',m,"\n")
          responseBundle=list(status=list(type='ERROR',message=m),callbackID=jr[['callbackID']],responseContent='status',contentType='text/json')
        }
        # response object to JSON 
        responseBundleJSON=jsonlite::toJSON(responseBundle,auto_unbox=TRUE,force=TRUE,pretty=TRUE)
        # send response
        result=ws$send(responseBundleJSON)
        # restore warn level
        options(warn=warnOptionSave)
        
        # reset error
        err=NULL
        
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
    if(!is.null(getServerHandle())){
      cat("Trying to stop orphaned server (handle: ",getServerHandle(),")\n")
      stopServer(getServerHandle())
      sh=tryCatch(startServer(host=host,port=port,app=app),error=function(e) e)
      if(inherits(sh,'error')){
        stop("Error starting server (second try): ",sh,"\n")
      }
    }else{
      stop("Error starting server: ",sh,"\n")
    }
  }
  # store handle global for recovery after crash otr terminated R session
  setServerHandle(sh)
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
  setServerHandle(NULL)
  if(debugLevel>0){
    cat("Closed emuR websocket HTTP service\n")
  }
  return(modified)
}


