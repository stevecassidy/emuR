require(httpuv)
require(base64enc)

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
##' @param database a emuDB database object
##' @param host host IP to listen to (default: 127.0.0.1  (localhost))
##' @param port the port number to listen on (default: 17890)
##' @param debug TRUE to enable debugging (default: no debugging messages)
##' @param debugLevel integer higher values generate more detailed debug output
##' @return the possibly modified Emu database object
##' @import httpuv jsonlite base64enc
##' @export
##' @author Klaus Jaensch
##' @keywords emuDB EMU-webapp database websocket Emu
##' @examples
##' \dontrun{ 
##' ## Load an EMU database and serve it to the EMU-webApp (opens default HTTP/websocket port 17890)
##' 
##' myDb=load.emuDB("/path/to/myDb")
##' myDb=serve(myDb)
##' }
##' 
serve<-function(database,host='127.0.0.1',port=17890,debug=FALSE,debugLevel=0){
  UseMethod('serve')
}

##' @export
serve.emuDB=function(database,host='127.0.0.1',port=17890,debug=FALSE,debugLevel=0){
  if(debug && debugLevel==0){
    debugLevel=2
  }
  emuDBserverRunning=FALSE
  bundleCount=0
  if(is(database,'emuDB')){
    for(s in database[['sessions']]){
      sName=s[['name']]
      bundleCount=bundleCount+length(s[['bundles']])
    }
    bundlesDf=data.frame(name=character(bundleCount),session=character(bundleCount),stringsAsFactors = FALSE)
    ## create dummy bundle list
    #ulNms=attr(database$sessions[[1]]$bundles,'names')
    #emuRuttList=list()
    #for(ulNm in ulNms){
    #emuRuttList[[length(emuRuttList)+1]]=list(name=ulNm)
    #}
    idx=1
    for(s in database[['sessions']]){
      sName=s[['name']]
      for(b in s[['bundles']]){
        bundlesDf[idx,'session']=sName
        bundlesDf[idx,'name']=b[['name']]
        idx=idx+1
      }
    }
    
    
  }
  else{
    stop("Supported object classes for database: 'emuDB'");
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
        
        uttCode=jr$name   
        bundleSess=jr[['session']]
        #cat("data:",jr[['data']],"\n")
        if(debugLevel>2){
          cat("Requested bundle:",uttCode,",session:",bundleSess,"\n")
        }
        sc=database[['DBconfig']]
        if(debugLevel>3){
          cat("Convert bundle to S3 format",uttCode,"\n")
        }
        b=get.bundle(database,bundleSess,uttCode)
        if(is.null(b)){
          # error
          m=paste('Could not load bundle ',uttCode,' of session ',bundleSess)
          responseBundle=list(status=list(type='ERROR',message=m),callbackID=jr$callbackID,responseContent='status',contentType='text/json')
        }else{
          
          bp=database[['basePath']]
          
          mediaFilePath=b$mediaFilePath
          if(debugLevel>4){
            cat("Mediafile: ",mediaFilePath," for ",b$name,"\n")
          }
          audioFile <- file(mediaFilePath, "rb")
          audioFileData=readBin(audioFile, raw(), n=file.info(mediaFilePath)$size)
          audioBase64=base64encode(audioFileData)
          close(audioFile)
          
          mediaFile=list(encoding="BASE64",data=audioBase64)
          
          ssffTrackNmsInUse=get.ssfftrack.names.used.by.webapp.config(database[['DBconfig']][['EMUwebAppConfig']])
          ssffFiles=list()
          # Hash (here: named charcter vector) with SSFF files extension as key and file path as value
          # avoids duplicates in ssff files list
          ssffFilesHash=character(0)
          for(ssffTr in sc[['ssffTrackDefinitions']]){
            if(ssffTr[['name']] %in% ssffTrackNmsInUse){
              fe=ssffTr[['fileExtension']]
              feRe=paste0('[.]',fe,'$')
              for(sp in b[['signalpaths']]){
                if(length(grep(feRe,sp))==1){
                  ssffFilesHash[fe]=sp
                }
              }
            }
          }
          
          # build ssff files list
          ssffFileExts=names(ssffFilesHash)
          cat("SSFF file extensions",ssffFileExts,"\n")
          for(ssffFileExt in ssffFileExts){
            ssffFilePath=ssffFilesHash[ssffFileExt]
            mf<- file(ssffFilePath, "rb")
            mfData=readBin(mf, raw(), n=file.info(sp)$size)
            mfDataBase64=base64encode(mfData)
            encoding="BASE64"
            close(mf)
            ssffDatObj=list(encoding=encoding,data=mfDataBase64,fileExtension=ssffFileExt)
            ssffFiles[[length(ssffFiles)+1]]=ssffDatObj
            
          }
          
          
          anno=marshal.for.persistence(b,emuR.persist.filters.bundle)
          data=list(mediaFile=mediaFile,ssffFiles=ssffFiles,annotation=anno)
          
          responseBundle=list(status=list(type='SUCCESS'),callbackID=jr$callbackID,responseContent='bundle',contentType='text/json',data=data)
        }
        responseBundleJSON=jsonlite::toJSON(responseBundle,auto_unbox=TRUE,force=TRUE,pretty=TRUE)
        result=ws$send(responseBundleJSON)
        if(debugLevel >= 2){
          
          if(debugLevel >=8){
            cat(responseBundleJSON,"\n")
          }
          cat("Sent bundle containing",length(ssffFiles),"SSFF files\n")
        }
        
      }else if(jr$type == 'SAVEBUNDLE'){
        jrData=jr[['data']]
        jrAnnotation=jrData[['annotation']]
        bundleSession=jrData[['session']]
        uttCode=jr$data$annotation$name
        if(debugLevel>3){
          #cat("Save bundle ",names(jr$data),"\n");
          cat("Save bundle ",uttCode," from session ",bundleSession,"\n");
          
        }
        ssffFiles=jr[['data']][['ssffFiles']]
        oldBundle=get.bundle(database,bundleSession,uttCode)
        
        # warnings as errors
        warnOptionSave=getOption('warn')
        options('warn'=2)
        
        if(is.null(oldBundle)){
          # error
          m=paste('Could not load bundle ',bundleSession,uttCode)
          responseBundle=list(status=list(type='ERROR',message=m),callbackID=jr[['callbackID']],responseContent='status',contentType='text/json')
        }else{
          for(ssffFile in ssffFiles){
            #cat("SSFF file: ",ssffFile[['ssffTrackName']],"\n")
            for(ssffTrackDef in database[['DBconfig']][['ssffTrackDefinitions']]){
              if(ssffTrackDef[['name']]==ssffFile[['ssffTrackName']]){
                ssffTrackExt=ssffTrackDef[['fileExtension']]
                extPatt=paste0('[.]',ssffTrackExt,'$')
                # TODO store signal paths in a better way!
                for(sp in oldBundle[['signalpaths']]){
                  if(grepl(extPatt,sp)){
                    # store
                    if(debugLevel>3){
                      cat("Writing SSFF track ",ssffFile[['ssffTrackName']]," to file: ",sp,"\n")
                    }
                    # Hmm. does not work: missing file argument
                    #base64decode(ssffFile[['data']],output=sp)
                    
                    ssffTrackBin=base64decode(ssffFile[['data']])
                    writeBin(ssffTrackBin,sp)
                  }
                }
              }
            } 
          }
          bundleData=jr[['data']][['annotation']]
          bundle=as.bundle(bundleData=bundleData)
          bundle[['sessionName']]=bundleSession
          responseBundleJSON=NULL
          
          sendErr<-function(e){
            
            # add the error to the message..
            m=e[['message']]
            # ..and the last warning, if available
            wns=warnings()
            wMsgs=names(wns)
            if(length(wMsgs)){
              m=paste(m,wMsgs[[1]])
            }
            cat(m,"\n")
            responseBundle=list(status=list(type='ERROR',message=m),callbackID=jr$callbackID,responseContent='status',contentType='text/json')
            responseBundleJSON=jsonlite::toJSON(responseBundle,auto_unbox=TRUE,force=TRUE,pretty=TRUE)
            result=ws$send(responseBundleJSON)
            return('sent-error')
          }
          #res=store.bundle.annotation(database,bundle)
          res=tryCatch(store.bundle.annotation(database,bundle),error=function(e) e)
          if(inherits(res,'error')){
            
            # prepare e amessage to send to server. 
            # Add the error to the message...
            m=res[['message']]
            # print to console
            cat('Error: ',m,"\n")
            ## ...and the last warning, if available (the error message alone is often not sufficient to describe the problem)
            #wns=warnings()
            #wMsgs=names(wns)
            #if(length(wMsgs)){
            #  m=paste(m,wMsgs[[1]],sep=',')
            #}
            responseBundle=list(status=list(type='ERROR',message=m),callbackID=jr$callbackID,responseContent='status',contentType='text/json')
          }else if(is.null(res)){
            cat("Error: function store.bundle.annotation returned NULL result\n")
          }else{
            database<<-res
            responseBundle=list(status=list(type='SUCCESS'),callbackID=jr$callbackID,responseContent='status',contentType='text/json')
          }
        }
        responseBundleJSON=jsonlite::toJSON(responseBundle,auto_unbox=TRUE,force=TRUE,pretty=TRUE)
        result=ws$send(responseBundleJSON)
        # restore warn level
        options(warn=warnOptionSave)
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
  return(database)
}


