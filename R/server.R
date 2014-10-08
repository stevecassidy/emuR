require(httpuv)
require(base64enc)

##' Serve EMU database to EMU-Webapp
##' 
##' @description Starts a websocket server and listens on commands from a running EMU-Webapplication browser GUI instance. The server runs in a loop and blocks the R console. Stop the server with the 'Clear' button of the webapp GUI. The server is also stopped when the browser is closed or the webapplication page is reloaded. Communication is defined by EMU-webApp-websocket-protocol version 0.0.1
##' @param database a emuDB database object
##' @param port the port number to listen on (default: 8080)
##' @param debug TRUE to enable debugging (default: no debugging messages)
##' @param debugLevel integer higher values generate more detailed debug output
##' @return the possibly modified Emu database object
##' @import httpuv rjson base64enc
##' @export
##' @author Klaus Jaensch
##' @keywords emuDB EMU-webapp database websocket Emu
##' @examples
##' \dontrun{ 
##' ## Serve database object ae (opens default HTTP/websocket port 8080)
##' 
##' ae=serve(ae)
##' 
##' ## serve database object ae, open HTTP/websocket port 9000
##' 
##' ae=serve(ae,port=9000)
##' 
##' }
##' 
serve<-function(database,port=8080,debug=FALSE,debugLevel=0){
  UseMethod('serve')
}

##' @export
serve.emuDB=function(database,port=8080,debug=FALSE,debugLevel=0){
    if(debug && debugLevel==0){
      debugLevel=2
    }
    emuDBserverRunning=FALSE
   
    if(is(database,'emuDB')){
       
      # create dummy bundle list
      ulNms=attr(database$sessions[[1]]$bundles,'names')
      emuRuttList=list()
      for(ulNm in ulNms){
        emuRuttList[[length(emuRuttList)+1]]=list(name=ulNm)
      }
    }
    else{
      stop("Supported object classes for database: 'emuDB'");
    }
    
    httpRequest = function(req){
      # not used
    }
    
    onHeaders<-function(req){
      return(NULL)
    }
    
    serverEstablished = function(ws){
      if(debugLevel>0){
        cat("emuR websocket service established\n")
      }
      
      serverClosed = function(ws){
       
        if(debugLevel>0){
          cat("emuR websocket service closed\n")
        }
        emuRserverRunning<<-FALSE
       
      }
      
      sendError = function(ws,errMsg,callbackID){
        status=list(type='ERROR',details=errMsg);
        response=list(callbackID=callbackID,status)
        responseJSON=rjson::toJSON(response)
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
        jr=rjson::fromJSON(D)
        if(debugLevel >= 2 ){
          cat("Received command from emuLVC: ",jr$type,"\n")
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
            cat("Received type from emuLVC: ",jr$type,"\n")
          }
          
        }
        if(jr$type == 'GETPROTOCOL'){
          protocolData=list(protocol='EMU-webApp-websocket-protocol',version='0.0.1')
          response=list(status=list(type='SUCCESS'),callbackID=jr$callbackID,data=protocolData)
          responseJSON=rjson::toJSON(response) 
          result=ws$send(responseJSON)
          if(debugLevel >= 2){
            cat("Sent protocol. \n")
          }
          
        }else if(jr$type == 'GETGLOBALDBCONFIG'){
          persistFilter=emuR.persist.filters[['DBconfig']]
          sp=marshal.for.persistence(database[['DBconfig']],persistFilter)
          #sp=WS[['server']][['emuDb']][['DBconfig']]
          if(debugLevel >= 4){
            #cat("Send config: ",sp,"\n")
          }
          response=list(status=list(type='SUCCESS'),callbackID=jr$callbackID,data=sp)
          responseJSON=rjson::toJSON(response) 
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
          responseJSON=rjson::toJSON(response) 
          result=ws$send(responseJSON)
          if(debugLevel >= 2){
            cat("Sent user managment: no. \n")
          }
          
          
        }else if(jr$type == 'GETBUNDLELIST'){
        
          response=list(status=list(type='SUCCESS'),callbackID=jr$callbackID,dataType='uttList',data=emuRuttList)
          responseJSON=rjson::toJSON(response)
          
          if(debugLevel >= 5)cat(responseJSON,"\n")
           result=ws$send(responseJSON)
          if(debugLevel >= 2){
            cat("Sent utterance list with length: ",length(emuRuttList)," \n")
          }
          
        }else if(jr$type == 'GETBUNDLE'){
          
          uttCode=jr$name   
          if(debugLevel>2){
            cat("Requested bundle: ",uttCode,"\n")
          }
          sc=database[['DBconfig']]
          if(debugLevel>3){
            cat("Convert bundle to S3 format",uttCode,"\n")
          }
          b=get.bundle(database,uttCode)
          if(is.null(b)){
            # error
            m=paste('Could not load bundle ',uttCode)
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
            
            ssffTrackDefinitions=list()
            for(ssffTr in sc$ssffTrackDefinitions){
              fe=ssffTr$fileExtension
              feRe=paste0('[.]',fe,'$')
              for(sp in b$signalpaths){
                if(length(grep(feRe,sp))==1){
                  mf<- file(sp, "rb")
                  mfData=readBin(mf, raw(), n=file.info(sp)$size)
                  mfDataBase64=base64encode(mfData)
                  close(mf)
                  ssffDatObj=list(encoding="BASE64",data=mfDataBase64,ssffTrackName=ssffTr$name)
                  ssffTrackDefinitions[[length(ssffTrackDefinitions)+1]]=ssffDatObj
                }
              }
            }
            
            anno=marshal.for.persistence(b,emuR.persist.filters[['bundle']])
            data=list(mediaFile=mediaFile,ssffFiles=ssffTrackDefinitions,annotation=anno)
            
            responseBundle=list(status=list(type='SUCCESS'),callbackID=jr$callbackID,responseContent='bundle',contentType='text/json',data=data)
          }
          responseBundleJSON=rjson::toJSON(responseBundle)
          result=ws$send(responseBundleJSON)
          if(debugLevel >= 2){
            if(debugLevel >=8){
              cat(responseBundleJSON,"\n")
            }
            cat("Sent bundle. \n")
          }
          
        }else if(jr$type == 'SAVEBUNDLE'){
          uttCode=jr$data$annotation$name
          if(debugLevel>3){
            #cat("Save bundle ",names(jr$data),"\n");
            cat("Save bundle ",uttCode,"\n");
            for(ssffFile in jr[['ssffFiles']]){
              cat("SSFF file: ",ssffFile,"\n")
              # TODO save SSFF file
            }
          }
          bundleData=jr[['data']][['annotation']]
          bundle=as.bundle(bundleData=bundleData)
          responseBundleJSON=NULL
          # warnings as errors
          warnOptionSave=getOption('warn')
          options('warn'=2)
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
            responseBundleJSON=rjson::toJSON(responseBundle)
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
          responseBundleJSON=rjson::toJSON(responseBundle)
          result=ws$send(responseBundleJSON)
          # restore warn level
          options(warn=warnOptionSave)
        }else if(jr[['type']]=='DISCONNECTWARNING'){
          response=list(status=list(type='SUCCESS'),callbackID=jr[['callbackID']],responseContent='status',contentType='text/json')
          responseJSON=rjson::toJSON(response)
          result=ws$send(responseJSON)
          emuRserverRunning<<-FALSE
          ws$close()
        }
      }
      ws$onMessage(serverReceive)
      ws$onClose(serverClosed)
    }
    
    app=list(call=httpRequest,onHeaders=onHeaders,onWSOpen=serverEstablished)
    
    sh = startServer(host="0.0.0.0",port=port,app=app)
    if(debugLevel>0){
      cat("HTTP Websocket handle: ",sh,"\n")
    }
   
    emuRserverRunning=TRUE
   
    while(emuRserverRunning) {
      #cat("emuR websocket service...",emuRserverRunning,"\n")
      service()
      Sys.sleep(0.01)
      
    }
    stopServer(sh)
    if(debugLevel>0){
      cat("Closed emuR websocket HTTP service\n")
    }
    return(database)
}


