
# Prepares object hierarchy for persistence (conversion to JSON)
marshal.for.persistence <- function(x, filter=NULL){
  if (is.list(x)) {
    
    cloneList=list()
    class(cloneList) <- class(x)
    nms=names(x)
    if(is.null(nms)){
      
      # pass through
      len=length(x)
      if(len>0){
        for (i in 1:len) {
          cloneList[[i]]=marshal.for.persistence(x[[i]],filter=filter);
        }
      }
    }else{
      if(any(duplicated(nms))){
        stop("Cannot handle list with duplicate names.")
      }
      # TODO if the value is null the key ist not in names
      
      persistAttr=attr(x,'ips.persist')
      refNms=list()
      refs=NULL
      typesJSON=NULL
      typesJSONnms=list()
      if(!is.null(persistAttr)){
        refs=persistAttr[['refs']]
        if(!is.null(refs)){
          refNms=names(refs)
        }
        typesJSON=persistAttr[['typesJSON']]
        if(!is.null(typesJSON)){
          typesJSONnms=names(typesJSON)
          #cat(typesJSONnms,"\n")
        }
      }
      childFilter=NULL
      for(n in nms){
        
        dropProp=FALSE
        if(!is.null(filter)){
          for(ps in filter){
            p=ps[1]
            if(n==p | p=='*'){
              lenP=length(ps)
              if(lenP>1){
                childSeq=2:lenP
                if(is.null(childFilter)){
                  childFilter=list()
                }
                childFilter[[length(childFilter)+1L]]=ps[childSeq]
                #cat("Added child filter item: ",ps[childSeq],"\n")
              }else{
                
                
                if(!dropProp && n==p){
                  # cat("Dropping property: ",n,"\n")
                  dropProp=TRUE
                }
              }
            }
          }
        }
        if(!dropProp){
          v=x[[n]]
          if(n %in% refNms){
            idFn=refs[[n]]
            if(is.null(idFn)){
              stop("No ID property given") 
            }
            # replace referenced object with ID
            
            # get target ID
            vPersistAttr=attr(v,'ips.persist')
            if(is.null(vPersistAttr)){
              stop("No persistence info for object to convert to ID")
            }
            vLocIdProp=vPersistAttr[['localId']]
            if(is.null(vLocIdProp)){
              stop("No ID property found.")
            }
            vLocId=v[[vLocIdProp]]
            #cat("ID field: ",idFn,vLocIdProp,vLocId,"\n")
            cloneList[[idFn]]=vLocId
          }else if(n %in% typesJSONnms){
            typeJSON=typesJSON[[n]]
            if(!is.null(typeJSON)){
              if(typeJSON=='array'){
                val=marshal.for.persistence(x[[n]],filter=childFilter)
                # convert to array by deletion of names attribute
                attr(val,'names') <-NULL
                cloneList[[n]]=val
                #cat("Array conversion of field ",n,"\n")
              }
            }
            
          }else{
            cloneList[[n]]=marshal.for.persistence(x[[n]],filter=childFilter)
          }
        }
      }
    }
    
    return(cloneList)
  }else{
    #if(is.character(x) & length(x)==1){
    #  # Or use jsonlite::toJSON(...,auto_unbox=TRUE)
    #  return(jsonlite::unbox(x))
    #}else{
    return(x)
    #}
  }
}

# Applies class names to object hierarchy after loading from JSON without type information
unmarshal.from.persistence <- function(x,classMap=list()){
  classNames=names(classMap)
  for(cn in classNames){
    pathes=classMap[[cn]]
    pathesLen=length(pathes)
    if(pathesLen==0){
      x=apply.class(x,c(),cn)
    }else{
      for(path in pathes){
        x=apply.class(x,path,cn)
      }
    }
  }  
  
  return(x);
}
