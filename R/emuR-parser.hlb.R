requireNamespace("stringr", quietly = T)

## Parser for EMU HLB hierarchy files
## 
## @param database the database object
## @param hlbFilePath file path to EMU HLB file
## @param levelDefinitions list of annotation level definitions
## @param levels list of already existing annotation levels
## @return emuDB database object including parsed hlb file
## @import stringr
## @author Klaus Jaensch
## @keywords emuR Emu hierarchy hlb
## 
parse_hlbFile <- function(hlbFilePath=NULL,levelDefinitions,levels,encoding=NULL) {
  EMU_HIERARCHY_HEADER = "**EMU hierarchical labels**"
  hlbTiers=list()
  
  if(is.null(hlbFilePath)){
    stop("Argument hlbFilepath must not be NULL\n")
  }
  
  # read
  if(is.null(encoding)){
    lines = try(readLines(hlbFilePath))
  }else{
    lines = try(readLines(hlbFilePath,encoding=encoding))
  }
  if(class(lines) == "try-error") {
    stop("Cannot read from file ", hlbFilePath)
  }
  lineCount=length(lines)  
  
  # assume header in line 1
  #if(lines[[1]]!=EMU_HIERARCHY_HEADER){
  # ALC EMU Db has trailing blank in header line 
  headerPattern=paste0(gsub('*','[*]',EMU_HIERARCHY_HEADER,fixed=TRUE),'[:blank:]*')
  # check header
  if(!grepl(headerPattern,lines[[1]])){
    stop("No Emu Hlb file header found! ", hlbFilePath)
  }
  
  links=list()
  items=list()
  # assume max id value in line 2
  maxId=as.integer(lines[[2]])
  #cat("Max ID: ",maxId,"\n")
  
  # initialize vars
  currentTierName=NULL
  newTier=NULL
  currentLevelDef=NULL
  currentitems=list()
  currentExistingItems=NULL
  currentIdx=1
  
  # parse through lines
  for(lnr in 3:lineCount){
    line=lines[[lnr]]
    if(line==''){
      if(!is.null(currentTierName)){
        # look for already existing (labfile) tier
        tierExists=FALSE
        for(t in levels){
          if(t$name==currentTierName){
            tierExists=TRUE
            # TODO check items
            
            # for now I assume that every item in ESPS label file has corresponding item in hlb file and vice versa
            #merge
            labitems=t$items
            currentExistingItems=labitems
            labitemsCount=length(labitems)
            currentitemsCount=length(currentitems)
            if(labitemsCount!=currentitemsCount){
              stop("Tier: ",currentTierName,": count of items (",currentitemsCount,") in HLB file '",hlbFilePath,"'' differs from count in ESPS label file (",labitemsCount,")");
            }
            #newTier=clone.bundle.level(t);
            newTier=t
            class(newTier) <- 'emuR.annotation.model.Level'
            break
          }
        }
        if(!tierExists){
          currentExistingItems=NULL
          newTier=list(name=currentTierName,type='ITEM',sampleRate=NULL,items=currentitems);
          
          #cat("Level Tier: ",newTier$name,length(newTier$items)," items\n")
        }else{
          #newTier$items=currentitems
          newItems=list()
          # TODO !!
          exItems=newTier$items
          exItemsLen=length(exItems)
          currItemsLen=length(currentitems)
          if(exItemsLen!=currItemsLen){
            # TODO more verbose
            stop("Existing item count mismatch: ",exItemsLen," != ",currItemsLen)
          }
          i=0
          for(i in 1:exItemsLen){
            exItem=exItems[[i]]
            currItem=currentitems[[i]]
            exType=exItem$type
            # cl=class(exItem)[[1]]
            # merge labels
            mergedLabels=exItem[['labels']]
            for(itLbl in currItem[['labels']]){
              for(exLabel in exItem[['labels']]){
                if(exLabel[['name']]==itLbl[['name']]){
                  # label exists, check equality
                  exLblVal=exLabel[['value']]
                  if(is.null(exLblVal)){
                    exLblVal=''
                  }
                  itLblVal=itLbl[['value']]
                  if(exLblVal!=itLblVal){
                    stop("Labels of attribute level '",exLabel[['name']],"' differ: '",exLabel[['value']],"' '",itLbl[['value']],"' in HLB file: '",hlbFilePath,"' line ",lnr,".\n")
                  }
                }else{
                  # merge
                  mergedLabels[[length(mergedLabels)+1]]=itLbl
                }
              }
              
            }
            # if(cl=='emuR.annotation.model.IntervalItem'){
            if(newTier$type == "SEGMENT"){
              newItems[[i]]=list(id=currItem$id,sampleStart=exItem$sampleStart,sampleDur=exItem$sampleDur,labels=mergedLabels)
            }else if(newTier$type == "EVENT"){
              newItems[[i]]=list(id=currItem$id,samplePoint=exItem$samplePoint,labels=mergedLabels)
            }
          }
          newTier$items=newItems
        }
        hlbTiers[[length(hlbTiers)+1]]=newTier
      }
      currentTierName=NULL
      currentLevelDef=NULL
      currentitems=list()
      currentIdx=1
    }
    lineTokensLst=strsplit(line,' ')
    lineTokens=lineTokensLst[[1]]
    lineTokenCount=length(lineTokens);
    if(lineTokenCount>=1){
      firstTk=lineTokens[[1]]
      if(!is.null(currentTierName)){
        # 
        idStr=firstTk
        id=as.integer(idStr)
        if(lineTokenCount<2){
          stop("Missing label for id: ",id," in HLB file: '",hlbFilePath,"' line ",lnr," !\n")
        }
        label=lineTokens[[2]]
        labels=NULL
        if(!is.null(items[[idStr]])){
          stop("Duplicate item id: ",id," in HLB file: '",hlbFilePath,"' line ",lnr," !\n")
        }
        
        attrs=list()
        # Add label of tier name
        #attrs[[currentTierName]]=label
        attrs[[length(attrs)+1]]=list(name=currentTierName,value=label)
        
        # add optional other labels
        if(lineTokenCount>=3){  
          for(ti in 3:lineTokenCount){
            attrDefCnt=length(currentLevelDef[['attributeDefinitions']])
            
            # ALC db has not the same sequence of attributes in .tpl and .hlb files
            # set the label key/values according to the order in HLB file 
            for(attrIdx in 1:attrDefCnt){
              ldAttrDef=currentLevelDef[['attributeDefinitions']][[attrIdx]]
              #cat(attrDefSeq)
              if(ldAttrDef[['name']]==attrDefSeq[ti-2]){
                l=lineTokens[[ti]]
                #cat("adding label: ",l," line ",line,"\n")
                #attrs[[ldAttrDef$name]]=l
                attrs[[attrIdx]]=list(name=ldAttrDef[['name']],value=l)
              }
            }
            
          }
        }
        id=as.integer(firstTk)
        item=NULL
        #if(!is.null(currentExistingItems)){
        #  exItem=currentExistingItems[[currentIdx]]
        #  item=emuR.annotation.model.Item.setId(exItem,id)
        #}else{
        item=list(id=id,labels=attrs)
        #}
        #currentIdx=currentIdx+1
        currentitems[[length(currentitems)+1]]=item
        
        items[[firstTk]]=item
      }
      
      for(td in levelDefinitions){
        
        if(td$name==firstTk){
          currentTierName=firstTk
          currentLevelDef=td
          if(lineTokenCount>=2){
            attrDefSeq=c()
            for(ti in 2:lineTokenCount){
              lblNi=ti-1
              tk=lineTokens[[ti]]
              
              ldAttrDef=NULL
              if(tk!=currentTierName){
                # ALC db has not the same sequence of attributes in .tpl and .hlb files
                #ldAttrDef=td$attributeDefinitions[[lblNi]]
                for(attrDef in td[['attributeDefinitions']]){
                  if(attrDef[['name']]==tk){
                    ldAttrDef=attrDef
                    # create sequnece order of label names (keys) from HLB file 
                    attrDefSeq=c(attrDefSeq,tk)
                    break
                  } 
                }
                tdLblName=ldAttrDef[['name']]
                #for(t1 in td$labelNames){
                # cat("Labelname: ",t1,"\n")
                #}
                #cat("Comp: ",tk," ",tdLblName,"\n")
                if(is.null(ldAttrDef)){
                  stop("Label name ",tk," has no declaration in level definition. '",hlbFilePath,"' line ",lnr,": ",line)
                }
                
                # This should never happen, the code can be removed safely
                if(tdLblName!=tk){
                  stop("Label name ",tk," does not match label name ",tdLblName," of level definition. '",hlbFilePath,"' line ",lnr,": ",line)
                }
              }
            }
          }else{
            stop("Missing label name in '",hlbFilePath,"' line ",lnr,": ",line)
          }
          break
        }
      }
      if(is.null(currentTierName)){
        # Link line
        #cat("Link line: ",line,"\n")
        fromIdStr=firstTk
        fromId=as.integer(fromIdStr)
        #fromItem=items[[fromIdStr]]
        if(lineTokenCount>=2){
          for(ti in 2:lineTokenCount){
            
            toIdStr=lineTokens[[ti]]
            toId=as.integer(toIdStr)
            #toItem=items[[toIdStr]]
            
            #links[[length(links)+1]]=emuR.annotation.model.Link(fromItem,toItem)
            links[[length(links)+1]]=list(fromID=fromId,toID=toId)
            
            #cat("Link: ",fromId,toId,"\n")
          }
        }
        
      }
    }
    
  }
  
  # Add levels which are not used in HLB file
  for(l in levels){
    ln=l$name
    found=FALSE
    for(hlbLvl in hlbTiers){
      hlbLvlNm=hlbLvl$name
      if(hlbLvlNm==ln){
        found=TRUE
        break
      }
    }
    if(!found){
      hlbTiers[[length(hlbTiers)+1]]=l
    }
  }
  result=list(hlbTiers=hlbTiers,links=links)
  return(result)
}
