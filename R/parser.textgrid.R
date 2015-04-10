require(stringr)
require(RSQLite)

## Parser for Praat TextGrid files
## 
## @param textGridCon TextGrid file connection
## @param sampleRate sample rate of correponding signal file
## @param encoding text encoding (currently the only excepted is the default UTF-8)
## @param dbName name of emuDB that the item/label entries will be written to
## @param bundle name of bundle 
## @param session name of session
## @param dbUUID optional UUID of emuDB
## @return an annoation object
## @author Klaus Jaensch, Raphael Winkelmann
## @import stringr RSQLite
## @keywords emuR TextGrid Praat Emu
## 
parse.textgrid <- function(textGridCon=NULL, sampleRate, encoding="UTF-8", 
                           dbName=NULL, bundle=NULL, session="0000", dbUUID = NULL) {
  
  #####################
  # check arguments (TODO better checks for classes and the like...)
  
  if(is.null(textGridCon)) {
    stop("Argument textGridCon must not be NULL\n")
  }
  if(sampleRate <=0 ){
    stop("Samplerate must be greater than zero\n")
  }
  if(encoding != "UTF-8"){
    stop("The only encoding that is currently supported is UTF-8\n")
  }
  if(is.null(dbName)){
    stop("Argument db must not be NULL!\n")
  }
  if(is.null(bundle)){
    stop("Argument bundle must not be NULL!\n")
  }
  if(is.null(session)){
    stop("Argument session must not be NULL!\n")
  }
  
  # get dbObj
  dbUUID = get.emuDB.UUID(dbName = dbName, dbUUID = dbUUID)
  dbObj = .load.emuDB.DBI(uuid = dbUUID)
  
  
  #
  #####################
  
  itemCounterGlobal = 1
  itemCounterLevel = 1
  
  FILE_TYPE_KEY="File type"
  OBJECT_CLASS_KEY="Object class"
  TIERS_SIZE_KEY="size"
  TIER_ITEM_KEY="item"
  NAME_KEY="name"
  INTERVALS_KEY="intervals"
  POINTS_KEY="points"
  XMIN_KEY="xmin"
  XMAX_KEY="xmax"
  TEXT_KEY="text"
  TIME_KEY="time"
  
  FILE_TYPE_VAL_OO_TEXTFILE="ooTextFile"
  OBJECT_CLASS_VAL_TEXTGRID="TextGrid"
  TIER_CLASS_VAL_INTERVAL="IntervalTier"
  TIER_CLASS_VAL_TEXT="TextTier"
  
  fileType=NULL
  objectClass=NULL
  hasTiers=FALSE
  tiersCount=NULL
  currentTier=NULL
  currentTierClass=NULL
  currentTierName=NULL
  currentTierSize=NULL
  
  #   preallocItemDf = data.frame(id='', session='', bundle='', level='',
  #                               itemID=-1, type='', seqIdx=-1, sampleRate=-1, 
  #                               samplePoint=-1, sampleStart=-1, sampleDur=-1, label='', stringsAsFactors=FALSE)
  #   
  #   preallocLabelDf = data.frame(itemID='', session='', bundle='',
  #                                labelIdx=-1, name='', label='', stringsAsFactors=FALSE)
  
  
  
  # read TextGrid
  tg = try(readLines(textGridCon))
  if(class(tg) == "try-error") {
    stop("read.TextGrid: cannot read from file ", textGridCon)
  }
  
  # remove all trailing/leading white spaces (for speed improvment)
  tg = gsub("^\\s+|\\s+$", "", tg)
  
  for(line in tg){
    # check for fileType
    if(is.null(fileType)){
      p=parse.line.to.key.value(line,doubleQuoted=TRUE, initialTrim=FALSE)
      if(! is.null(p)){
        if(p[1]==FILE_TYPE_KEY){
          #cat("Found file type: ",p[2],"\n")
          fileType=p[2]
        }
      }
    }else{
      # check for objectClass
      if(is.null(objectClass)){
        p=parse.line.to.key.value(line,doubleQuoted=TRUE, initialTrim=FALSE)
        if(! is.null(p)){
          if(p[1]==OBJECT_CLASS_KEY){
            #cat("Found object class: ",p[2],"\n")
            objectClass=p[2]
          }
        }
      }else{
        # if we have both the file type and the object class        
        if((fileType==FILE_TYPE_VAL_OO_TEXTFILE) && (objectClass==OBJECT_CLASS_VAL_TEXTGRID)){
          
          #if(!hasTiers){
          #  Property p=parseProperty(line,"\\s+", false);
          #  hasTiers=(p!=null && p.key.equals("tiers?") && p.value.equals("<exists>"));
          #}else{
          if(is.null(tiersCount)){
            
            p=parse.line.to.key.value(line, initialTrim=FALSE)
            if((!is.null(p)) && (p[1]=='size')){
              #cat("Found number of tiers (size=X): ",p[2],"\n")
              tiersCount=p[2]
            }
          }else{
            ## if we have tiersCount tiers
            if(length(grep("^item",line))==1){
              
              tierIndexStr=sub('item\\s*','',line);
              #cat("Tier indxstr: ",tierIndexStr,"\n")
              tierIndexStr=sub('\\s*:$','',tierIndexStr);
              if(length(grep('\\[\\s*[0-9]+\\s*\\]',tierIndexStr))==1){
                tierIndexStr=sub('\\[\\s*','',tierIndexStr);
                tierIndexStr=sub('\\s*\\]','',tierIndexStr);
                
                tierIndex=tierIndexStr;
                #cat("Tier indx:",tierIndex,"\n")
                #cat("Tiers1: ",length(tiers),"\n")
                #if(!is.null(currentTier)){
                #cat("Tiers2: ",length(tiers),"\n")
                # TODO use tier name !!!
                #levels[[currentTier[['name']]]]=currentTier
                #}
                #currentTier=create.bundle.level(sampleRate=sampleRate)
                
                #tiers[[length(tiers)+1]] <- currentTier                       
                #    currentTier=new Tier();
                #    
                
                
                # reset level/tier attributes
                itemCounterLevel = 1
                currentTierClass=NULL;
                currentTierName=NULL;
                currentTierSize=NULL;
                currentSegment=NULL;
                currentSegmentIndex=NULL;
                currentSegmentStart=NULL;
                currentSegmentDur=NULL;
                currentSegmentLabel=NULL;
                currentMark=NULL;
                currentPointIndex=NULL;
                currentPointSample=NULL;
                currentPointLabel=NULL;
              }
            }else {
              # check for currentTierClass
              if(is.null(currentTierClass)){
                p=parse.line.to.key.value(line,doubleQuoted=TRUE, initialTrim=FALSE)
                if((! is.null(p)) && ('class' == p[1])){
                  currentTierClass=p[2];
                  if(currentTierClass==TIER_CLASS_VAL_INTERVAL){
                    #currentTier$type='SEGMENT';
                    #annotation.getTiers().add(currentTier);
                    
                  }else if(currentTierClass==TIER_CLASS_VAL_TEXT){
                    #currentTier$type='EVENT';
                    #annotation.getTiers().add(currentTier);
                    
                  }else{
                    stop("TextGrid tiers of class \"",currentTierClass,"\" not supported!");
                  }
                }
              }
              # check for currentTierName
              if(is.null(currentTierName)){
                p=parse.line.to.key.value(line,doubleQuoted=TRUE, initialTrim=FALSE)
                if((! is.null(p)) && ('name' == p[1])){
                  currentTierName=p[2];
                  #currentTier$name=currentTierName;
                  #cat("Tier name:",currentTierName,currentTier$TierName,tiers[[length(tiers)]]$TierName,"\n")
                  
                }
              }
              # if we have the currentTierClass
              if(!is.null(currentTierClass)){
                if(currentTierClass==TIER_CLASS_VAL_INTERVAL){
                  #currentTier.setType(Tier.Type.INTERVAL);
                  #cat("Interval tier check line 2 : ",line,"\n")
                  # find size (and other properties)
                  if((is.null(currentTierSize)) && (length(grep('^intervals[[:space:]]*:.*',line))==1)){
                    
                    intervalsPropertyStr=str_trim(sub('^intervals[[:space:]]*:','',line))
                    #cat("Intervals prop str: ",intervalsPropertyStr,"\n")
                    intervalsProperty=parse.line.to.key.value(intervalsPropertyStr,initialTrim=FALSE);
                    if((!is.null(intervalsProperty)) && (intervalsProperty[1]=='size')){
                      currentTierSize=intervalsProperty[2]
                      #cat("intervals: size=",currentTierSize,"\n");
                      
                    }
                  }
                  #cat("grep('intervals[[:space:]]*[[][[:space:]]*[0-9]+[[:space:]]*[]][[:space:]]*[:][[:space:]]*'",line,"\n");
                  if(length(grep('intervals[[:space:]]*[[][[:space:]]*[0-9]+[[:space:]]*[]][[:space:]]*[:][[:space:]]*',line))==1){
                    
                    #cat("Interval !!\n");
                    segmentIndexStr=sub("intervals[[:space:]]*[[][[:space:]]*","",line);
                    segmentIndexStr=sub("[[:space:]]*[]][[:space:]]*[:][[:space:]]*","",segmentIndexStr);
                    currentElementIndex=segmentIndexStr;
                    
                    #currentSegment=emuR.annotation.model.IntervalItem()
                    
                    currentSegmentIndex=segmentIndexStr;
                    currentSegmentStart=NULL;
                    currentSegmentEnd=NULL;
                    currentSegmentLabel=NULL;
                    
                    #cat("Events: ",length(currentTier$events),"\n")
                    #currentSegment.setSampleRate(sampleRate);
                    #currentTier.getSegments().add(currentSegment);
                    #currentSegment.setTier(currentTier);
                    #if(DEBUG)System.out.println("Interval: "+currentElementIndex);
                    
                  }else{
                    p=parse.line.to.key.value(line, doubleQuoted=TRUE, initialTrim=FALSE)
                    if((!is.null(p)) && (!is.null(currentSegmentIndex))){
                      if(p[1] == "xmin"){
                        minTimeStr=p[2]
                        minTime=as(minTimeStr,"numeric")
                        startSample = floor(minTime * sampleRate)
                        currentSegmentStart=startSample
                        #long minFrames=timeToFrames(minTime);
                        #currentSegment.setBegin(minFrames);
                        #if(DEBUG)System.out.println("Segment: "+currentSegment);
                      }else if(p[1]=="xmax"){
                        maxTimeStr=p[2];
                        #long maxFrames=timeToFrames(maxTime);
                        #currentSegment.setEnd(maxFrames);
                        #if(DEBUG)System.out.println("Segment: "+currentSegment);
                        maxTime=as(maxTimeStr,"numeric")
                        currentSegmentEnd = floor(maxTime * sampleRate)
                        
                        
                      }else if(p[1]=="text"){
                        #currentSegment.setLabel(p.value);
                        #if(DEBUG)System.out.println("Segment: "+currentSegment);
                        label=p[2];
                        currentSegmentLabel=label;
                        #cat("Found label: ",label,"\n")
                      }
                      
                      if(!is.null(currentSegmentIndex) && 
                           !is.null(currentSegmentStart) &&
                           !is.null(currentSegmentEnd) &&
                           !is.null(currentSegmentLabel)){
                        #cat("New segment\n");
                        sampleDur = currentSegmentEnd - currentSegmentStart - 1
                        labels=list(list(name=currentTierName,value=currentSegmentLabel))
                        
                        
                        # item entry:
                        dbSendQuery(emuDBs.con, paste0("INSERT INTO items VALUES"," ('", dbUUID, "', '", session, "', '", bundle, "', '", itemCounterGlobal, 
                                                       "', '", currentTierName, "', '", "SEGMENT", 
                                                       "', ", itemCounterLevel, ", ", sampleRate, ", ", "NULL", ", ", currentSegmentStart, 
                                                       ", ", sampleDur, ")"))
                        
                        
                        
                        # label entry:
                        dbSendQuery(emuDBs.con, paste0("INSERT INTO labels VALUES","('", 
                                                       dbUUID, "', '", session, "', '", bundle, "',", itemCounterGlobal,
                                                       ", ", 0,", '", currentTierName, "', '", gsub("'","''", currentSegmentLabel), "')"))
                        
                        # links entry:
                        # no link entry because TextGrids don't have hierarchical infos
                        
                        # increase counters
                        itemCounterGlobal = itemCounterGlobal + 1
                        itemCounterLevel = itemCounterLevel + 1
                        
                        
                        currentSegment=NULL;
                        currentSegmentIndex=NULL;
                        currentSegmentStart=NULL;
                        currentSegmentDur=NULL;
                      }
                      
                    }
                  }
                  
                }else if(currentTierClass==TIER_CLASS_VAL_TEXT){
                  #currentTier.setType(Tier.Type.POINT);
                  
                  # find size (and other properties)
                  if((is.null(currentTierSize)) && (length(grep('^points[[:space:]]*[:].*',line))==1)){
                    
                    intervalsPropertyStr=str_trim(sub('^points[[:space:]]*[:]','',line))
                    #cat("Ivp:",intervalsPropertyStr,"\n")
                    intervalsProperty=parse.line.to.key.value(intervalsPropertyStr, initialTrim=FALSE);
                    if((!is.null(intervalsProperty)) && (intervalsProperty[1]=='size')){
                      currentTierSize=intervalsProperty[2]
                      #cat("points: size=",currentTierSize,"\n");
                      
                    }
                  }
                  if(length(grep("points[[:space:]]*[[][[:space:]]*[0-9]+[[:space:]]*[]][[:space:]]*[:][[:space:]]*",line))==1){
                    pointIndexStr=sub("points[[:space:]]*[[][[:space:]]*","",line);
                    pointIndexStr=sub("[[:space:]]*[]][[:space:]]*[:][[:space:]]*","",pointIndexStr);
                    currentPointIndex=as.integer(pointIndexStr)
                    currentElementIndex=currentPointIndex
                    currentPointLabel=NULL;
                    currentPointSample=NULL;
                  }else{
                    #cat("inside point: \n")
                    p=parse.line.to.key.value(line,doubleQuoted=TRUE, initialTrim=FALSE)
                    if((!is.null(p)) && (!is.null(currentPointIndex))){
                      if(p[1]=="time" || p[1]=="number"){
                        timePointStr=p[2];
                        timePoint=as(timePointStr,"numeric")
                        samplePoint = floor(timePoint * sampleRate)
                        currentPointSample=samplePoint
                        #cat("point sample: ",currentPointSample,"\n")
                        #    long frames=timeToFrames(time);
                        #    currentMark.setPosition(frames);
                      }else if(p[1]=="mark"){
                        currentPointLabel=p[2]
                        # cat("point label: ",currentPointLabel,"\n")
                      }else if(p[1]=="text"){
                        currentPointLabel=p[2]
                      }
                    }
                    if(!is.null(currentPointIndex) && 
                         !is.null(currentPointSample) &&
                         !is.null(currentPointLabel)){
                      
                      labels=list(list(name=currentTierName,value=currentPointLabel))
                      
                      # item entry
                      itemId = paste0(dbName, '_', session, '_', bundle, '_', itemCounterGlobal)
                      
                      
                      dbSendQuery(emuDBs.con, paste0("INSERT INTO items VALUES"," ('", dbUUID, "', '", session, "', '", bundle, "', ",
                                                     itemCounterGlobal, ", '", currentTierName,"', '", "EVENT", 
                                                     "', ", itemCounterLevel, ", ", sampleRate, ", ", currentPointSample, ", ", "NULL", 
                                                     ", ", "NULL", ")"))
                      
                      
                      # label entry:
                      dbSendQuery(emuDBs.con, paste0("INSERT INTO labels VALUES","('", 
                                                     dbUUID, "', '", session, "', '", bundle, "',", itemCounterGlobal,
                                                     ", ", 0,", '", currentTierName, "', '", gsub("'","''", currentPointLabel), "')"))              
                      
                      
                      
                      # links entry:
                      # no link entry because TextGrids don't have hierarchical infos
                      
                      # increase counters
                      itemCounterGlobal = itemCounterGlobal + 1
                      itemCounterLevel = itemCounterLevel + 1
                      
                      currentPointIndex=NULL;
                      currentPointLabel=NULL;
                      currentPointSample=NULL;
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
  if(inherits(textGridCon,"connection")){
    close(textGridCon)
  }
}

# FOR DEVELOPMENT
# library('testthat')
# test_file('tests/testthat/test_parse.textgrid.R')

