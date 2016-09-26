#######################################################
# annotJSON representation to annotDFs conversion functions

# convert annotJSON to list of data.frames including 
# meta information (name, annotates, samplerate)
annotJSONcharToBundleAnnotDFs <- function(annotJSONchar){

  jsonObj = jsonlite::fromJSON(annotJSONchar, simplifyVector=F) # SIC pass in path 2 json instead -> a bit faster

  ##############################
  # extract items

  # extract all IDs
  allIds = unlist(lapply(jsonObj$levels, function(level){
    allIds = sapply(level$items, function(it) {
      it$id
    })
  }))
  if(is.null(allIds)) allIds = integer()
  
  # extract all levels
  allLevels = unlist(lapply(jsonObj$levels, function(level){
    allLevels = sapply(level$items, function(it) {
      level$name
    })
  }))
  if(is.null(allLevels)) allLevels = character()
  
  # extract all types
  allTypes = unlist(lapply(jsonObj$levels, function(level){
    allTypes = sapply(level$items, function(it) {
      level$type
    })
  }))
  if(is.null(allTypes)) allTypes = character()
  
  # extract all seq_idx
  allSeqIdx = unlist(lapply(jsonObj$levels, function(level){
    curIdx = 0
    allIdx = sapply(level$items, function(it) {
      curIdx <<- curIdx + 1
      curIdx
    })
  }))
  if(is.null(allSeqIdx)) allSeqIdx = integer()
  
  # extract all sample points
  allSamplePoints = unlist(lapply(jsonObj$levels, function(level){
    allSamplePoints = sapply(level$items, function(it) {
      if(is.null(it$samplePoint)){
        return(NA)
      }else{
        return(it$samplePoint)
      }
    })
  }))
  if(is.null(allSamplePoints)) allSamplePoints = integer()
  
  # extract all sample start
  allSampleStarts = unlist(lapply(jsonObj$levels, function(level){
    allSampleStarts = sapply(level$items, function(it) {
      if(is.null(it$sampleStart)){
        return(NA)
      }else{
        return(it$sampleStart)
      }
    })
  }))
  if(is.null(allSampleStarts)) allSampleStarts = integer()
  
  # extract all sample durs
  allSampleDurs = unlist(lapply(jsonObj$levels, function(level){
    allSampleDurs = sapply(level$items, function(it) {
      if(is.null(it$sampleDur)){
        return(NA)
      }else{
        return(it$sampleDur)
      }
    })
  }))
  if(is.null(allSampleDurs)) allSampleDurs = integer()
  
  items = data.frame(item_id = allIds, level = allLevels, type = allTypes, 
                     seq_idx = allSeqIdx, sample_rate = rep(jsonObj$sampleRate, length(allIds)),
                     sample_point = allSamplePoints, sample_start = allSampleStarts,
                     sample_dur = allSampleDurs, stringsAsFactors = F)
  
  ##############################
  # extract links
  
  # extract all from ids
  allFromIds = sapply(jsonObj$links, function(l) l$fromID)
  if(is.null(allFromIds)) allFromIds = integer()
  # extract all to ids
  allToIds = sapply(jsonObj$links, function(l) l$toID)
  if(is.null(allToIds)) allToIds = integer()
  
  links = data.frame(from_id = allFromIds, to_id = allToIds, stringsAsFactors = F)
                     

  ##############################
  # extract labels

  # extract all label item ids
  allLabelItemIds = unlist(lapply(jsonObj$levels, function(level){
    allLabelItemIds = lapply(level$items, function(it) {
      allLabelItemIds = lapply(it$labels, function(l) it$id)
    })
  }))
  if(is.null(allLabelItemIds)) allLabelItemIds = integer()
  
  # extract all label label idx
  allLabelLabelIdx = unlist(lapply(jsonObj$levels, function(level){
    allLabelLabelIdx = lapply(level$items, function(it) {
      curIdx = 0
      allLabelLabelIdx = lapply(it$labels, function(l) {
        curIdx <<- curIdx + 1
        curIdx
      })
    })
  }))
  if(is.null(allLabelLabelIdx)) allLabelLabelIdx = integer()
  
  # extract all label names
  allLabelNames = unlist(lapply(jsonObj$levels, function(level){
    allLabelNames = lapply(level$items, function(it) {
      allLabelNames = lapply(it$labels, function(l) l$name)
    })
  }))
  if(is.null(allLabelNames)) allLabelNames = character()
  
  # extract all label values
  allLabelValues = unlist(lapply(jsonObj$levels, function(level){
    allLabelValues = lapply(level$items, function(it) {
      allLabelValues = lapply(it$labels, function(l) l$value)
    })
  }))
  if(is.null(allLabelValues)) allLabelValues = character()
  
  labels = data.frame(item_id = allLabelItemIds, label_idx = allLabelLabelIdx, name = allLabelNames, label = allLabelValues, stringsAsFactors = F)
                 
  return(list(name = jsonObj$name, annotates = jsonObj$annotates, sampleRate = jsonObj$sampleRate, items = items, links = links, labels = labels))
  
}

# convert annotDFs (annotation list of data.frame representation) to annotJSON
bundleAnnotDFsToAnnotJSONchar <- function(emuDBhandle, annotDFs){
  # load DBconfig to generate levelNames vector (although levels are not ordered per say)
  levelDefs = list_levelDefinitions(emuDBhandle)
  
  levels = list()
  
  for(l in levelDefs$name){
    levelItems = dplyr::filter_(annotDFs$items, ~(level == l))
    
    levels[[length(levels) + 1]] = list(
      items = apply(levelItems, 1, function(r) {
      
      labels = apply(dplyr::filter_(annotDFs$labels, ~(item_id == as.numeric(r[1]))), 1, function(r2) list(name = as.character(r2[3]), value = as.character(r2[4])))
      res = NULL
      if(r[3] == "ITEM"){
        res = list(id = as.numeric(r[1]),
                   labels = labels)
      }else if(r[3] == "SEGMENT"){
        res = list(id = as.numeric(r[1]),
                   sampleStart = as.numeric(r[7]),
                   sampleDur = as.numeric(r[8]),
                   labels = labels)
      }else if(r[3] == "EVENT"){
        res = list(id = as.numeric(r[1]),
                   samplePoint = as.numeric(r[6]),
                   labels = labels)
      }
      return(res)
    }),
    name = l,
    type = levelDefs$type[levelDefs$name == l]
    )
  }
  
  if(nrow(annotDFs$links) >0){
    links = apply(annotDFs$links, 1, function(r) list(fromID = as.numeric(r[1]), toID = as.numeric(r[2])))
  }else{
    links = list()
  }
  
  # reset null entries of empty items to list for correct cohesion into json
  for(i in 1:length(levels)){
    if(is.null(levels[[i]]$items)){
      levels[[i]]$items = list()
    }
  }
  
  annotJSON = list(name = annotDFs$name,
                   annotates = annotDFs$annotates,
                   sampleRate = annotDFs$sampleRate,
                   levels = levels, links = links)
  
  return(jsonlite::toJSON(annotJSON, auto_unbox = T, force = T, pretty = T))
}

