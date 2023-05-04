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
  
  links = data.frame(from_id = allFromIds, 
                     to_id = allToIds, 
                     stringsAsFactors = F)
  
  
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
  
  labels = data.frame(item_id = allLabelItemIds, 
                      label_idx = allLabelLabelIdx, 
                      name = allLabelNames, 
                      label = allLabelValues, 
                      stringsAsFactors = F)
  
  return(list(name = jsonObj$name, 
              annotates = jsonObj$annotates, 
              sampleRate = jsonObj$sampleRate, 
              items = items, 
              links = links, 
              labels = labels))
  
}

# convert annotDFs (annotation list of data.frame representation) to annotJSON
bundleAnnotDFsToAnnotJSONchar <- function(emuDBhandle, annotDFs){
  # NOTE load_bundleAnnotDFsDBI that produces annotDFs orders the items according to DBconfig
  
  # load DBconfig to generate levelNames vector (although levels are not ordered per say)
  levelDefs = list_levelDefinitions(emuDBhandle)
  
  attrDefs = list_attributeDefinitions(emuDBhandle, levelDefs$name)
  
  levelsdf = dplyr::full_join(annotDFs$items, attrDefs, by=c("level"), multiple="all") %>%
    dplyr::left_join(annotDFs$labels, by=c("item_id", "name")) 
  
  levelsdf$label[is.na(levelsdf$label)] = "" # set missing labels top ""
  
  # convert columns that are split() to factors to prevent reodering
  levelsdf$level = factor(levelsdf$level, levels = unique(levelDefs$name))
  
  levels = split(levelsdf, levelsdf$level) %>%
    purrr::map(function(lev) {
      # convert columns that are split() to factors to prevent reodering
      lev$item_id = factor(lev$item_id, levels = unique(lev$item_id))
      split(lev, lev$item_id)
    }) %>%
    purrr::modify_depth(2, function(df){
      if(unique(df$type.x) == "ITEM"){
        list(id = as.integer(as.character(unique(df$item_id))), 
             labels = data.frame(name = df$name, value = df$label, stringsAsFactors = F))
      }else if(unique(df$type.x) == "SEGMENT"){
        list(id=as.integer(as.character(unique(df$item_id))), 
             sampleStart = unique(df$sample_start), 
             sampleDur = unique(df$sample_dur), 
             labels = data.frame(name = df$name, value = df$label, stringsAsFactors = F))
      }else if(unique(df$type.x) == "EVENT"){
        list(id=as.integer(as.character(unique(df$item_id))), 
             samplePoint = unique(df$sample_point), 
             labels = data.frame(name = df$name, value = df$label, stringsAsFactors = F))
      }
    })
  
  levels = purrr::map2(levels, 1:length(levels), function(itemsList, levelIdx){
    names(itemsList) = NULL # remove items key values
    list(name = levelDefs$name[levelIdx], type = levelDefs$type[levelIdx], items = itemsList)
  })
  
  names(levels) = NULL # remove level key values
  
  # build links
  if(nrow(annotDFs$links) > 0){
    links = data.frame(fromID = annotDFs$links$from_id,
                       toID = annotDFs$links$to_id,
                       stringsAsFactors = F)
  }else{
    links = list()
  }
  
  annotJSON = list(name = annotDFs$name,
                   annotates = annotDFs$annotates,
                   sampleRate = annotDFs$sampleRate,
                   levels = levels, links = links)
  
  return(jsonlite::toJSON(annotJSON, auto_unbox = T, force = T, pretty = T))
  
}

