require(tidyjson)

#######################################################
# annotJSON representation to annotDFs conversion functions

# convert annotJSON to list of data.frames including 
# meta information (name, annotates, samplerate)
annotJSONcharToBundleAnnotDFs <- function(annotJSONchar){
  
  json = annotJSONchar %>% as.tbl_json
  
  # get top level data
  tlData = json %>%
    spread_values(name = jstring("name"), annotates = jstring("annotates"), sampleRate = jstring("sampleRate"))
  
  # gen. links data.frame
  links = json %>%
    enter_object("links") %>%
    gather_array  %>%
    spread_values(fromID = jstring("fromID"), toID = jstring("toID")) %>%
    select_(~fromID, ~toID)
  
  # gen. items list of data.frame
  items = json %>%
    spread_values(sampleRate = jstring("sampleRate")) %>%
    enter_object("levels") %>%
    gather_array  %>%
    spread_values(level = jstring("name"), type = jstring("type")) %>%
    enter_object("items") %>%
    gather_array(column.name = "seqIdx") %>%
    spread_values(itemID = jstring("id"), samplePoint = jstring("samplePoint"), sampleStart = jstring("sampleStart"), sampleDur = jstring("sampleDur")) %>%
    select_(~itemID, ~level, ~type, ~seqIdx, ~sampleRate, ~samplePoint, ~sampleStart, ~sampleDur)
  
  # gen. label list of data.frame
  labels = json %>%
    enter_object("levels") %>%
    gather_array  %>%
    spread_values(level = jstring("name")) %>%
    enter_object("items") %>%
    gather_array %>%
    spread_values(itemID = jstring("id")) %>%
    enter_object("labels") %>%
    gather_array(column.name = "labelIdx") %>%
    spread_values(name = jstring("name"), label = jstring("value")) %>%
    select_(~itemID, ~labelIdx, ~name, ~label)
  
  return(list(name = tlData$name, annotates = tlData$annotates, sampleRate = tlData$sampleRate, items = items, links = links, labels = labels))
  
}

# convert annotDFs (annotation list of data.frame representation) to annotJSON
bundleAnnotDFsToAnnotJSONchar <- function(emuDBhandle, annotDFs){
  
  # load DBconfig to generate levelNames vector (although levels are not ordered per say)
  DBconfig = load_DBconfig(emuDBhandle)
  levelNames = sapply(DBconfig$levelDefinitions, function(l)l$name)
  
  levels = list()
  
  for(l in levelNames){
    levelItems = filter(annotDFs$items, level == l)
    
    levels[[length(levels) + 1]] = apply(levelItems, 1, function(r) {
      
      labels = apply(filter(annotDFs$labels, itemID == as.numeric(r[1])), 1, function(r2) list(name = r2[3], toID = r2[4]))
      
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
                   samplePoint = as.numeric(r[9]),
                   labels = labels)
      }
      return(res)
    })
  }
  
  links = apply(annotDFs$links, 1, function(r) list(fromID = as.numeric(r[1]), toID = as.numeric(r[2])))

  annotJSON = list(name = annotDFs$name,
                   annotates = annotDFs$annotates,
                   sampleRate = annotDFs$sampleRate,
                   levels = levels, links = links)
  
  return(jsonlite::toJSON(annotJSON, auto_unbox = T, force = T, pretty = T))
}

