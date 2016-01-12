require(tidyjson)

# convert annotJSON to list of data.frames including 
# meta information (name, annotates, samplerate)
annotJSONtoListOfDataFrames <- function(json){
  
  # get top level data
  tlData = json %>%
    spread_values(name = jstring("name"), annotates = jstring("annotates"), sampleRate = jstring("sampleRate"))
  
  # gen. links data.frame
  links = json %>%
    enter_object("links") %>%
    gather_array  %>%
    spread_values(fromID = jstring("fromID"), toID = jstring("toID")) %>%
    select(fromID, toID)
  
  # gen. items list of data.frame
  items = json %>%
    spread_values(sampleRate = jstring("sampleRate")) %>%
    enter_object("levels") %>%
    gather_array  %>%
    spread_values(level = jstring("name"), type = jstring("type")) %>%
    enter_object("items") %>%
    gather_array(column.name = "seqIdx") %>%
    spread_values(itemID = jstring("id"), samplePoint = jstring("samplePoint"), sampleStart = jstring("sampleStart"), sampleDur = jstring("sampleDur")) %>%
    select(itemID, level, type, seqIdx, sampleRate, samplePoint, sampleStart, sampleDur)
  
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
    select(itemID, labelIdx, name, label)
  
  return(list(name = tlData$name, annotates = tlData$annotates, sampleRate = tlData$sampleRate, items = items, links = links, labels = labels))
  
}
