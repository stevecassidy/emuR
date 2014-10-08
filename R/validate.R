## Validates the list representation of bundle 
##
validate.listFrom.bundle<-function(DBconfig, bundle){
  # validate sequence and overlaps
  for(level in bundle[['levels']]){
    levelType=level[['type']]
    sp=-1
    for(item in level[['items']]){
      
      if(levelType=='SEGMENT'){
        start=item[['sampleStart']]
        if(start<=sp){
          return(FALSE)
        }
        dur=item[['sampleDur']]
        if(dur<0){
          return(FALSE)
        }
        sp=start+dur
      }else if(levelType=='EVENT'){
        point=item[['samplePoint']]
        if(point<=sp){
          return(FALSE)
        }
        sp=point
      }else if(levelType=='ITEM'){
        # 
      }
      
      
    }
    # check for cross links  
    for(link in links){
      # TODO
    }
    
  }
  return(TRUE)
}

## Validates the dataframe representation of bundle 
##
validate.dfForm.bundle<-function(DBconfig, bundle){
  stop('NOT IMPLEMENTED YET!!!')
}
