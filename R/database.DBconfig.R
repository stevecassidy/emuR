get.levelDefinition <- function(DBconfig, name){
  res = NULL
  for(ld in DBconfig$levelDefinitions){
    if(ld$name == name){
      res = ld
      break
    }
  }
  return(res)
}

# FOR DEVELOPMENT 
#library('testthat') 
#test_file('tests/testthat/test_database.DBconfig.R')
