"get.bundle.stub" <- function(dbObj, ...)
{
  UseMethod("get.bundle.stub")
  
}

"get.bundle.stub.emuDB" <- function(dbObj, bundleName = NULL)
{
  # loop through session and sapply through bundles
  foundSessionBundles = NULL
  for (i in 1:length(dbObj$sessions)){
    foundBool = sapply(dbObj$sessions[[i]]$bundles, function(x){x$name == bundleName})
    foundSessionBundles = c(foundSessionBundles, dbObj$sessions[[i]]$bundles[foundBool])
  }
  
  # check if 0 or more than one bundles where found
  if(length(foundSessionBundles) > 1 ){
    stop('Found multiple bundles with the name: ', bundleName, ' This means that the db must be configured')
  }else if(length(foundSessionBundles) == 0){
    stop('No bundle found with name: ', bundleName)
  }
  
  return(foundSessionBundles[[1]])
}

#######################
# FOR DEVELOPMENT

# ae.db = load.database('~/Desktop/ae/')
# class(ae.db) = 'emuDB'
# nSegl = query.database(ae.db, 'Phonetic=n')
# bndl = get.bundle.stub(ae.db, bundleName = 'msajc003')
# print(bndl$name)
