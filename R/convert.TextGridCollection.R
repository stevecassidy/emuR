##' 
##' 
##' 
##' @param path2rootDir path to root directory (CAUTION: think of DB size and search space!) 
##' @param tgExt extention of TextGrid files (default=TextGrid meaning filesnames of the form baseName.TextGrid)
##' @param audioExt extention of audio files (default=wav meaning filesnames of the form baseName.wav). 
##' NOTE: Currently the only supported format accross all components of the new EMU system is the 'wav' audio format .
##' 
##' 
convert.TextGridCollection.to.emuDB <- function(path2rootDir, dbName, tgExt='TextGrid', audioExt='wav'){
  
  # gernerate file pail list
  fpl = create.filePairList(path2rootDir, audioExt, tgExt)
  
  # gereate schema from first TextGrid in fpl
  dbd = create.database.schema.from.TextGrid(fpl[1,2], dbName)
  
  
  # create empty database
  db = create.database(name = dbd$name, basePath = '', DBconfig = dbd)
  db = initialize.database.dataframes(db)

  # allBundles object to hold bundles without levels and links
  allBundles = list()
  
  # loop through fpl
  i = 1
  
  # get sampleRate of audio file
  asspObj = read.AsspDataObj(fpl[i,1])
  
  # parse TextGrid
  levels = parse.textgrid(fpl[i,2],  attributes(asspObj)$sampleRate) # SIC check parser -> still seems to have + 1 bug
  
  bndlName = basename(file_path_sans_ext(fpl[i,2]))
  
  # create bundle
  bundle = create.bundle(name = bndlName,
                         annotates = paste0('0000_ses/', bndlName, '_bndl/', bndlName, '.', audioExt),
                         sampleRate = attr(asspObj,'sampleRate'),
                         levels = levels,
                         signalpaths = list(fpl[i,1]),
                         mediaFilePath = fpl[i,1],
                         links = list())
  
  # this will be slow for large DBs
  db = append.bundle.to.tmp.list(db, bundle)
  
  # remove levels and links and append to allBundles
  bundle[['levels']] = NULL
  bundle[['links']] = NULL
  
  bName=bundle[['name']]
  
  allBundles[[bndlName]]=bundle
  

  # reassign items as dataframe
  itemsIdx=db[['itemsIdx']]
  db[['items']]=data.frame(id=db[['items']][['id']][1:itemsIdx],bundle=db[['items']][['bundle']][1:itemsIdx],level=db[['items']][['level']][1:itemsIdx],bundleId=db[['items']][['bundleId']][1:itemsIdx],type=db[['items']][['type']][1:itemsIdx],seqIdx=db[['items']][['seqIdx']][1:itemsIdx],sampleRate=db[['items']][['sampleRate']][1:itemsIdx],samplePoint=db[['items']][['samplePoint']][1:itemsIdx],sampleStart=db[['items']][['sampleStart']][1:itemsIdx],sampleDur=db[['items']][['sampleDur']][1:itemsIdx],label=db[['items']][['label']][1:itemsIdx],stringsAsFactors=FALSE)
  
  # reassign labels as dataframe
  labelsIdx=db[['labelsIdx']]
  db[['labels']]=data.frame(itemID=db[['labels']][['itemID']][1:labelsIdx],bundle=db[['labels']][['bundle']][1:labelsIdx],labelIdx=db[['labels']][['labelIdx']][1:labelsIdx],name=db[['labels']][['name']][1:labelsIdx],label=db[['labels']][['label']][1:labelsIdx],stringsAsFactors=FALSE)
  
  # reassign links as dataframe
  linksIdx=db[['linksIdx']]
  db[['links']]=data.frame(bundle=db[['links']][['bundle']][1:linksIdx],fromID=db[['links']][['fromID']][1:linksIdx],toID=db[['links']][['toID']][1:linksIdx],label=db[['links']][['label']][1:linksIdx],stringsAsFactors=FALSE)
  
  # create dummy container
  containerSession=emuDB.session(name='0000', bundles=allBundles)
  db[['sessions']][['0000']]=containerSession
  
  
  
  return(db)
}




# FOR DEVELOPMENT
path2rootDir = system.file("extdata/legacy_emu/DBs/", package = "emuR")
newDB = convert.TextGridCollection.to.emuDB(path2rootDir, 'test12')
