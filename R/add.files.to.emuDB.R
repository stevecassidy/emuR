add.files.to.emuDB <- function(emuDB, path2rootDir, fileExt){
  
  # gernerate file pail list
  fpl = create.filePairList(emuDB$basePath, path2rootDir, emuDB$DBconfig$mediafileExtension, fileExt)
  
  # extract dirnames
  fpl[,1] = dirname(fpl[,1])
  
  # copy files
  file.copy(from = fpl[,2], to = fpl[,1])
  
} 

####################
# For Development

# ae.emu = load.emuDB('~/Desktop/ae/')
# add.files.to.emuDB(ae.emu, '~/Desktop/aeNewFiles/', 'fms')
