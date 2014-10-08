add.files.to.emuDB <- function(emuDB, path2rootDir, fileExt){
  
  # gernerate file pail list
  fpl = create.filePairList(emuDB$basePath, path2rootDir, emuDB$DBconfig$mediafileExtension, fileExt)
  
  # extract sourcePaths and destDirs
  sourcePaths = fpl[,2]
  destDirs = dirname(fpl[,1])
  
  # copy files
  for (i in 1:length(sourcePaths)){
    file.copy(sourcePaths[i], destDirs[i])
  }
  
} 

####################
# For Development

#convert.legacyEmuDB.to.emuDB('~/emuDBs/ae/ae.tpl', '~/Desktop/')
# ae.emu = load.emuDB('~/Desktop/ae/')
add.files.to.emuDB(ae.emu, '~/Desktop/aeNewFiles/', 'fms')
