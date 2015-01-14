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


#########################
# FOR DEVELOPMENT
library('testthat')
test_file('tests/testthat/test_add.files.to.emuDB.R')

