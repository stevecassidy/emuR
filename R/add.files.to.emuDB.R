##' Add files to bundles
##' 
##' Matches the basenames of all files found in path2rootDir
##' with the according mediafiles contained in the session
##' folder specified and copies the files into the according
##' _bndl folders.
##' 
##' @param path2rootDir path to directory containing files that should be added
##' @param path2sessionDir path to session directory containing the destination bundles
##' @param fileExt file extention of the files that should be added to the bundles
##' @param mediafileExtension file extention of media files of emuDB (default='wav')
##' @export
##' @author Raphael Winkelmann
##'
add.files.to.bundles <- function(path2rootDir, path2sessionDir, 
                                 fileExt, mediafileExtension = 'wav'){
  
  # gernerate file pail list
  fpl = create.filePairList(path2sessionDir, path2rootDir, mediafileExtension, fileExt)

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
# library('testthat')
# test_file('tests/testthat/test_add.files.to.emuDB.R')

