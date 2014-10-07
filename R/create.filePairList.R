## Create a file-pair-list
##
## Recursivly searches through a root directory and matches the
## basenames of files that have the extentions provided. 
##
## 
## @param path2rootDir path to root directory (CAUTION: think of DB size and search space!) 
## @param ext1 first extention to look for
## @param ext2 second extention to pair base names of first extention with
##
create.filePairList <- function(path2rootDir, ext1, ext2){
  # PathToDbRootFolder is valid path
  if(!file.exists(path2rootDir)){
    stop('path2rootDir does not exist!')
  }
  
  # get all ext1 file paths
  allExt1FilePaths = list.files(path2rootDir, pattern=paste(ext1, "$", sep = ""), recursive=T, full.names=T)
  
  # get all audio file paths
  allExt2FilePaths = list.files(path2rootDir, pattern=paste(ext2, "$", sep = ""), recursive=T, full.names=T)
  
  # extract base names
  allExt1FilePathsBNs = basename(file_path_sans_ext(allExt1FilePaths))
  allExt2FilePathsBNs = basename(file_path_sans_ext(allExt2FilePaths))
  
  # check for equality
  if(length(setdiff(allExt1FilePathsBNs, allExt2FilePathsBNs)) != 0){
    stop('Did not find matching audio/TextGrid file pairs for file base name(s): ', paste(setdiff(allExt1FilePathsBNs, allExt2FilePathsBNs), collapse = ' '))
  }
  
  # check they are the same length (not caught by setdiff())
  if(length(allExt1FilePathsBNs) != length(allExt2FilePathsBNs)){
    stop('Not the same amount of ', ext1, ' and ', ext2, ' files found in ', path2rootDir) 
  }
  
  # cbind filePairList
  fpl = cbind(allExt1FilePaths, allExt2FilePaths)
  
}