## Create a file-pair-list
##
## Recursivly searches through a root directory and matches the
## basenames of files that have the extentions provided. 
##
## 
## @param ext1Path2rootDir path to root directory of first file extention (CAUTION: think of DB size and search space!) 
## @param ext2Path2rootDir path to root directory of second file extention (CAUTION: think of DB size and search space!) 
## @param ext1 first extention to look for
## @param ext2 second extention to pair base names of first extention with
##
create.filePairList <- function(ext1Path2rootDir, ext2Path2rootDir, ext1, ext2){
  # ext1Path2rootDir is valid path
  if(!file.exists(ext1Path2rootDir)){
    stop('ext1Path2rootDir does not exist!')
  }

  # ext2Path2rootDir is valid path
  if(!file.exists(ext2Path2rootDir)){
    stop('ext2Path2rootDir does not exist!')
  }
  
  
  
  # get all ext1 file paths
  allExt1FilePaths = list.files(ext1Path2rootDir, pattern=paste(ext1, "$", sep = ""), recursive=T, full.names=T)
  
  # get all audio file paths
  allExt2FilePaths = list.files(ext2Path2rootDir, pattern=paste(ext2, "$", sep = ""), recursive=T, full.names=T)
  
  # extract base names
  allExt1FilePathsBNs = basename(file_path_sans_ext(allExt1FilePaths))
  allExt2FilePathsBNs = basename(file_path_sans_ext(allExt2FilePaths))
  
  # check for equality
  if(length(setdiff(allExt1FilePathsBNs, allExt2FilePathsBNs)) != 0){
    stop('Did not find matching ext1/ext2 file pairs for file base name(s): ', paste(setdiff(allExt1FilePathsBNs, allExt2FilePathsBNs), collapse = ' '))
  }
  
  # check they are the same length (not caught by setdiff())
  if(length(allExt1FilePathsBNs) != length(allExt2FilePathsBNs)){
    stop('Not the same amount of ', ext1, ' and ', ext2, ' files found in ', ext1Path2rootDir, ' and ', ext2Path2rootDir ) 
  }

  # check they are empty
  if(length(allExt1FilePathsBNs)==0 || length(allExt2FilePathsBNs) == 0){
    stop('Both colomns in file pair list are empty! That means no files where found...') 
  }
  
  # cbind filePairList
  fpl = cbind(allExt1FilePaths, allExt2FilePaths)
  
}