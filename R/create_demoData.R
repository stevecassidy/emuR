##' Create demo data for emuR package
##' 
##' Will create a folder in the folder specified
##' by the dir argument called emuR_demoData.
##' This folder contains the folders:
##' \itemize{
##' \item{ae: }{Containing an emuDB that adheres to the new formant specification (as expected by the \code{load_emuDB()} function)} 
##' \item{TextGrid_collection: }{Containing a TextGrid collection (as expected from the \code{convert_TextGridCollection_to_emuDB()} function)}
##' }
create_demoData <- function(dir = tempdir()){
  
  
  path2extdata = system.file("extdata", package = "emuR")
  create_filePairList()
  
  tgPaths = list.files(path2extdata, pattern = "TextGrid", recursive = T)
  
  
}