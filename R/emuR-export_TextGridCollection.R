##' Export to TextGridCollection
##' @description Export the annotations of an emuDB to a TextGrid collection (.TextGrid and .wav file pairs)
##' @details add more
##' @param emuDBhandle emuDB handle object (see \link{load_emuDB})
##' @param targetDir directory where the TextGrid collection should be saved
##' @param sessionPattern A regular expression pattern matching session names to be exported from the database
##' @param bundlePattern A regular expression pattern matching bundle names to be exported from the database
##' @export
##' @seealso \code{\link{load_emuDB}}
##' @keywords emuDB database query Emu EQL 
##' @examples
##' \dontrun{
##' 
##' ##################################
##' # prerequisite: loaded ae emuDB 
##' # (see ?load_emuDB for more information)
##' 
##' ## Export all levels
##' 
##' 
##' }
##' 
export_TextGridCollection <- function(emuDBhandle, targetDir, sessionPattern = '.*', bundlePattern = '.*', 
                                      levelNames = NULL) {
  
  dbConfig = load_DBconfig(emuDBhandle)
  lds = list_levelDefinitions(emuDBhandle)
  
  # extract all items as giant seglist
  slAll = NULL
  for(i in 1:nrow(lds)){
    sl = query(emuDBhandle, paste0(lds$name[i], "=~ .*"))
    slAll = rbind(slAll, sl)
  }
  # convert times to seconds
  slAll$start = slAll$start / 1000
  slAll$end = slAll$end / 1000
  
  # create target dir
  if(dir.exists(targetDir)){
    stop(paste0("targetDir: ", targetDir, " already exists!"))
  }else{
    dir.create(targetDir)
  }
  
  # extract rel.  bundles
  bndls = list_bundles(emuDBhandle)
  bndls = bndls[grepl(sessionPattern, bndls$session) & grepl(bundlePattern, bndls$name),]
  
  # loop through bundles and write to TextGrids & copy wav
  for(i in 1:nrow(bndls)){
    curSes = bndls[i, ]$session
    curBndl = bndls[i, ]$name
    # check if session folder exists
    sesDir = file.path(targetDir, bndls[i, ]$session)
    if(!dir.exists(sesDir)){
      dir.create(sesDir)
    }
    
    # copy wav file
    wavPath = file.path(emuDBhandle$basePath, paste0(curSes, session.suffix), paste0(curBndl, bundle.dir.suffix), paste0(curBndl, ".", dbConfig$mediafileExtension))
    file.copy(wavPath, sesDir)
    # extract bundle sl
    slBndl = slAll[grepl(curSes, slAll$session) & grepl(curBndl, slAll$bundle), ]
    tgPath = file.path(sesDir, paste0(curBndl, ".TextGrid"))
    
    wavDur = wrassp::dur.AsspDataObj(wrassp::read.AsspDataObj(wavPath))
    
    # tg header
    tgHeader = c("File type = \"ooTextFile\"",
                 "Object class = \"TextGrid\"", 
                 "",
                 "xmin = 0 ",
                 paste0("xmax = ", wavDur, " "),
                 "tiers? <exists> ",
                 paste0("size = ", nrow(lds), " "),
                 "item []: ")
    
    write(tgHeader, tgPath)
    
    # TODO: multiple attribute defs!
    for(ldRowIdx in 1:nrow(lds)){
      
      slTier = slBndl[slBndl$level == lds[ldRowIdx,]$name,]
      
      emptyRow = data.frame(labels = "", start = -1, end = -1, 
                            utts = "", db_uuid = "", session = "", bundle = "", 
                            startItemID = "", endItemID = "", level = "", 
                            type = "", sampleStart = "", sampleEnd = "", sampleRate = "", stringsAsFactors = F)
      # tier header
      if(all(slTier$end == 0)){
        tierType = "TextTier"
      }else{
        tierType = "IntervalTier"
        if(min(slTier$start) > 0){
          # add empty segment to left (== pad left)
          emptyRow$start = 0
          emptyRow$end = min(slTier$start)
          slTier = rbind(emptyRow, slTier)
        }
        
        if(max(slTier$end) < wavDur){
          # add empty segment to right (== pad right)
          emptyRow$start = max(slTier$end)
          emptyRow$end = wavDur
          slTier = rbind(slTier, emptyRow)
        }
        # TODO: missing segments!
      }
      tierHeader = c(paste0("    item [", ldRowIdx, "]:"), 
                     paste0("        class = \"", tierType, "\" "),
                     paste0("        name = \"", lds[ldRowIdx,]$name, "\" "),
                     "        xmin = 0 ",
                     paste0("        xmax = ", wavDur, " "))
      
      write(tierHeader, tgPath, append=TRUE)
      
      # tier items
      if(tierType == "IntervalTier"){
        tierItems = c(paste0("        intervals: size = ", nrow(slTier), " "), 
                      c(rbind(paste0("        intervals [",1:nrow(slTier), "]:"), 
                              paste0("            xmin = ", slTier$start, " "),
                              paste0("            xmax = ", slTier$end, " "),
                              paste0("            text = \"", slTier$labels, "\" "))))
        
      }else{
        tierItems = c(paste0("        points: size = ", nrow(slTier), " "), 
                      c(rbind(paste0("        points [",1:nrow(slTier), "]:"), 
                              paste0("            number = ", slTier$start, " "),
                              paste0("            mark = \"", slTier$labels, "\" "))))
        
      }
      write(tierItems, tgPath, append=TRUE)
    }
  }
  
}

# FOR DEVELOPMENT
# library('testthat')
# test_file('tests/testthat/test_emuR-export_TextGridCollection.R')

