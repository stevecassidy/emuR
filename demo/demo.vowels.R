packpath = .path.package(package = "emuR", quiet = FALSE)
  sepa = .Platform$file.sep
  nfile = "demo-vowels.seg"
  filepath = paste(packpath,sepa,"demo",sepa,nfile,sep = "")
 
 demo.vowels <- read.segs(filepath)
