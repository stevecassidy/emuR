packpath = .path.package(package = "emuR", quiet = FALSE)
  sepa = .Platform$file.sep
  nfile = "demo-all.seg"
  filepath = paste(packpath,sepa,"data",sepa,nfile,sep = "")
  

demo.all <- read.segs(filepath)
