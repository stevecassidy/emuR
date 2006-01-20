packpath = .path.package(package = "emu", quiet = FALSE)
  sepa = .Platform$file.sep
  nfile = "demo-all.seg"
  filepath = paste(packpath,sepa,"demo",sepa,nfile,sep = "")
  

demo.all <- read.segs(filepath)
