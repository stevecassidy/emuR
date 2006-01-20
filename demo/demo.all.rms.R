packpath = .path.package(package = "emu", quiet = FALSE)
  sepa = .Platform$file.sep
  nfile = "demo-all-rms.dat"
  filepath = paste(packpath,sepa,"demo",sepa,nfile,sep = "")
 
 demo.all.rms <- read.trackdata(filepath)
