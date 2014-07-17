packpath = .path.package(package = "emuR", quiet = FALSE)
  sepa = .Platform$file.sep
  nfile = "demo-all-rms.dat"
  filepath = paste(packpath,sepa,"data",sepa,nfile,sep = "")
 
 demo.all.rms <- read.trackdata(filepath)
