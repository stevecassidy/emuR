packpath = .path.package(package = "emuR", quiet = FALSE)
  sepa = .Platform$file.sep
  nfile = "demo-vowel-fm.dat"
  filepath = paste(packpath,sepa,"demo",sepa,nfile,sep = "")
 
 demo.vowels.fm <- read.trackdata(filepath)
