context("init data for testing")
# AAA in the name so it is run as first test

unlink(file.path(tempdir(),"emuR_demoData"), recursive = T)
create_emuRdemoData(precache = T)