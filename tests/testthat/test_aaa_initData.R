context("init data for testing")
# AAA in the name so it is run as first test
path2demoData = file.path(tempdir(),"emuR_demoData")
path2testhatFolder = file.path(tempdir(),"emuR_testthat")

purge_all_emuDBs(interactive = F)
unlink(path2demoData, recursive = T)
unlink(path2testhatFolder, recursive = T)

create_emuRdemoData(precache = T)

dir.create(file.path(tempdir(), "emuR_testthat"))
