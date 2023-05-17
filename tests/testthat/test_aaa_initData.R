context("init data for testing")
# AAA in the name so it is run as first test
path2demoData = file.path(tempdir(),"emuR_demoData")
path2testhatFolder = file.path(tempdir(),"emuR_testthat")

unlink(path2demoData, recursive = TRUE)
unlink(path2testhatFolder, recursive = TRUE)

create_emuRdemoData(precache = TRUE)
create_BPFcollectionManipulated(path2demoData)

dir.create(path2testhatFolder)
