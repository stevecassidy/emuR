context("clean up test data")

unlink(file.path(tempdir(),"emuR_demoData"), recursive = TRUE)
unlink(file.path(tempdir(),"emuR_testthat"), recursive = TRUE)
