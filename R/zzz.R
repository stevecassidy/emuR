.onLoad <- function(libname, pkgname) {
  op <- options()
  op.emuR <- list(
    emuR.emuWebApp.dir = file.path(tempdir(), "EMU-webApp")
    # emuR.emuWebApp.dir = file.path("~/Developer/EMU-webApp/dist/") # for devel
  )
  # set package options 
  toset <- !(names(op.emuR) %in% names(op))
  if(any(toset)) options(op.emuR[toset])
  
  invisible() 
}