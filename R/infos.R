"dbinfo" <- function () 
{
  if (unlist(strsplit(tclvalue(.TclEval(sprintf("catch emuR::getdbs"))), " ")) == "1") {
    cat("You need a least version EMU 2.4 to use the dbinfo function!\n")
  } else {
    unlist(strsplit(tclvalue(.TclEval(sprintf("emuR::getdbs"
                                                    ))), " "))
  }
}

"emurinfo" <- function () 
{
  if (unlist(strsplit(tclvalue(.TclEval(sprintf("catch emuR::getemurversion"))), " ")) == "1") {
    cat("You need a least version EMU 2.4 to use the emurinfo function!\n")
  } else {
    unlist(strsplit(tclvalue(.TclEval(sprintf("emuR::getemurversion"
                                                    ))), " "))
  }
}

"emuinfo" <- function () 
{
  if (unlist(strsplit(tclvalue(.TclEval(sprintf("catch emuR::getemuversion"))), " ")) == "1") {
    cat("You need a least version EMU 2.4 to use the emuinfo function!\n")
  } else {
    unlist(strsplit(tclvalue(.TclEval(sprintf("emuR::getemuversion"
                                                    ))), " "))
  }                                                  
}

