
".onAttach" <- function(libname, pkgname)
{
  
  if (interactive()) {
    
    # packpath = path.package(package = "emuR", quiet = FALSE)
    # sepa = .Platform$file.sep
    # nfile = "emudirectory"
    # filepath = paste(packpath,sepa,nfile,sep = "")
    # mess = paste("\n \n     _  _________     _______    \n    | ||  ______ \\  / _______|    \n    | || | _____| || |_______     \n    | || ||______/  \\_______ \\    \n    | || | _   . . .   _ ___| |   \n    |_||_||_||.:.:.:.||_|____/            \n               : : :             \n      INSTITUTE OF PHONETICS      \n      AND SPEECH PROCESSING        \n                                  \n(C) IPS University of Munich\n\nR Package of the EMU Speech Database system - Version",packageVersion("emuR")," \nFor further information see https://github.com/IPS-LMU/emuR \n")    
    # packageStartupMessage(mess, appendLF = FALSE)
    # packageStartupMessage( "\nLibrary successfully loaded." )
  }
}




# ##' emudata init
# ##' 
# ##' loads package emudata as far as installed or reports message
# ##' 
# ##' 
# ##' @keywords internal
# emudata.init <- function() {
#    messemudatamore = ""
#   if(require(emudata)) {
#     
#     messemudata="V Additional datasets successfully loaded from package emudata."
#   } else {
#     messemudata="X NO additional databases loaded."
#     messemudatamore="    To have access to all datasets used in \n    Harrington, J. (2010). The Phonetic Analysis of Speech Corpora. Blackwell,\n    install package emudata."
#   }
#   if (interactive()) {
#     message(messemudata)
#     cat(messemudatamore)
#   }
# }
