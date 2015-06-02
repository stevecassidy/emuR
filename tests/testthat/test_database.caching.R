##' testthat tests for autobuild
##'
##' @author Raphael Winkelmann
context("testing caching functions")

suppressMessages(require('jsonlite'))

tmpDbName = 'ae_copy'

path2ae = system.file("extdata/emu/DBs/ae/", package = "emuR")

# load database 
if(!is.emuDB.loaded("ae")){
  load_emuDB(path2ae, verbose = F)
}

d = list_emuDBs()

update_cache('ae')
