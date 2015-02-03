##' testthat tests for autobuild
##'
##' @author Raphael Winkelmann
context("testing autobuild functions")

path2ae = system.file("extdata/emu/DBs/ae/", package = "emuR")

ae = load.emuDB(path2ae, verbose = F)

##############################
test_that("bad calls", {
  
#   expect_error(autobuild.linkFromTimes(ae, 'Phoneti', 'Tone'))
#   expect_error(autobuild.linkFromTimes(ae, 'Phonetic', 'Ton'))
  
})


##############################
test_that("first test...", {
  # add levelDef.
  tmpLevDef = create.schema.linkDefinition(type='ONE_TO_MANY', superlevelName='Phonetic', sublevelName='Tone')
  ae$DBconfig$linkDefinitions[[length(ae$DBconfig$linkDefinitions) + 1]] = tmpLevDef 
  
  autobuild.linkFromTimes(ae, 'Phonetic', 'Tone')
  
})
