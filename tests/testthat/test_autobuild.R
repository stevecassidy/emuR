##' testthat tests for autobuild
##'
##' @author Raphael Winkelmann
context("testing autobuild functions")

path2ae = system.file("extdata/emu/DBs/ae/", package = "emuR")

ae = load.emuDB(path2ae, verbose = F)

##############################
test_that("bad calls to autobuild.linkFromTimes", {
  
  expect_error(autobuild.linkFromTimes(ae, 'Phoneti', 'Tone'))
  expect_error(autobuild.linkFromTimes(ae, 'Phonetic', 'Ton'))
  expect_error(autobuild.linkFromTimes(ae, 'Phonetic', 'Tone'))
  
})


##############################
test_that("correct links are present after autobuild.linkFromTimes", {
  # add levelDef.
  tmpLevDef = create.schema.linkDefinition(type='ONE_TO_MANY', superlevelName='Phonetic', sublevelName='Tone')
  ae$DBconfig$linkDefinitions[[length(ae$DBconfig$linkDefinitions) + 1]] = tmpLevDef 
  
  res = autobuild.linkFromTimes(ae, 'Phonetic', 'Tone', FALSE)
  
  expect_equal(dim(res$links)[1], 839)
  expect_equal(res$links[786,]$session, '0000')
  expect_equal(res$links[786,]$bundle, 'msajc003')
  expect_equal(res$links[786,]$fromID, 149)
  expect_equal(res$links[786,]$toID, 181)

  expect_equal(res$links[787,]$session, '0000')
  expect_equal(res$links[787,]$bundle, 'msajc003')
  expect_equal(res$links[787,]$fromID, 156)
  expect_equal(res$links[787,]$toID, 182)
})
