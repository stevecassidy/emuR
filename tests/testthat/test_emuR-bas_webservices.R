context("stub tests for BAS webservices (to be extended)")

sourceDirMain = file.path(tempdir(), "emuR_demoData")
sourceDir = file.path(sourceDirMain, "txt_collection")
testDir = file.path(tempdir(), "emuR_testthat")
dbName = "bas_test"

unlink(file.path(testDir, paste0(dbName, emuDB.suffix)), recursive = T)

test_that("testing whether runBASwebservice_all runs without error",
          {
            skip("skipping as it takes 4 ever to test -> make sure to rerun on deploy")
            skip_on_cran()
            convert_txtCollection(
              sourceDir = sourceDir,
              targetDir = testDir,
              dbName = dbName,
              verbose = F
            )
            handle = load_emuDB(file.path(testDir, paste0(dbName, emuDB.suffix)), verbose = F)
            
            runBASwebservice_all(handle, "transcription", "eng-AU", verbose = F)
          })

unlink(file.path(testDir, paste0(dbName, emuDB.suffix)), recursive = T)

test_that(
  "testing whether all the other runBASwebservice_* functions run without error when chained together",
  {
    skip("skipping as it takes 4 ever to test -> make sure to rerun on deploy")
    skip_on_cran()
    convert_txtCollection(
      sourceDir = sourceDir,
      targetDir = testDir,
      dbName = dbName,
      verbose = F
    )
    handle = load_emuDB(file.path(testDir, paste0(dbName, emuDB.suffix)), verbose = F)
    
    runBASwebservice_g2pForTokenization(handle, "transcription", "eng-GB", verbose = F)
    runBASwebservice_g2pForPronunciation(handle, "ORT", "eng-GB", verbose = F)
    runBASwebservice_chunker(handle,
                             "KAN",
                             "eng-GB",
                             rootLevel = "bundle",
                             verbose = F)
    runBASwebservice_maus(handle,
                          "KAN",
                          "eng-GB",
                          chunkLevel = "TRN",
                          verbose = F)
    runBASwebservice_minni(handle, "eng-GB", rootLevel = "bundle", verbose = F)
    runBASwebservice_pho2sylCanonical(handle, "KAN", "eng-GB", verbose = F)
    runBASwebservice_pho2sylSegmental(handle,
                                      "MAU",
                                      "eng-GB",
                                      superLevel = "ORT",
                                      verbose = F)
  }
)

unlink(file.path(testDir, paste0(dbName, emuDB.suffix)), recursive = T)