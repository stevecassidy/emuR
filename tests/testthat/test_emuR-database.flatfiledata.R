##' testthat tests for flatfiledata
##'
context("testing database flatfiledata functions")

dbName = "ae"

path2orig = file.path(tempdir(), 
                      "emuR_demoData", 
                      paste0(dbName, emuDB.suffix))
path2testData = file.path(tempdir(), "emuR_testthat")
path2db = file.path(path2testData, 
                    paste0(dbName, emuDB.suffix))

# delete, copy and load
unlink(path2db, recursive = T)
file.copy(path2orig, 
          path2testData, 
          recursive = T)

# create a second session
dir.create(file.path(path2db, "0001_ses"))
file.copy(from = list.files(file.path(path2db, "0000_ses"), full.names = T),
          to = file.path(path2db, "0001_ses"),
          recursive = T)

# extract internalVars from environment .emuR_pkgEnv
internalVars = get("internalVars", 
                   envir = .emuR_pkgEnv)

db = load_emuDB(path2db, 
                inMemoryCache = internalVars$testingVars$inMemoryCache, 
                verbose = F)

test_that("join_flatFileData works on emuDB level", {
  
  # key value emuDB data
  flat_data = tibble::tibble(key = c("location", "institution"), value = c("Muenchen", "IPS"))
  
  readr::write_tsv(x = flat_data, file = file.path(db$basePath, paste0(db$dbName, "_keyValue.", "tsv")))
  
  sl = query(db, "Phonetic == S")
  
  sl_joined = join_flatFileData(db, sl)
  
  expect_true(all(c("location", "institution") %in% names(sl_joined)))
  
  expect_equal(sl_joined$location, rep("Muenchen", 10))
  
  expect_equal(sl_joined$institution, rep("IPS", 10))
  
  # long emuDB data
  # specify session and bundle
  long_data = tibble::tibble(session = c("0000", "0000"),
                             bundle = c("msajc003", "msajc012"),
                             eyecolor = c("blue", "brown"))
  
  readr::write_tsv(x = long_data, file = file.path(db$basePath, paste0(db$dbName, "_long.", "tsv")))
  
  sl_joined = join_flatFileData(db, sl)
  expect_equal(length(which(is.na(sl_joined$eyecolor))), 7)
})

test_that("join_flatFileData works on session level", {
  
  # key value session data
  flat_data = tibble::tibble(key = c("location", "fudge", "speed"), value = c("Muenchen", "yummy", "fast"))
  
  readr::write_tsv(x = flat_data, file = file.path(db$basePath, "0000_ses", paste0("0000", "_keyValue.", "tsv")))
  
  sl = query(db, "Phonetic == S")
  
  sl_joined = join_flatFileData(db, sl)
  
  expect_true(all(c("location.x", "location.y") %in% names(sl_joined)))
  
  expect_equal(length(which(is.na(sl_joined$location.x))), 5)
  expect_equal(length(which(is.na(sl_joined$fudge))), 5)
  expect_equal(length(which(is.na(sl_joined$speed))), 5)
  
  # 
  # expect_equal(sl_joined$location, c("Muenchen", 
  #                                    "Muenchen",
  #                                    "Muenchen",
  #                                    "Muenchen",
  #                                    "Muenchen"))
  # 
  # expect_equal(sl_joined$institution, c("IPS",
  #                                       "IPS",
  #                                       "IPS",
  #                                       "IPS",
  #                                       "IPS"))
  # 
  # # long emuDB data
  # # specify session and bundle
  # long_data = tibble::tibble(session = c("0000", "0000"),
  #                            bundle = c("msajc003", "msajc012"),
  #                            eyecolor = c("blue", "brown"))
  # 
  # readr::write_tsv(x = long_data, file = file.path(db$basePath, paste0(db$dbName, "_long.", "tsv")))
  # 
  # sl_joined = join_tsvs(db, sl)
  # # 4 NAs
  # expect_equal(length(which(is.na(sl_joined$eyecolor))), 2)
})
