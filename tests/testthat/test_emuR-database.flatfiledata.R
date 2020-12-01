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
  
  sl = query(db, "Phonetic == S")
  
  # check error is thrown for bad columns in long data
  keyValue_data = tibble::tibble(bad_col_name = c("location", "institution"), 
                                 value = c("Muenchen", "IPS"))
  
  file_path = file.path(db$basePath, paste0(db$dbName, "_long.", "csv"))
  
  readr::write_excel_csv2(x = keyValue_data, file = file_path)
  expect_error(join_flatFileData(db, sl), 
               ".*doesn't only contain the columns.*")
  
  # long emuDB data
  keyValue_data = tibble::tibble(key = c("location", "institution"), 
                                 value = c("Muenchen", "IPS"))
  
  file_path = file.path(db$basePath, paste0(db$dbName, "_long.", "csv"))
  
  readr::write_excel_csv2(x = keyValue_data, file = file_path)
  
  sl_joined = join_flatFileData(db, sl)
  
  expect_true(all(c("location", "institution") %in% names(sl_joined)))
  expect_equal(sl_joined$location, rep("Muenchen", 10))
  expect_equal(sl_joined$institution, rep("IPS", 10))
  
  unlink(file_path)
  
  # wide emuDB data
  # specify session and bundle
  wide_data = tibble::tibble(session = c("0000", "0000", "0001"),
                             bundle = c("msajc003", "msajc012", "msajc022"),
                             eyecolor = c("blue", "brown", "green"),
                             height = c("1.80", "1.60", "2.00"))
  
  file_path = file.path(db$basePath, paste0(db$dbName, "_wide.", "csv"))
  
  readr::write_csv2(x = wide_data, file = file_path)
  
  sl_joined = join_flatFileData(db, sl)
  # check that long is still the same
  expect_true(all(c("location", "institution") %in% names(sl_joined)))
  expect_equal(sl_joined$location, rep("Muenchen", 10))
  expect_equal(sl_joined$institution, rep("IPS", 10))
  # plus new wide data is added and correct
  expect_equal(sl_joined$eyecolor[1:3], c("blue", "brown", "brown"))
  expect_equal(sl_joined$eyecolor[9:10], c("green", "green"))
  expect_equal(length(which(is.na(sl_joined$eyecolor))), 5)
  
})

test_that("join_flatFileData works on session level", {
  
  sl = query(db, "Phonetic == S")
  # long session data
  flat_data = tibble::tibble(key = c("location", "fudge", "speed"), 
                             value = c("Muenchen", "yummy", "fast"))
  
  readr::write_csv2(x = flat_data, 
                    file = file.path(db$basePath, "0000_ses", paste0("0000", "_long.", "csv")))
  
  
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
