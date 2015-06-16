##' testthat tests for database.DBconfig.EMUwebAppConfig
##'
##' @author Raphael Winkelmann
context("testing database.DBconfig functions")

dbName = 'ae'

path2orig = file.path(tempdir(), "emuR_demoData", dbName)
path2testData = file.path(tempdir(), "emuR_testthat")
path2db = file.path(path2testData, dbName)

##############################
test_that("CRUD operations work for perspectives", {
  
  # purge, delete, copy and load
  if(is.emuDB.loaded(dbName)){
    purge_emuDB(dbName, interactive = F)
  }
  unlink(path2db, recursive = T)
  file.copy(path2orig, path2testData, recursive = T)
  load_emuDB(path2db, inMemoryCache = internalVars$testingVars$inMemoryCache, verbose = F)
  
  test_that("add = (C)RUD", {
    # bad call persp. already exists
    expect_error(add_perspective(dbName=dbName, 
                                 name = 'default'))
    add_perspective(dbName=dbName, 
                    name = 'newPersp')
    
  })
  
  test_that("list = C(R)UD", {
    df = list_perspectives(dbName=dbName)
    
    expect_true(df$name[1] == "default")
    expect_true(df$signalCanvasesOrder[1] == "OSCI; SPEC")
    expect_true(df$levelCanvasesOrder[1] == "Phonetic; Tone")
    
    expect_true(df$name[2] == "newPersp")
    expect_true(df$signalCanvasesOrder[2] == "OSCI; SPEC")
    expect_true(df$levelCanvasesOrder[2] == "")
  })
  
  test_that("modify = CR(U)D", {
    # currently not implemented
  })
  
  test_that("remove = CRU(D)", {
    
    remove_perspective(dbName=dbName, 
                       name = 'newPersp')
    
    df = list_perspectives(dbName=dbName)
    expect_equal(nrow(df), 1)
  })
  
})

##############################
test_that("CRUD operations work for signalCanvasesOrder", {
  
  # purge, delete, copy and load
  purge_emuDB(dbName, interactive = F)
  unlink(path2db, recursive = T)
  file.copy(path2orig, path2testData, recursive = T)
  load_emuDB(path2db, inMemoryCache = internalVars$testingVars$inMemoryCache, verbose = F)
  
  
  test_that("set = (C)RUD", {
    expect_error(set_signalCanvasesOrder(dbName, 
                                         perspectiveName = "default",
                                         order = c("OSCI", "badTrackName")))
    
    set_signalCanvasesOrder(dbName, 
                            perspectiveName = "default",
                            order = c("OSCI", "SPEC", "fm"))
    
  })
  
  test_that("get = C(R)UD", {
    order = get_signalCanvasesOrder(dbName, perspectiveName = "default")
    
    expect_equal(order[1], "OSCI")
    expect_equal(order[2], "SPEC")
    expect_equal(order[3], "fm")
  })
  
  test_that("modify = CR(U)D", {
    # currently not implemented
  })
  
  test_that("remove = CRU(D)", {
    # currently not implemented
  })
  
})

##############################
test_that("CRUD operations work for levelCanvasesOrder", {
  # purge, delete, copy and load
  purge_emuDB(dbName, interactive = F)
  unlink(path2db, recursive = T)
  file.copy(path2orig, path2testData, recursive = T)
  load_emuDB(path2db, inMemoryCache = internalVars$testingVars$inMemoryCache, verbose = F)
  
  
  test_that("set = (C)RUD", {
    # bad level name
    expect_error(set_levelCanvasesOrder(dbName, 
                                        perspectiveName = "default",
                                        order = c("Phonetic", "badLevelName")))
    
    # bad level type
    expect_error(set_levelCanvasesOrder(dbName, 
                                        perspectiveName = "default",
                                        order = c("Phonetic", "Tone", "Word")))
    
    set_levelCanvasesOrder(dbName, 
                           perspectiveName = "default",
                           order = c("Tone", "Phonetic"))
    
  })
  
  test_that("get = C(R)UD", {
    order = get_levelCanvasesOrder(dbName, perspectiveName = "default")
    
    expect_equal(order[1], "Tone")
    expect_equal(order[2], "Phonetic")
  })
  
  test_that("modify = CR(U)D", {
    # currently not implemented
  })
  
  test_that("remove = CRU(D)", {
    # currently not implemented
  })
  
})



