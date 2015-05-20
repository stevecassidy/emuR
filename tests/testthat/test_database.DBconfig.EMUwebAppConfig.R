##' testthat tests for database.DBconfig.EMUwebAppConfig
##'
##' @author Raphael Winkelmann
context("testing database.DBconfig functions")

path2extdata = system.file("extdata", package = "emuR")

if(!is.emuDB.loaded("ae")){
  load_emuDB(paste(path2extdata, '/emu/DBs/ae/', sep = ''), verbose = F)
}

tmpDbName = 'ae_copy'

##############################
test_that("CRUD operations work for perspectives", {
  # pre clean (just in case)
  unlink(file.path(tempdir(),tmpDbName), recursive = TRUE)
  
  # copy ae and rename
  file.copy(file.path(path2extdata, '/emu/DBs/ae/'), tempdir(), recursive = T)
  file.rename(file.path(tempdir(), 'ae'), file.path(tempdir(), 'ae_copy'))
  
  # make copy of ae to mess with (caution correct DBconfig not stored)
  fp = file.path(tempdir(), tmpDbName)
  duplicate.loaded.emuDB("ae", tmpDbName, fp)
  
  test_that("add = (C)RUD", {
    # bad call persp. already exists
    expect_error(add_perspective(dbName=tmpDbName, 
                                 name = 'default'))
    add_perspective(dbName=tmpDbName, 
                    name = 'newPersp')
    
  })
  
  test_that("list = C(R)UD", {
    df = list_perspectives(dbName=tmpDbName)
    
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
    
    remove_perspective(dbName=tmpDbName, 
                       name = 'newPersp')
    
    df = list_perspectives(dbName=tmpDbName)
    expect_equal(nrow(df), 1)
  })
  
  # clean up
  if(is.emuDB.loaded(tmpDbName)){
    UUID = get_emuDB_UUID(dbName = tmpDbName)
    purge_emuDB(dbName = tmpDbName, dbUUID = UUID, interactive = F)
  }
})

##############################
test_that("CRUD operations work for signalCanvasesOrder", {
  # pre clean (just in case)
  unlink(file.path(tempdir(),tmpDbName), recursive = TRUE)
  
  # copy ae and rename
  file.copy(file.path(path2extdata, '/emu/DBs/ae/'), tempdir(), recursive = T)
  file.rename(file.path(tempdir(), 'ae'), file.path(tempdir(), 'ae_copy'))
  
  # make copy of ae to mess with (caution correct DBconfig not stored)
  fp = file.path(tempdir(), tmpDbName)
  duplicate.loaded.emuDB("ae", tmpDbName, fp)
  
  test_that("set = (C)RUD", {
    expect_error(set_signalCanvasesOrder(tmpDbName, 
                                         perspectiveName = "default",
                                         order = c("OSCI", "badTrackName")))
    
    set_signalCanvasesOrder(tmpDbName, 
                            perspectiveName = "default",
                            order = c("OSCI", "SPEC", "fm"))
    
  })
  
  test_that("get = C(R)UD", {
    order = get_signalCanvasesOrder(tmpDbName, perspectiveName = "default")
    
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
  
  # clean up
  if(is.emuDB.loaded(tmpDbName)){
    UUID = get_emuDB_UUID(dbName = tmpDbName)
    purge_emuDB(dbName = tmpDbName, dbUUID = UUID, interactive = F)
  }
})

##############################
test_that("CRUD operations work for levelCanvasesOrder", {
  # pre clean (just in case)
  unlink(file.path(tempdir(),tmpDbName), recursive = TRUE)
  
  # copy ae and rename
  file.copy(file.path(path2extdata, '/emu/DBs/ae/'), tempdir(), recursive = T)
  file.rename(file.path(tempdir(), 'ae'), file.path(tempdir(), 'ae_copy'))
  
  # make copy of ae to mess with (caution correct DBconfig not stored)
  fp = file.path(tempdir(), tmpDbName)
  duplicate.loaded.emuDB("ae", tmpDbName, fp)
  
  test_that("set = (C)RUD", {
    # bad level name
    expect_error(set_levelCanvasesOrder(tmpDbName, 
                                        perspectiveName = "default",
                                        order = c("Phonetic", "badLevelName")))
    
    # bad level type
    expect_error(set_levelCanvasesOrder(tmpDbName, 
                                        perspectiveName = "default",
                                        order = c("Phonetic", "Tone", "Word")))
    
    set_levelCanvasesOrder(tmpDbName, 
                           perspectiveName = "default",
                           order = c("Tone", "Phonetic"))
    
  })
  
  test_that("get = C(R)UD", {
    order = get_levelCanvasesOrder(tmpDbName, perspectiveName = "default")

    expect_equal(order[1], "Tone")
    expect_equal(order[2], "Phonetic")
  })
  
  test_that("modify = CR(U)D", {
    # currently not implemented
  })
  
  test_that("remove = CRU(D)", {
    # currently not implemented
  })
  
  # clean up
  if(is.emuDB.loaded(tmpDbName)){
    UUID = get_emuDB_UUID(dbName = tmpDbName)
    purge_emuDB(dbName = tmpDbName, dbUUID = UUID, interactive = F)
  }
})



