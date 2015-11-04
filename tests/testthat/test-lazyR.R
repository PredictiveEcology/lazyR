test_that("lazy loading mechanisms don't work", {
  library(lazyR)
  library(archivist)
  library(magrittr)
  library(raster)
  library(testthat)

  exampleRepoDir <- file.path(tempdir(), "lazyDir") %>%
    normalizePath(., winslash = "/", mustWork = FALSE)
  lazyDir <- lazyDir(exampleRepoDir, create=TRUE)
  
  expect_equal(exampleRepoDir, lazyDir)

  deleteRepo( exampleRepoDir )
  createEmptyRepo( repoDir = exampleRepoDir )

  expect_equal(nrow(showLocalRepo(method = "md5hashes")), 0)
  a <- 1:3e6
  env <- environment()

  # expect no error:
  #  if error thrown in lazySave, TRUE won't eval
  expect_true({ lazySave(a); TRUE })

  expect_equal(length(lazyLs("b", exact=TRUE)), 0)

  expect_true(lazyExists("a"))
  expect_message(lazyLoad2("a"), "1 objects loaded of 1")

  artifact <- showLocalRepo(method = "md5hashes")$md5hash
  expect_equal(artifact, "2e753e7c1e97de88dd03e21d3855e115")

  # test that lazy loading is faster
  # loads "obj" because it was mis-named during lazySave because archivist
  # has the wrong naming mechanism
  rm(a, envir = env)
  timeArchivist <- system.time({
    loadFromLocalRepo(artifact, repoDir = lazyDir, value = FALSE)
  })
  timelazyLoad <- system.time({lazyLoad2("a", lazyDir = lazyDir)})
  expect_equal(a, obj)
  expect_less_than(timelazyLoad[1], timeArchivist[1])

  expect_message(lazyRm("a", lazyDir=lazyDir), "Object removed: a")

  # Test assignCache
  lazyRm("a")
  if (packageVersion("testthat") >= "0.11.0") {
    expect_silent(a %<% 1:10)
  }
  expect_message(a %<% 1:10, "1 objects loaded of 1")


})

test_that("Test raster mechanisms", {
    # Test raster mechanisms
  env <- environment()
  expect_message(lazyRm("r"), "r not in lazy load db. Nothing removed")

  # no file backing
  if (packageVersion("testthat") >= "0.11.0") {
    expect_silent(r %<% raster::raster(matrix(1:9e4, ncol = 3e2)))
  }

  # with file backing
  rasterFile <- file.path(tempdir(), "r.tif") %>%
    normalizePath(., winslash = "\\", mustWork = FALSE)
  raster::writeRaster(r, rasterFile, overwrite = TRUE)
  rm(r, envir = env)
  r <- raster(rasterFile)
  expect_equal(filename(r), rasterFile)
  lazySave(r, overwrite = TRUE)
  rm(r, envir = env)
  expect_false(exists("r", envir = env))
  expect_message(lazyLoad2("r"), "1 objects loaded")
  fname <- file.path(dirname(rasterFile), "lazyDir", "rasters", basename(rasterFile)) %>%
    normalizePath(., winslash = "\\", mustWork = FALSE)
  expect_equal(filename(r), fname)

#     r1 %<% raster::raster(matrix(1:9, ncol=3))
#
#     lazyDir(oldLazyDir)
#     #' # make some objects
#     obj1 <- 1:10
#     obj2 <- 11:20
#     r %<% raster::raster(matrix(1:9e4, ncol=3e2))
#     ras %<% raster::raster(matrix(1:9e4, ncol=3e2))
#
#     raster::writeRaster(r, file.path(tempdir(),"r.tif"), overwrite=TRUE)
#     raster::writeRaster(r, file.path(tempdir(),"ras.tif"), overwrite=TRUE)
#     rm(r, ras)
#     r %<% raster::raster(file.path(tempdir(),"r.tif"))
#     ras %<% raster::raster(file.path(tempdir(),"ras.tif"))
#     copyLazyDir(oldLazyDir, newLazyDir)
#
#     rm(r, ras)
#     unlink(oldLazyDir)
#
#     r %<% raster::raster(file.path(tempdir(),"r.tif"))
#     ras %<% raster::raster(file.path(tempdir(),"ras.tif"))
#     library(SpaDES)
#     Plot(r, ras, new=T)
#
#   r

})
