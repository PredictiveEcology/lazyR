#' Alternative to saveToRepo for rasters
#'
#' Rasters are sometimes file-based, so the normal save mechanism doesn't work. This function creates an
#' explicit save of the file that is backing the raster, in addition to saving the object metadata in
#' the archivist repository database.
#'
#' @param obj The raster object to save to the repository.
#'
#' @param objName A character representation of the object name.
#'
#' @param lazyDir the lazyDir to use.
#'
#' @param tags Optional character vector of tags. Passed to \code{saveToRepo}.
#'
#' @param compareRasterFileLength Numeric. This is passed to the length arg in \code{digest}
#' when determining if the Raster file is already in the database. Default 1e6. Passed to \code{lazySave}.
#'
#' @return A raster object and its file backing will be passed to the archivist repository.
#'
#' @importFrom digest digest
#' @importFrom archivist saveToRepo
#'
#' @seealso \code{\link{lazyLs}}, \code{\link{lazyLoad2}}
#' @docType methods
#' @author Eliot McIntire
#' @rdname saveToRepoRaster
#' @export
saveToRepoRaster <- function(obj, objName=NULL, lazyDir=NULL, tags=NULL, compareRasterFileLength=1e6) {
  
  if(is.null(objName))
    objName <- deparse(substitute(obj))
  lazyDir <- checkLazyDir(lazyDir)
  if (!inMemory(obj)) {
    curFilename <- normalizePath(filename(obj), winslash = "/")
    
    saveFilename <- file.path(lazyDir, "rasters", basename(curFilename)) %>%
      normalizePath(., winslash = "/", mustWork=FALSE)
    
    if (saveFilename!=curFilename) {
      shouldCopy <- TRUE
      if (file.exists(saveFilename)) {
        if (!(compareRasterFileLength==Inf)) {
          if (digest(file = saveFilename, length=compareRasterFileLength) ==
              digest(file = curFilename, length=compareRasterFileLength)) {
            shouldCopy <- FALSE
          }
        } else {
          shouldCopy = TRUE
        }
      }
      if (shouldCopy) {
        pathExists <- file.exists(dirname(saveFilename))
        if (!pathExists) dir.create(dirname(saveFilename))
        if(saveFilename %>% grepl(., pattern=".grd$")) {
          copyFile(to = saveFilename, overwrite = TRUE,
                   from = curFilename)
          griFilename <- sub(saveFilename, pattern=".grd$", replacement = ".gri")
          curGriFilename <- sub(curFilename, pattern=".grd$", replacement = ".gri")
          copyFile(to = griFilename, overwrite = TRUE,
                   from = curGriFilename)
          #           file.copy(to = griFilename, overwrite = TRUE,
          #                     recursive = FALSE, copy.mode = TRUE,
          #                     from = curGriFilename)
        } else {
          copyFile(to = saveFilename, overwrite = TRUE,
                   from = curFilename)
        }
      }
      slot(slot(obj, "file"), "name") <- saveFilename
    }
  } else {
    saveFilename <- slot(slot(obj, "file"), "name")
  }
  
  saveToRepo(obj, repoDir = lazyDir,
             userTags = c(paste0("objectName:", objName), tags,
                          paste0("class:", is(obj)),
                          paste0("filename:", saveFilename)
             ))
  
}
