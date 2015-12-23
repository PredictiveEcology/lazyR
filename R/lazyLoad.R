#' Load lazy objects from a \code{lazyR} database
#'
#' Load all named objects into the specified environment.
#'
#' @note Rasters have a special behaviour. If the filename associated with a raster is
#' pointing to a file that doesn't exist, then it will try the prepend the \code{lazyDir} to the
#' filename inside the \code{Raster*} object. The raster filenames can become incorrect if
#' the absolute path to the file changes due to a changing of operating system or moving
#' from a removable drive. For example, a removable drive, which was mapped to, say E: in the
#' computer where the object was originally \code{lazySave}d, gets replugged in but mapped to
#' F:, then the absolute paths will be wrong. This will be automatically corrected, with a warning,
#' if the filename can be found by looking in the relative path \code{/rasters/} below
#' \code{lazyDir}.
#'
#' @param objNames A character vector of object names to load lazily, usually from a lazyLs call.
#'                 If \code{NULL}, all object in lazyDir will be loaded.
#'
#' @param md5Hashes Object hash.
#'
#' @param lazyDir Character string of directory to be used for the lazy databases.
#'
#' @param envir Environment into which the objects are loaded. Default is \code{.GlobalEnv}.
#'
#' @param verbose Logical. If notes about rasters whose file backings were corrected on the
#' fly from previous, i.e,. wrong, filenames.
#'
#' @return Invisibly, the objects read in returned. This function is used for its
#' side effects, i.e., loading lazy objects.
#'
#' @seealso \code{\link{lazyLs}}, \code{\link{lazyRm}}.
#' @docType methods
#' @rdname lazyLoad2
#' @author Eliot McIntire
#' @export
#' @importFrom archivist showLocalRepo
#' @importFrom raster raster
#' @importFrom magrittr %>%
#' @examples
#' library(SpaDES)
#' tmpdir <- checkPath(file.path(tempdir(), "lazyDir"), create=TRUE)
#' obj <- rnorm(10)
#' # save the obj
#' lazySave(obj, lazyDir=tmpdir)
#' # remove the obj
#' rm(obj)
#' any(ls()=="obj") # Is FALSE
#' # load it back in
#' lazyLoad2("obj", lazyDir=tmpdir)
#' any(ls()=="obj") # Is TRUE
#' unlink(tmpdir, recursive = TRUE)
lazyLoad2 <- function(objNames=NULL, md5Hashes=NULL, lazyDir=NULL,
                      envir=parent.frame(), verbose=FALSE) {
  
  obsRead <- character(0)
  hadRasterWrongPath <- FALSE
  notInLazyDir <- character()
  on.exit(expr = {
    message(length(obsRead), " objects loaded of ", length(objNames))
    if(hadRasterWrongPath & !verbose)
      message("Some raster filenames were corrected. Use verbose next time to see details")
    if (length(obsRead)!=length(objNames)) {
      message("Failed on ", paste(objNames[!(objNames %in% obsRead)],collapse=", "))
      if(length(notInLazyDir)>0) 
        message("  because not in lazyDir")
    }
  })
  
  lazyDir <- checkLazyDir(lazyDir = lazyDir, create=FALSE)
  
  if(!is.null(md5Hashes)) {
    objNames <- md5Hashes
  }
  
  if (is.null(objNames) & is.null(md5Hashes)) {
    objNames <- unique(lazyLs(lazyDir = lazyDir))
  }
  
  possibleNewRasterName <- ""
  rasterName <- possibleNewRasterName
  rastersWithChangedName <- character()
  newFilePaths <- character()
  oldFilePaths <- character()
  
  lapply(objNames, function(y) {
    if (any(y == lazyLs(tag="class:Raster", lazyDir=lazyDir))) {
      if(is.null(md5Hashes)) {
        md5Hash <- lazyLs(tag=y, archivistCol="artifact", lazyDir=lazyDir,
                          exact=TRUE) %>% unique
      } else {
        md5Hash <- y
      }
      
      rasterFile <- showLocalRepo(repoDir=lazyDir, method="tags") %>%
        filter(artifact==md5Hash) %>%
        filter(grepl("filename:", tag)) %>%
        select(tag) %>%
        unique
      
      # Test if it had a filename associated with it; if not, then load the rda directly
      if (nchar(gsub(rasterFile, pattern = "filename:", replacement = "")) == 0) {
        lazyLoadFromRepo(md5Hash, lazyDir=lazyDir, objName=y, envir=envir)
      } else {
        
        rasterName <- gsub(rasterFile$tag, pattern="filename:", replacement = "")
        if (file.exists(rasterName)) {
          assign(y, value=raster(rasterName), envir=envir)
        } else {
          possibleNewRasterName <- file.path(lazyDir, "rasters", basename(rasterName))
          if(file.exists(possibleNewRasterName)) {
            hadRasterWrongPath <<- any(TRUE, hadRasterWrongPath)
            assign(y, value=raster(possibleNewRasterName), envir=envir)
            rastersWithChangedName <<- c(rastersWithChangedName, y)
            newFilePaths <<- c(newFilePaths, possibleNewRasterName)
            oldFilePaths <<- c(oldFilePaths, rasterName)
          } else {
            warning("Failed to load file ", rasterName, ".\n")
          }
        }
      }
      obsRead <<- c(obsRead,  y)
    } else {
      if(is.null(md5Hashes)) {
        md5Hash <- lazyLs(tag = y, archivistCol = "artifact",
                          lazyDir = lazyDir, exact = TRUE)
        #md5Hash <- showLocalRepo(method="md5hashes", repoDir = lazyDir)$md5hash
        #md5Hash <- md5Hash[length(md5Hash)]
        
      } else {
        md5Hash <- y
      }
      if(length(md5Hash)>0) {
        lazyLoadFromRepo(md5Hash, lazyDir=lazyDir, objName=y, envir=envir)
        
        obsRead <<- c(obsRead,  y)
      } else {
        notInLazyDir <<- c(notInLazyDir, y)
      }
    }
  })
  
  if(hadRasterWrongPath) {
    if(verbose)
      message("\nThe original files used by ", paste(rastersWithChangedName, collapse=", "),
              " did not exist. These old names (left intact in the lazyR database objects) are\n",
              paste(oldFilePaths, collapse=",\n"),
              "\nWhen lazyLoaded, ",
              "the following filenames were used instead: \n",paste(newFilePaths, collapse=",\n"),
              " \nTo permanently update these in the lazyDir database, please ",
              "use \nlazySave(",paste(rastersWithChangedName, collapse=", "),", overwrite=TRUE, lazyDir=\"",lazyDir,
              "\") \nbut this may be slow.")
  }
  return(invisible(sort(obsRead)))
}

#' Lazy load mechanism
#'
#' This creates the promise to be evaluated later, used internally by \code{lazyLoad2}.
#'
#' @param artifact A character vector of the hash associated with an object in the lazyDir
#'
#' @param lazyDir the lazyDir to use
#'
#' @param objName Must pass the object name as a character string to assign the lazy evaluation to
#'
#' @param envir The environment to assign into (passed to \code{assign.env} arg of \code{delayedAssign})
#'
#' @return An object named objName is assigned into the envir, lazily via \code{delayedAssign}.
#'
#' @importFrom lazyeval lazy lazy_eval
#' @importFrom archivist loadFromLocalRepo
#'
#' @seealso \code{\link{lazyLs}}, \code{\link{lazyLoad2}}
#' @docType methods
#' @author Eliot McIntire
#' @rdname lazyLoadFromRepo
lazyLoadFromRepo <- function(artifact, lazyDir=lazyDir(), objName, envir=parent.frame(1)) {
  loadedObj <- lazy(loadFromLocalRepo(force(artifact), lazyDir, value=TRUE))
  delayedAssign(objName, value = lazy_eval(loadedObj), assign.env = envir)
}

