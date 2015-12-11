#' Set and get the lazyR database directory
#'
#' This can be set and gotten with these functions, or all functions can take
#' an argument, lazyDir, manually. \code{lazyDir()} is a shorthand for \code{lazyDir()}.
#' \code{lazyDir("someDir")} is a shorthand for \code{lazyDir("someDir")}
#'
#' @param lazyDir Character string of directory to be used for the lazy databases.
#' If \code{lazyDir=NULL}, then it removes the active lazy directory. If NA, the default,
#' returns the currently set lazyDir.
#'
#' @param create Logical, passed to checkLazyDir, i.e., it will create dir if it doesn't exist.
#' Default is FALSE.
#'
#' @return New lazyDir created (for \code{lazyDir}) or currently active lazyDir returned.
#'
#' @importFrom archivist setLocalRepo
#'
#' @seealso \code{\link{lazyLs}}, \code{\link{lazyLoad2}}
#' @docType methods
#' @author Eliot McIntire
#' @rdname lazyDir
#' @examples
#' lazyDir() # Returns NULL if not set
#'
#' # Set new lazyDir
#' lazyDir(file.path(tempdir(), "lazyDir"), create=TRUE)
#'
#' # Check new value
#' lazyDir()
#'
#' a <- rnorm(10)
#' lazySave(a) # add to lazyDir
#' lazyRm("a") # remove object from lazyDir
#'
#' lazyDir(NULL) # removes currently set lazyDir
#' lazyDir()  # confirm removal
#' unlink(file.path(tempdir(), "lazyDir"), recursive=TRUE) # remove folder and lazyDir
.setLazyDir <- function(lazyDir, create=FALSE) {
  
  #lazyDir <- checkLazyDir(lazyDir, create = create)
  
  stopifnot(is.character(lazyDir))
  if (!dir.exists(lazyDir)) {
    dir.create(lazyDir)
  }
  lazyDir <- normalizePath(lazyDir, winslash = "/")
  
  assign(".lazyDir", lazyDir, envir = .lazyREnv)
  
  # Must set the archivist location too, for internal archivist functions:
  setLocalRepo(lazyDir)
  
  message(paste("Local lazyR database set to", lazyDir))
  return(invisible(lazyDir))
}

#' @rdname lazyDir
.getLazyDir <- function() {
  if (exists(".lazyDir", envir = .lazyREnv)) {
    get(".lazyDir", envir = .lazyREnv) %>%
      gsub(pattern = "/$", x=., replacement = "") %>%
      gsub(pattern = "\\$", x=., replacement = "")
  } else {
    NULL
  }
}


#' @rdname lazyDir
#' @export
lazyDir <- function(lazyDir=NA, create=FALSE) {
  
  if(is.null(lazyDir)) {
    if(exists(".lazyDir", envir = .lazyREnv)) {
      rm(".lazyDir", envir = .lazyREnv)
    }
    return(NULL)
  }
  
  if(is.na(lazyDir))
    return(.getLazyDir())
  
  lazyDir <- checkLazyDir(lazyDir)
  
  return(invisible(.setLazyDir(lazyDir, create=create)))
}


#' Checks and sets for lazyDir
#'
#' @param lazyDir Character string of directory to be used for the lazy databases.
#'
#' @param create A logical. If \code{TRUE}, it will create the directory specified with \code{lazyDir}
#'
#' @return Directory of lazyDir. If it was not specified, then it returns a
#' temp directory
#'
#' @docType methods
#' @author Eliot McIntire
#' @rdname checkLazyDir
#' @importFrom archivist createEmptyRepo
#' @export
#' @examples
#' \dontrun{
#' checkLazyDir(create=TRUE) # because missing, it will provide a tmpdir
#' unlink(file.path(tempdir(), "lazyDir"), recursive=TRUE)
#' }
checkLazyDir <- function(lazyDir=NULL, create=TRUE) {
  # check that lazyDir is specified, if not, use lazyDir, if still nothing, then use temp
  if(missing(lazyDir)) lazyDir <- NULL
  if (is.null(lazyDir)) {
    lazyDir= lazyDir()
    if (is.null(lazyDir)) {
      lazyDir <- tryCatch(checkPath(file.path(tempdir(), "lazyDir"), create=create),
                          error=function(x) NULL)
      if (is.null(lazyDir)) {
        stop("Please specify a lazyDir that exists, or set it via lazyDir(). Nothing to do.")
      }
      message("Setting lazy directory via lazyDir(", lazyDir, ")")
      .setLazyDir(lazyDir, create=create)
    }
  }
  
  if (create) {
    if (!dir.exists(lazyDir)) {
      dir.create(lazyDir, showWarnings = FALSE)
      createNewRepo = TRUE
    } else {
      if (file.exists(file.path(lazyDir,"backpack.db"))) {
        if (file.info(file.path(lazyDir,"backpack.db"))$size == 0) {
          createNewRepo = TRUE
        } else {
          createNewRepo = FALSE
        }
      } else {
        createNewRepo = TRUE
      }
    }
    #      createNewRepo = TRUE
    if(createNewRepo) {
      createEmptyRepo(lazyDir)
    }
  } else { # if create is false
    if (!dir.exists(lazyDir)) {
      warning(paste0("The lazyDir, ", lazyDir,", does not exist.",
                     "Please specify the lazyDir argument or with lazyDir()."))
    } else {
      if (file.exists(file.path(lazyDir,"backpack.db"))) {
        if (file.info(file.path(lazyDir,"backpack.db"))$size == 0) {
          createNewRepo = TRUE
        }
      } else {
        stop(paste0("The lazyDir, ", lazyDir,", exists, but is not a lazy directory. ",
                    "Please create it with create=TRUE."))
      }
    }
  }
  return(lazyDir)
}

# The following is an attempt to rewrite the cache function to work
#  with lazyR databases. It doesn't work at this point because we
# can't do a lazy return of an
# object. We can only do delayedAssign, which can't be used within a
# return() call.  Therefore, we can't return the lazy object in an
# <- assignment call. Also, because all objects are given the default object
# name via a substitute call in the saveToRepo function, every object has
# the same name, i.e., output in the case below.
# @export
# @importFrom lazyeval lazy_
# @importFrom pryr %d-%
# lazyCache <- function(lazyDir, FUN, ..., notOlderThan=NULL)
# #function (cacheRepo = NULL, FUN, ..., notOlderThan = NULL)
# {
#   tmpl <- list(...)
#   tmpl$.FUN <- FUN
#   outputHash <- digest(tmpl)
#   localTags <- showLocalRepo(lazyDir, "tags")
#   isInRepo <- localTags[localTags$tag == paste0("cacheId:",
#                                                 outputHash), , drop = FALSE]
#   if (nrow(isInRepo) > 0) {
#     lastEntry <- max(isInRepo$createdDate)
#     if (is.null(notOlderThan) || (notOlderThan < lastEntry)) {
#       lastOne <- order(isInRepo$createdDate, decreasing = TRUE)[1]
#       lazyLoad2(md5Hashes=lazyLs(paste0("cacheId:",outputHash),
#                               archivistCol="artifact", lazyDir=lazyDir),
#                        lazyDir = lazyDir, envir=environment())
#       return(output) # all cached objects are called output
#     }
#   }
#   output <- do.call(FUN, list(...))
#   attr(output, "tags") <- paste0("cacheId:", outputHash)
#   attr(output, "call") <- ""
#   lazySave(output, lazyDir=lazyDir,
#            tags=paste0("cacheId:", outputHash),
#            overwrite=TRUE)
# #   saveToRepo(output, repoDir = cacheRepo, archiveData = TRUE,
# #              archiveMiniature = FALSE, rememberName = FALSE, silent = TRUE)
#   output
# }




#' @rdname lazyDir
#' @export
setLazyDir <- function(lazyDir, create=TRUE) {
  .Deprecated("lazyDir", "lazyR")
  lazyDir(lazyDir=lazyDir, create=create)
}

#' @rdname lazyDir
#' @export
getLazyDir <- function(lazyDir, create=TRUE) {
  .Deprecated("lazyDir()", "lazyR")
  lazyDir()
}
