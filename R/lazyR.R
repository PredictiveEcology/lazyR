if (getRversion() >= "3.1.0") {
  utils::globalVariables(c(".", "artifact", "lazyDir", "raster", "tag"))
}

################################################################################
#' Save objects to a \code{lazyR} database
#'
#' This function creates an archivist repository that holds metadata ("tags")
#' associated with the object. Unlike the \code{archivist} package, the objects
#' themselves are saved as lazy load databases (i.e., via tools:::makeLazyLoadDB).
#' Each object is saved into its own lazy load database, with a filename that
#' matches the object name. Because it is also an archivist repository, lazy
#' loading of objects can be done via tags, classes etc..
#'
#' The main use case for this is large, complicated datasets, such as GIS databases, that
#' are used within R. Loading them lazily means that the user can have access to all of them,
#' including their characteristics, without loading them all first (could be very slow).
#' When \code{copyRasterFile} is \code{TRUE}, it may take a while as the original source
#' file is copied. Also, the slot in the raster file that indicates the file pointer
#' is switched to new location. This allows more modular lazyR database, i.e., if the Raster
#' files are copied into the lazyR database, then the whole database contains everything needed
#' to copy to another machine. Note: If \code{copyRasterFile} is TRUE, but the file is already
#' there and is identical (using \code{\link[digest]{digest}}), then no copy is done.
#'
#' When \code{copyRasterFile} is \code{TRUE}, it will actually run a \code{digest}
#' command on the file, if it already exists in the rasters directory of the lazyR database.
#' If this hash resulting from the \code{digest} indicates that the file is already there and
#' it is identical, then it will not actually be copied. This will be good for speed, but
#' will miss small changes in files. Set \code{compareRasterFileLength} to \code{Inf} if
#' truly exact comparisons behaviour is needed.
#'
#' @param ... Objects to save to an R lazy database, with metadata saved via archivist package repository.
#'
#' @param lazyDir Character string of directory to be used for the lazy databases.
#' This creates an archivist repository.
#'
#' @param tags Character vector of additional tags to add to the objects being saved
#'
#' @param clearRepo Logical. Should the repository be deleted before adding new objects
#'
#' @param overwrite Logical. Should new object replace (i.e., overwrite) previous object with same name
#'
#' @param copyRasterFile Logical. Should the source file for rasters be copied into the lazyDir.
#' Default TRUE, which may be slow. See Details.
#'
#' @param compareRasterFileLength Numeric. This is passed to the length arg in \code{digest}
#' when determining if the Raster file is already in the database. Default 1e6. See Details.
#'
#' @return invisibly returns a character vector of objects saved.
#'
#' @seealso \code{\link{lazyLs}}, \code{\link{lazyRm}}, \code{\link{lazyLoad2}}.
#' @docType methods
#' @rdname lazySave
#' @author Eliot McIntire
#' @export
#' @importClassesFrom SpaDES spatialObjects
#' @importFrom raster crs inMemory filename
#' @importFrom archivist deleteRepo createEmptyRepo saveToRepo addTagsRepo
#' @importFrom magrittr %>%
#' @importFrom digest digest
#' @importFrom SpaDES checkPath
#' @examples
#' a <- rnorm(10)
#' b <- rnorm(20)
#' lazySave(a, b)
#' \dontrun{ # may be many objects
#' lazySave(mget(ls(envir=.GlobalEnv)))
#' unlink(file.path(tempdir(), "lazyDir"), recursive=TRUE)
#' }
lazySave <- function(..., lazyDir=NULL, tags=NULL, clearRepo=FALSE,
                     overwrite=FALSE, copyRasterFile=TRUE,
                     compareRasterFileLength=1e6) {
  objList <- list(...)
  objNames <- NULL


#   if(is.null(lazyDir)) {
#     lazyDir <- checkPath(file.path(tempdir(), "lazyDir"), create=TRUE)
#     message("Lazy directory is ", lazyDir, ". It will only persist for this R session")
#   }

  if (is(objList[[1]], "list")) {
    objList <- objList[[1]]
  }
  if (!is.null(names(objList))) {
    if (length(names(objList)) == length(objList)) {
      objNames <- names(objList)
    } else {
      stop("If passing a list, it must be a named list.")
    }
  }
  if (is.null(objNames)) {
    objNames <- sapply(as.list(substitute(list(...)))[-1L], deparse)
  }
  #objList <- list(obj)
  names(objList) <- objNames

  lazyDir <- checkLazyDir(lazyDir, create=TRUE)
#   if (!is.null(getLazyDir())) {
#     lazyDir <- getLazyDir()
#   }
#
#   if (is.null(lazyDir)) {
#     lazyDir <- checkPath(file.path(tempdir(), "lazyDir"), create=TRUE)
#     message("Lazy directory is ", lazyDir, ". It will only persist for this R session")
#   }
#   checkPath(lazyDir, create = TRUE)
  #dir.create(lazyDir, showWarnings = FALSE)

  if (!is.null(lazyDir)) {
    if (clearRepo) {
      deleteRepo(lazyDir)
      Sys.sleep(0.1) # over a slow network, it is possible that the deleteRepo wasn't complete yet
    }
    if (!file.exists(file.path(lazyDir, "backpack.db"))) {
      createEmptyRepo(lazyDir)
    }

    lapply(1:length(objList), function(N) {
      shouldSave <- TRUE
      obj <- objList[[N]]
      objName <- names(objList[N])
      firstOne <- any(
        lazyLs(tag = objName, lazyDir = lazyDir, exact=TRUE) == objName
      )
      if (firstOne) {
        if (!overwrite) {
          message(paste("Object", objName, "is already in the database.",
                  "Use overwrite=TRUE to replace it."))
          shouldSave <- FALSE
        } else {
          lazyRm(objName)
        }
      }
      if (shouldSave) {
        if (is(obj, "Raster")) {
          if (copyRasterFile & !inMemory(obj)) {
            curFilename <- normalizePath(filename(obj), winslash = "/")
            checkPath(file.path(lazyDir, "rasters"), create=TRUE)

            saveFilename <- file.path(getLazyDir(), "rasters", basename(curFilename)) %>%
              normalizePath(., winslash = "/", mustWork=FALSE)

            if (saveFilename!=curFilename) {
              shouldCopy <- TRUE
              if (file.exists(saveFilename)) {
                if (!(compareRasterFileLength==Inf)) {
                  if (digest(file = saveFilename, length=compareRasterFileLength)==
                       digest(file = curFilename, length=compareRasterFileLength)) {
                    shouldCopy <- FALSE
                  }
                } else {
                  shouldCopy = TRUE
                }
              }
              if(shouldCopy) {
                pathExists <- file.exists(dirname(saveFilename))
                if(!pathExists) dir.create(dirname(saveFilename))
                file.copy(to = saveFilename, overwrite = TRUE,
                        recursive = FALSE, copy.mode = TRUE,
                        from = curFilename)
              }
              slot(slot(objList[[N]], "file"), "name") <- saveFilename
            }
          } else {
            saveFilename <- slot(slot(obj, "file"), "name")
          }
          saveToRepo(objName, repoDir = lazyDir,
                     userTags = c(paste0("objectName:", objName), tags,
                                  paste0("class:", is(obj)),
                                  paste0("filename:", saveFilename)
                                ))
        } else {
          saveToRepo(objName, repoDir = lazyDir,
                   userTags = c(paste0("objectName:", objName), tags,
                                paste0("class:", is(obj))))
        }

        md5Hash <- lazyLs(tag=objName, archivistCol = "artifact", lazyDir = lazyDir, exact = TRUE)

        # Add tags by class
        if (is(obj, "spatialObjects")) addTagsRepo(md5Hash, tags=paste0("crs:",crs(obj)),
                                                  repoDir = lazyDir)

      # Save the actual objects as lazy load databases
        list2env(x = objList[N]) %>%
          tools:::makeLazyLoadDB(., file.path(lazyDir,"gallery", md5Hash))
      }
    })
  }
  return(invisible(names(objList)))
}

#' List contents of lazy load database, using a tag
#'
#' This will simply list object names that are contained within an archivist
#' lazy load database. The listing can be done with a tag.
#'
#' @param tag Character string (not vectorized!). Search objects using this tag as a pattern.
#'
#' @param lazyDir Character string of directory to be used for the lazy databases.
#' This creates an archivist repository.
#'
#' @param tagType Some tags have prefixes, such as "class:".
#' The search can be thus within a tagType if this is specified.
#' If "all", then all tag types are returned.
#'
#' @param archivistCol The name of the column to return from a showLocalRepo call.
#' This can be "tag" or "artifact" or "createdDate".
#'
#' @param exact Logical. Should the tag matching be exact matching or not.
#'
#' If exact=TRUE is used, this is equivalent to a regexp expression with ^expr$.
#'
#' @return A character vector that matches the tagType, tag, and archivistCol requested.
#'
#' @seealso \code{\link{lazyLs}}, \code{\link{lazyRm}}, \code{\link{lazyLoad2}},
#' \code{\link{lazyIs}}.
#' @docType methods
#' @rdname lazyLs
#' @author Eliot McIntire
#' @export
#' @importFrom dplyr select select_ filter distinct left_join distinct_
#' @importFrom archivist showLocalRepo
#' @importFrom magrittr %>%
#' @importFrom SpaDES checkPath
#' @examples
#' \dontrun{
#' library(SpaDES)
#' tmpdir <- checkPath(file.path(tempdir(), "lazyDir"), create=TRUE)
#' a <- rnorm(10)
#' b <- rnorm(20)
#' lazySave(a, b, lazyDir = tmpdir)
#' lazyLs(lazyDir=tmpdir)
#'
#' # can set lazyDir and don't need lazyDir argument
#' setLazyDir(tmpdir)
#' lazyLs()
#'
#' # can add a tag to file list
#' lazyLs(tag = "function") # empty character vector
#' lazyLs(tag = "numeric") # object names of a and b
#'
#' # To return the values of different tags, use the tagType argument
#' lazyLs(tagType="objectName:") # Just object names, the default
#' lazyLs(tagType="class:")      # All classes in the database,
#'                               #returns multiple lines per object
#' lazyLs(tagType="all")         # returns all information in the database
#' unlink(tmpdir, recursive = TRUE)
#' }
lazyLs <- function(tag=NULL, lazyDir=NULL,
                   tagType="objectName:",
                   archivistCol="tag",
                   exact=FALSE) {

  if (tagType=="all") {
    tagTypeAll <- TRUE
    tagType <- ""
  } else {
    tagTypeAll <- FALSE
  }

  # check that lazyDir is specified, if not, use getLazyDir, if still nothing, then use temp
  lazyDir <- checkLazyDir(lazyDir)
  b <- showLocalRepo(repoDir=lazyDir, method="tags") %>%
    filter(grepl(pattern=tagType, tag)) %>%
    distinct_("artifact", "tag") #%>%
    #select_("artifact", archivistCol)

  if (!is.null(tag)) {

    tag2 <- tag # creates confusion in dplyr because tag is a column name in
                # showLocalRepo and an argument in this function
    if (exact) {
      a <- showLocalRepo(repoDir=lazyDir, method="tags") %>%
        filter(grepl(paste0("^",tagType,tag2,"$"), tag)) %>%
        select_("artifact")
    } else {
      a <- showLocalRepo(repoDir=lazyDir, method="tags") %>%
        filter(grepl(paste0(tag2), tag)) %>%
        select_("artifact")
    }

    b <- left_join(a, b, by="artifact") %>%
      distinct_
  }
  if (tagTypeAll) {
    out <- b
  } else {
    a <- showLocalRepo(repoDir=lazyDir, method="tags") %>%
      filter(grepl(paste0(tag2), tag)) %>%
      select_("artifact")
  }

  b <- left_join(a, b, by="artifact") %>% distinct_
  
  if (tagTypeAll) {
    out <- b
  } else {
    out <- gsub(x = b[, archivistCol], pattern = tagType, replacement = "") %>% sort
  }
  return(out)
}

#' Load lazy objects from a \code{lazyR} database
#'
#' Load all named objects into the specified environment.
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
#' @return Invisibly, the objects read in returned. This function is used for its side effects, i.e., loading lazy objects.
#'
#' @seealso \code{\link{lazyLs}}, \code{\link{lazyRm}}.
#' @docType methods
#' @rdname lazyLoad2
#' @author Eliot McIntire
#' @export
#' @importFrom archivist showLocalRepo
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
lazyLoad2 <- function(objNames=NULL, md5Hashes=NULL, lazyDir=NULL, envir=parent.frame()) {
  
  obsRead <- character(0)
  on.exit(expr = {
    message(length(obsRead), " objects loaded of ", length(objNames))
    if (length(obsRead)!=length(objNames)) {
      message("Failed on ", objNames[!(objNames %in% obsRead)][1])
    }})

  lazyDir <- checkLazyDir(lazyDir = lazyDir, create=FALSE)
#   if (exists(".lazyDir", envir = .lazyREnv)) {
#     lazyDir <- get(".lazyDir", envir = .lazyREnv) %>%
#       gsub(pattern = "/$", x=., replacement = "")
#   }
#
#   if (is.null(lazyDir)) {
#     lazyDir <- checkPath(file.path(tempdir(), "lazyDir"))
#   }

  if(!is.null(md5Hashes)) {
    objNames <- md5Hashes
  }
  if (is.null(objNames) & is.null(md5Hashes)) {
    objNames <- unique(lazyLs(lazyDir = lazyDir))
  }

  lapply(objNames, function(y) {
    if (any(y == lazyLs(tag="class:Raster", lazyDir=lazyDir))) {
      if(is.null(md5Hashes)) {
        md5Hash <- lazyLs(tag=y, archivistCol="artifact", lazyDir=lazyDir,
                        exact=TRUE)
      } else {
        md5Hash <- y
      }

      rasterFile <- showLocalRepo(repoDir=lazyDir, method="tags") %>%
        filter(artifact==md5Hash) %>%
        filter(grepl("filename:", tag)) %>%
        select(tag)

      # Test if it had a filename associated with it; if not, then load the rdx directly
      if (nchar(gsub(rasterFile, pattern = "filename:", replacement = "")) == 0) {
        lazyLoad(file.path(lazyDir, "gallery", md5Hash), envir = envir)
      } else {
        rasterName <- gsub(rasterFile$tag, pattern="filename:", replacement = "")
        if (file.exists(rasterName)) {
          assign(y, value=raster(rasterName), envir=envir)
        } else {
          warning("Failed to load file ", rasterName, ".\n")
        }
      }
      obsRead <<- c(obsRead,  y)
    } else {
      if(is.null(md5Hashes)) {
        md5Hash <- lazyLs(tag = y, archivistCol = "artifact",
                          lazyDir = lazyDir, exact = TRUE)
      } else {
        md5Hash <- y
      }
      lazyLoad(file.path(lazyDir, "gallery", md5Hash), envir = envir)

      obsRead <<- c(obsRead,  y)
    }
  })

  return(invisible(sort(obsRead)))
}

#' Load lazy objects from a \code{lazyR} database
#'
#' Load all named objects into the specified environment.
#'
#' @param objNames A character vector of names of objects to be removed from a \code{lazyR} database.
#'
#' @param lazyDir Character string of directory to be used for the lazy databases.
#'
#' @param exact Should the character string matching be exact, i.e., ^objNames$ in regexp.
#'
#' @param removeRasterFile Logical. If true then the source raster file will be removed in addition to the
#' database entry. This is only true if the file is in the lazyDir
#'
#' @return Nothing returned. This function is used for its side effects, i.e., loading lazy objects
#'
#' @seealso \code{\link{lazyLs}}, \code{\link{lazyLoad2}}
#' @docType methods
#' @rdname lazyRm
#' @author Eliot McIntire
#' @export
#' @importFrom archivist rmFromRepo
#' @importFrom raster filename
#' @examples
#' \dontrun{
#' a <- rnorm(10)
#' lazySave(a, lazyDir=tempdir())
#' lazyRm("a", lazyDir=tempdir())
#' }
lazyRm <- function(objNames=NULL, lazyDir=NULL, exact=TRUE, removeRasterFile=FALSE) {

  lazyDir <- checkLazyDir(lazyDir)
#   if (exists(".lazyDir", envir = .lazyREnv)) {
#     lazyDir <- get(".lazyDir", envir = .lazyREnv) %>%
#       gsub(pattern = "/$", x=., replacement = "")
#   }

  if (is.null(objNames)) {
    objNames <- unique(lazyLs(lazyDir = lazyDir))
  }

  lapply(objNames, function(y) {
    z <- lazyLs(y, archivistCol = "artifact", lazyDir=lazyDir, exact=exact)
    if (length(z)>0) {
      for (toRm in z) {
        objName <- lazyObjectName(toRm, lazyDir=lazyDir)

        if (lazyIs(objName, "Raster", lazyDir = lazyDir) & removeRasterFile) {
          tmpEnv <- new.env()
          lazyLoad2(objName, envir = tmpEnv)
          file.remove(filename(tmpEnv[[objName]]))
        }

        rmFromRepo(toRm, repoDir = lazyDir)
        unlink(file.path(lazyDir, "gallery", paste0(toRm, ".rdx")))
        unlink(file.path(lazyDir, "gallery", paste0(toRm, ".rdb")))
        message(paste("Object removed:", objName))
      }
    } else {
      message(y, " not in lazy load db. Nothing removed.")
    }

  })
  return(invisible())
}


#' Set and get the lazyR database directory
#'
#' This can be set and gotten with these functions, or all functions can take
#' an argument, lazyDir, manually.
#'
#' @param lazyDir Character string of directory to be used for the lazy databases.
#' If \code{lazyDir=NULL}, then it removes the active lazy directory.
#'
#' @param create Logical, passed to checkLazyDir, i.e., it will create dir if it doesn't exist.
#' Default is FALSE.
#'
#' @return New lazyDir
#'
#' @importFrom archivist setLocalRepo
#'
#' @seealso \code{\link{lazyLs}}, \code{\link{lazyLoad2}}
#' @docType methods
#' @author Eliot McIntire
#' @rdname lazyDir
#' @export
#' @examples
#' \dontrun{
#' setLazyDir(file.path(tempdir(), "lazyDir"), create=TRUE)
#' a <- rnorm(10)
#' lazySave(a)
#' lazyRm("a")
#' unlink(file.path(tempdir(), "lazyDir"), recursive=TRUE)
#' }
setLazyDir <- function(lazyDir, create=FALSE) {

  lazyDir <- checkLazyDir(lazyDir, create = create)

#   if (is.null(lazyDir)) {
#     if (exists(".lazyDir", envir = .lazyREnv)) {
#       rm(".lazyDir", envir = .lazyREnv)
#       return("removed lazyDir")
#     } else {
#       return("LazyDir was not set. Nothing to remove")
#     }
#   }
  stopifnot(is.character(lazyDir))
  if (!file.exists(lazyDir)) {
    dir.create(lazyDir)
  }
  lazyDir <- normalizePath(lazyDir, winslash = "/")
  assign(".lazyDir", lazyDir, envir = .lazyREnv)

  # Must set the archivist location too, for internal archivist functions:
  setLocalRepo(lazyDir)

  return(paste("Local lazyR database set to", lazyDir))
}

#' @rdname lazyDir
#' @export
getLazyDir <- function() {
  if (exists(".lazyDir", envir = .lazyREnv)) {
    get(".lazyDir", envir = .lazyREnv) %>%
      gsub(pattern = "/$", x=., replacement = "") %>%
      gsub(pattern = "\\$", x=., replacement = "")
  } else {
    NULL
  }
}

#' The lazyREnv environment
#'
#' Environment used internally to store active lazyDir
#'
#' @rdname lazyREnv
.lazyREnv <- new.env(parent=emptyenv())


#' Get object name with an md5hash
#'
#' @param md5Hash A character string indicating the md5hash value to look up the object name
#'
#' @param lazyDir Character string of directory to be used for the lazy databases.
#'
#' @return An object name
#'
#' @importFrom archivist showLocalRepo
#' @importFrom dplyr filter select_
#'
#' @seealso \code{\link{lazyLs}}, \code{\link{lazyLoad2}}
#' @docType methods
#' @author Eliot McIntire
#' @rdname lazyObjectName
#' @export
#' @examples
#' \dontrun{
#' a <- rnorm(10)
#' lazySave(a) # default uses an tempdir() call for location
#' hash <- lazyLs("a", archivistCol = "artifact", exact=TRUE)
#' objName <- lazyObjectName(hash)
#' unlink(file.path(tempdir(), "lazyDir"), recursive=TRUE)
#' }
lazyObjectName <- function(md5Hash, lazyDir=NULL) {
  lazyDir <- checkLazyDir(lazyDir)
  lazyObjectName <- showLocalRepo(method="tags", repoDir = lazyDir) %>%
    filter(artifact==md5Hash) %>%
    filter(grepl(pattern="objectName:", tag)) %>%
    select_("tag") %>%
    gsub(.$tag, pattern="objectName:", replacement="")
  return(lazyObjectName)

}

#' Get object class
#'
#' @param objName A character string indicating the object name
#'
#' @param class2 An optional character string of the class to test against
#'
#' @param removeCharacter Logical. There is an artifact tag, class:character for any
#' non-character object. This will be removed automatically in the return, unless this
#' is FALSE
#'
#' @param lazyDir Character string of directory to be used for the lazy databases.
#'
#' @return A character vector equivalent to the an \code{is(objName)} command
#'
#' @importFrom dplyr filter select_ as.tbl
#'
#' @seealso \code{\link{lazyLs}}, \code{\link{lazyLoad2}}
#' @docType methods
#' @author Eliot McIntire
#' @rdname lazyIs
#' @export
#' @examples
#' setLazyDir(file.path(tempdir(), "lazyDir"), create=TRUE)
#' a <- rnorm(10)
#' lazySave(a)
#' lazyIs("a", "numeric")
#' lazyIs("a") # shows all
#' lazyRm("a")
#'
#' \dontrun{
#' lazyIs("b") # error, does not exist
#' }
#'
#' unlink(file.path(tempdir(), "lazyDir"), recursive=TRUE)
lazyIs <- function(objName, class2=NULL, removeCharacter=TRUE, lazyDir=NULL) {

  lazyDir <- checkLazyDir(lazyDir)
#   if (is.null(lazyDir)) {
#     lazyDir= getLazyDir()
#   }
  out <- lazyLs(tagType = "all", lazyDir=lazyDir)
  if (length(lazyLs(objName, exact=TRUE,
            archivistCol = "artifact", lazyDir=lazyDir))>0) {
    out <- out %>%
      filter(artifact==lazyLs(objName, exact=TRUE,
                              archivistCol = "artifact", lazyDir=lazyDir)) %>%
      filter(grepl(pattern="class", tag)) %>%
      select_("tag") %>%
      gsub(x = .$tag, pattern="class:", replacement = "")
  if (removeCharacter)
    out <- out[!grepl(x = out, pattern="character")]
  if (!is.null(class2))
    out <- any(out==class2)
  } else {
    stop(paste(objName, "does not exist"))
  }
  return(out)
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
#' @export
#' @examples
#' \dontrun{
#' checkLazyDir(create=TRUE) # because missing, it will provide a tmpdir
#' unlink(file.path(tempdir(), "lazyDir"), recursive=TRUE)
#' }
checkLazyDir <- function(lazyDir=NULL, create=FALSE) {
  # check that lazyDir is specified, if not, use getLazyDir, if still nothing, then use temp
  if (is.null(lazyDir)) {
    lazyDir= getLazyDir()
    if (is.null(lazyDir)) {
      lazyDir <- tryCatch(checkPath(file.path(tempdir(), "lazyDir"), create=create),
                          error=function(x) NULL)
      if (is.null(lazyDir))
        stop("Please specify a lazyDir that exists, or set it via setLazyDir(). Nothing to do.")
      message("Setting lazy directory via setLazyDir(",lazyDir,")")
      setLazyDir(lazyDir)
#      message("Lazy directory is ", lazyDir, ". It will only persist for this R session")
    }
  }

  if (!dir.exists(lazyDir)) {
    if (create) {
      dir.create(lazyDir)
    } else {
      stop(paste0("The lazyDir, ", lazyDir,", does not exist. Please specify it with lazyDir arg ",
                "or with setLazyDir()."))
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
