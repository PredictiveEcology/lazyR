if (getRversion() >= "3.1.0") {
  utils::globalVariables(c(".", "artifact", "lazyDir", "raster", "tag"))
}

################################################################################
#' Save objects to a \code{lazyR} database
#'
#' This function creates an archivist repository that holds metadata ("tags")
#' associated with the object. Unlike the \code{archivist} package, the objects
#' themselves are saved as lazy load databases (i.e., via \code{tools:::makeLazyLoadDB)}.
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
#' @param objNames Optional vector of names of same length as ... to override the object names.
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
#' @importFrom raster crs inMemory filename
#' @importFrom archivist deleteRepo createEmptyRepo saveToRepo addTagsRepo
#' @importFrom magrittr %>%
#' @importFrom digest digest
#' @importFrom utils getFromNamespace
#' @examples
#' a <- rnorm(10)
#' b <- rnorm(20)
#' lazyDir(file.path(tempdir(),"lazyDir"), create=TRUE)
#' lazySave(a, b)
#' \dontrun{ # may be many objects
#' lazySave(mget(ls(envir=.GlobalEnv)))
#' unlink(file.path(tempdir(), "lazyDir"), recursive=TRUE)
#' }
lazySave <- function(..., objNames=NULL, lazyDir=NULL, tags=NULL, clearRepo=FALSE,
                     overwrite=FALSE, copyRasterFile=TRUE,
                     compareRasterFileLength=1e6) {

  objList <- list(...)

  if (is(objList[[1]], "list")) {
    objList <- objList[[1]]
  }

  if(!is.null(objNames)) {
    if(length(objNames) != length(objList)) {
      stop("objNames must be same length as objList")
    }
    names(objList) <- objNames

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
  names(objList) <- objNames

  lazyDir <- checkLazyDir(lazyDir, create=TRUE)

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
          lazyRm(objName, lazyDir=lazyDir)
        }
      }
      if (shouldSave) {
        md5Hash <- digest(obj)
        if(any(showLocalRepo(method = "md5hashes", repoDir = lazyDir)$md5hash==md5Hash)) {
#          rmFromRepo(md5Hash)
          addTagsRepo(md5Hash, repoDir = lazyDir, tags = paste0("objectName:", objName))
          #message("Resaving ",md5Hash," with new name, ",objName," in repository")
        } else {
          if (is(obj, "Raster")) {
              saveToRepoRaster(obj, objName=objName, lazyDir=lazyDir,
                                         tags=tags, compareRasterFileLength=compareRasterFileLength)
          } else {
            saveToRepo(artifact=obj, repoDir = lazyDir,
                       userTags = c(paste0("objectName:", objName), tags,
                                    paste0("class:", is(obj))))
          }
#        md5Hash <- lazyLs(tag=objName, archivistCol = "artifact",
#                          lazyDir = lazyDir, exact = TRUE) %>% unique

          # Add tags by class
          if (is(obj, "spatialObjects")) {
            addTagsRepo(md5Hash, tags=paste0("crs:", crs(obj)), repoDir = lazyDir)
          }
        }

      # Save the actual objects as lazy load databases
#         list2env(x = objList[N]) %>%
#           getFromNamespace("makeLazyLoadDB", "tools")(., file.path(lazyDir, "gallery", md5Hash))
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
#' @examples
#' \dontrun{
#' tmpdir <- lazyDir(file.path(tempdir(), "lazyDir"), create=TRUE)
#' a <- rnorm(10)
#' b <- rnorm(20)
#' lazySave(a, b, lazyDir = tmpdir)
#' lazyLs(lazyDir=tmpdir)
#'
#' # can set lazyDir and don't need lazyDir argument
#' lazyDir(tmpdir)
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

  # check that lazyDir is specified, if not, use lazyDir, if still nothing, then use temp
  lazyDir <- checkLazyDir(lazyDir)
  firstRepoLs <- showLocalRepo(repoDir=lazyDir, method="tags") %>%
    filter(grepl(pattern=tagType, tag)) %>%
    distinct_("artifact", "tag") #%>%
    #select_("artifact", archivistCol)

  if (!is.null(tag)) {
    tag2 <- tag # creates confusion in dplyr because tag is a column name in
    # showLocalRepo and an argument in this function
    if (exact) {
      secondRepoLs <- showLocalRepo(repoDir=lazyDir, method="tags") %>%
        filter(grepl(paste0("^",tagType,tag2,"$"), tag)) %>%
        select_("artifact")
    } else {
      secondRepoLs <- showLocalRepo(repoDir=lazyDir, method="tags") %>%
        filter(grepl(paste0(tag2), tag)) %>%
        select_("artifact")
    }

    firstRepoLs <- left_join(secondRepoLs, firstRepoLs, by="artifact") %>%
      distinct_

  }

  if (tagTypeAll) {
    out <- firstRepoLs
  } else {
    out <- gsub(x = firstRepoLs[, archivistCol], pattern = tagType, replacement = "") %>%
      sort
  }
  return(out)
}


#' Remove lazy objects from a \code{lazyR} database
#'
#' Remove all named objects from the specified lazyDir. 
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

  if (is.null(objNames)) {
    objNames <- unique(lazyLs(lazyDir = lazyDir))
  }

  lapply(objNames, function(y) {
    z <- lazyLs(y, archivistCol = "artifact", lazyDir=lazyDir, exact=exact)
    if (length(z)>0) {
      for (toRm in z) {
        objName <- lazyObjectName(toRm, lazyDir=lazyDir)
        for(oN in objName) {
          if (lazyIs(oN, "Raster", lazyDir = lazyDir) & removeRasterFile) {
            tmpEnv <- new.env()
            lazyLoad2(oN, envir = tmpEnv)
            file.remove(filename(tmpEnv[[objName]]))
          }

          rmFromRepo(toRm, repoDir = lazyDir)
          message(paste("Object removed:", objName))
        }
      }
    } else {
      message(y, " not in lazy load db. Nothing removed.")
    }

  })
  return(invisible())
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
#' lazyDir(file.path(tempdir(), "lazyDir"), create=TRUE)
#' lazySave(a) # default uses an tempdir() call for location
#' hash <- lazyLs("a", archivistCol = "artifact", exact=TRUE)
#' (objName <- lazyObjectName(hash))
#' unlink(file.path(tempdir(), "lazyDir"), recursive=TRUE)
#' }
lazyObjectName <- function(md5Hash, lazyDir=NULL) {
  lazyDir <- checkLazyDir(lazyDir)
  lazyObjectName <- showLocalRepo(method="tags", repoDir = lazyDir) %>%
    filter(artifact==md5Hash) %>%
    filter(grepl(pattern="objectName:", tag)) %>%
    select_("tag")
  lazyObjectName <- gsub(lazyObjectName$tag, pattern="objectName:", replacement="")
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
#' lazyDir(file.path(tempdir(), "lazyDir"), create=TRUE)
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
#     lazyDir= lazyDir()
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




#' Does an object exist in the lazyDir
#'
#' @param objNames A character vector indicating the object names to test
#'
#' @param lazyDir the lazyDir to use
#'
#' @param exact Should the character string matching be exact, i.e., ^objNames$
#' in regexp. Passed to \code{lazyLs()}
#'
#' @return Logical
#'
#' @importFrom dplyr filter select_ as.tbl
#'
#' @seealso \code{\link{lazyLs}}, \code{\link{lazyLoad2}}
#' @docType methods
#' @author Eliot McIntire
#' @rdname lazyExists
#' @export
#' @examples
#' lazyDir(file.path(tempdir(), "lazyDir"), create=TRUE)
#' a <- rnorm(10)
#' lazySave(a)
#' lazyExists("a")
#' lazyRm("a")
#' lazyExists("b") # error, does not exist
#' unlink(file.path(tempdir(), "lazyDir"), recursive=TRUE)
lazyExists <- function(objNames, lazyDir=NULL, exact=TRUE) {
  
  lazyDir <- checkLazyDir(lazyDir)
  
  out <- sapply(objNames, lazyLs, exact=exact)
  if(length(out) > 0) {
    return(objNames %in% out)
  } else {
    return(FALSE)
  }
}
