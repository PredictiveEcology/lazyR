if (getRversion() >= "3.1.0") {
  utils::globalVariables(c(".", "artifact", "lazyDir", "raster", "tag"))
}

################################################################################
#' Save objects to a \code{lazyR} database
#'
#' This function creates an archivist repository that holds metadata ("tags") associated
#' with the object. Unlike the archivist package, the objects themselves are saved as
#' lazy load databases (i.e., via tools:::makeLazyLoadDB). Each object is saved into its own
#' lazy load database, with a filename that matches the object name. Because it is also an
#' archivist repository, lazy loading of objects can be done via tags, classes etc..
#'
#' The main use case for this is large, complicated datasets, such as GIS databases, that
#' are used within R. Loading them lazily means that the user can have access to all of them,
#' including their characteristics, without loading them all first (could be very slow).
#'
#' @param ... Objects to save to an R lazy database, with metadata saved via archivist package repository.
#'
#' @param lazyDir Character string of directory to be used for the lazy databases. This creates an archivist repository.
#'
#' @param tags Character vector of additional tags to add to the objects being saved
#'
#' @param clearRepo Logical. Should the repository be deleted before adding new objects
#'
#' @param overwrite Logical. Should new object replace (i.e., overwrite) previous object with same name
#'
#' @return invisibly returns a character vector of objects saved.
#'
#' @seealso \code{\link{lazyLs}}, \code{\link{lazyRm}}, \code{\link{lazyLoad2}}.
#' @docType methods
#' @rdname lazySave
#' @author Eliot McIntire
#' @export
#' @importClassesFrom SpaDES spatialObjects
#' @importFrom raster crs
#' @importFrom archivist deleteRepo createEmptyRepo saveToRepo
#' @importFrom magrittr %>%
#' @importFrom SpaDES checkPath 
#' @examples
#' a <- rnorm(10)
#' b <- rnorm(20)
#' lazySave(a, b)
#' \dontrun{ # may be many objects
#' lazySave(mget(ls(envir=.GlobalEnv)))
#' }
lazySave <- function(..., lazyDir=NULL, tags=NULL, clearRepo=FALSE,
                     overwrite=FALSE) {
  objList <- list(...)
  file <- NULL

  if (is(objList[[1]], "list")) {
    objList <- objList[[1]]
  }
  if (!is.null(names(objList))) {
    if (length(names(objList)) == length(objList)) {
      file <- names(objList)
    } else {
      stop("If passing a list, it must be a named list.")
    }
  }

  if (is.null(file)) {
    file <- sapply(as.list(substitute(list(...)))[-1L], deparse)
  }
  #objList <- list(obj)
  names(objList) <- file

  if (!is.null(getLazyDir())) {
    lazyDir <- getLazyDir() 
  }

  if(is.null(lazyDir)) {
    lazyDir <- checkPath(file.path(tempdir(), "lazyDir"), create=TRUE)
    message("Lazy directory is ", lazyDir, ". It will only persist for this R session")
  }
  
  checkPath(lazyDir, create = TRUE)
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
      file <- names(objList[N])
      firstOne <- any(
        lazyLs(tag = file, lazyDir = lazyDir, exact=TRUE) == file
      )
      if (firstOne) {
        if (!overwrite) {
          message("Object ", file, " is already in the database. ",
                  "Use overwrite=TRUE to replace it")
          shouldSave <- FALSE
        } else {
          lazyRm(file)
        }
      }
      if (shouldSave) {
        if (is(obj, "Raster")){
#          if (nchar(slot(slot(obj, "file"), "name"))==0) { # in memory
#          }
          saveToRepo(file, repoDir = lazyDir,
                     userTags = c(paste0("objectName:", file), tags,
                                 paste0("class:", is(obj)),
                                 paste0("filename:", slot(slot(obj, "file"), "name"))
                                ))
        } else {
          saveToRepo(file, repoDir = lazyDir,
                   userTags = c(paste0("objectName:", file), tags,
                                paste0("class:", is(obj))))
        }

        md5Hash <- lazyLs(tag=file, archivistCol = "artifact", lazyDir = lazyDir, exact = TRUE)
        
        # Add tags by class
        if(is(obj, "spatialObjects")) addTagsRepo(md5Hash, tags=paste0("crs:",crs(obj)),
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
#' @seealso \code{\link{lazyLs}}, \code{\link{lazyRm}}, \code{\link{lazyLoad2}}.
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

  if(tagType=="all") {
    tagTypeAll <- TRUE
    tagType <- ""
  } else {
    tagTypeAll <- FALSE
  }

  if (!is.null(getLazyDir())) {
    lazyDir <- get(".lazyDir", envir = .lazyREnv) %>%
      gsub(pattern = "/$", x=., replacement = "")
  } 

  if(is.null(lazyDir)) {
    lazyDir <- checkPath(file.path(tempdir(), "lazyDir"))
  }
  
  if (!dir.exists(lazyDir)) {
    stop(paste0("The lazyDir, ", lazyDir,", does not exist. Please specify it with lazyDir arg ",
                "or with setLazyDir()."))
  }

    b <- showLocalRepo(repoDir=lazyDir, method="tags") %>%
      filter(grepl(pattern=tagType, tag)) %>%
      distinct_("artifact", "tag") #%>%
      #select_("artifact", archivistCol) 

    if (!is.null(tag)) {

      tag2 <- tag # creates confusion in dplyr because tag is a column name in
                  # showLocalRepo and an argument in this function
      if(exact) {
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
    if(tagTypeAll) {
      out <- b
    } else {
      out <- gsub(x = b[, archivistCol], pattern = tagType, replacement = "")
    }
    return(sort(out))
}

#' Load lazy objects from a \code{lazyR} database
#'
#' Load all named objects into the specified environment.
#'
#' @param objNames A character vector of object names to load lazily, usually from a lazyLs call.
#'
#' @param lazyDir Character string of directory to be used for the lazy databases.
#'
#' @param envir Which environment should the objects be loaded into. Default is \code{.GlobalEnv}
#'
#' @return Invisibly, the objects read in returned. This function is used for its side effects, i.e., loading lazy objects
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
#'
lazyLoad2 <- function(objNames=NULL, lazyDir=NULL, envir=parent.frame()) {

  obsRead <- character(0)
  on.exit(expr= {
    message(length(obsRead), " objects loaded of ", length(objNames))
    if(length(obsRead)!=length(objNames)) {
      message("Failed on ", objNames[!(objNames %in% obsRead)][1])
    }})
  if (exists(".lazyDir", envir = .lazyREnv)) {
    lazyDir <- get(".lazyDir", envir = .lazyREnv) %>%
      gsub(pattern = "/$", x=., replacement = "")
  }

  if(is.null(lazyDir)) {
    lazyDir <- checkPath(file.path(tempdir(), "lazyDir"))
  }
  
  
  if (is.null(objNames)) {
    objNames <- unique(lazyLs(lazyDir = lazyDir))
  }

  lapply(objNames, function(y) {
    if (any(y == lazyLs(tag="class:Raster", lazyDir=lazyDir))) {
      md5Hash <- lazyLs(tag=y, archivistCol="artifact", lazyDir=lazyDir, 
                        exact=TRUE)

      rasterFile <- showLocalRepo(repoDir=lazyDir, method="tags") %>%
        filter(artifact==md5Hash) %>%
        filter(grepl("filename:", tag)) %>%
        select(tag) 
      
      # Test if it had a filename associated with it; if not, then load the rdx directly
      if(nchar(gsub(rasterFile, pattern="filename:",replacement = ""))==0) {
        lazyLoad(file.path(lazyDir, "gallery", md5Hash), 
                 envir = envir)
      } else {
        rasterName <- gsub(rasterFile$tag, replacement = "", pattern="filename:")
        assign(y, value=raster(rasterName), envir=envir)
      }
      obsRead <<- c(obsRead,  y)
    } else {
      md5Hash <- lazyLs(tag=y, archivistCol="artifact", 
                        lazyDir=lazyDir, exact=TRUE)
      
      lazyLoad(file.path(lazyDir, "gallery", md5Hash), 
               envir = envir)
      
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
#' @return Nothing returned. This function is used for its side effects, i.e., loading lazy objects
#'
#' @seealso \code{\link{lazyLs}}, \code{\link{lazyLoad2}}
#' @docType methods
#' @rdname lazyRm
#' @author Eliot McIntire
#' @export
#' @importFrom archivist rmFromRepo
#' @examples
#' \dontrun{
#' a <- rnorm(10)
#' lazySave(a, lazyDir=tempdir())
#' lazyRm("a", lazyDir=tempdir())
#' }
lazyRm <- function(objNames=NULL, lazyDir="lazyDir", exact=TRUE) {

  if (exists(".lazyDir", envir = .lazyREnv)) {
    lazyDir <- get(".lazyDir", envir = .lazyREnv) %>%
      gsub(pattern = "/$", x=., replacement = "")
  }

  if (is.null(objNames)) {
    objNames <- unique(lazyLs(lazyDir = lazyDir))
  }

  lapply(objNames, function(y) {
    z <- lazyLs(y, archivistCol = "artifact", lazyDir=lazyDir, exact=exact)
    if(length(z)>0) {
      for (toRm in z) {
        objName <- lazyObjectName(toRm, lazyDir=lazyDir)
        rmFromRepo(toRm, repoDir = lazyDir)
        unlink(file.path(lazyDir, "gallery", paste0(toRm, ".rdx")))
        unlink(file.path(lazyDir, "gallery", paste0(toRm, ".rdb")))
        message(paste("Object removed", objName))
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
#' @param lazyDir A character string to the directory where the lazy database should be kept. 
#' If \code{lazyDir=NULL}, then it removes the active lazy directory.
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
#' setLazyDir(file.path(tempdir(), "lazyDir"))
#' a <- rnorm(10)
#' lazySave(a)
#' lazyRm("a")
#' unlink(file.path(tempdir(), "lazyDir"))
#' }
setLazyDir <- function(lazyDir) {
  if(is.null(lazyDir)) {
    if(exists(".lazyDir", envir = .lazyREnv)){
      rm(".lazyDir", envir = .lazyREnv)
      return("removed lazyDir")
    } else {
      return("LazyDir was not set. Nothing to remove")
    }
  }
  stopifnot(is.character(lazyDir))
  if (!file.exists(lazyDir)) {
    dir.create(lazyDir)
  }
  lazyDir <- normalizePath(lazyDir)
  assign(".lazyDir", lazyDir, envir = .lazyREnv)
  setLocalRepo(lazyDir) # Must set the archivist location too, for internal archivist functions
  return(paste("Local lazyR database set to",lazyDir))
}

#' @rdname lazyDir
#' @export
getLazyDir <- function() {
  if (exists(".lazyDir", envir = .lazyREnv)) {
    get(".lazyDir", envir = .lazyREnv) %>%
      gsub(pattern = "/$", x=., replacement = "") %>%
      gsub(pattern = "\\$", x=., replacement = "") 
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
#' }
lazyObjectName <- function(md5Hash, lazyDir) {
  lazyObjectName <- showLocalRepo(method="tags", repoDir = lazyDir) %>% 
    filter(artifact==md5Hash) %>% 
    filter(grepl(pattern="objectName:", tag)) %>%
    select_("tag") %>%
    gsub(.$tag, pattern="objectName:", replacement="")
  return(lazyObjectName)
    
}
