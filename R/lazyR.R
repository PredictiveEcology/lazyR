if (getRversion() >= "3.1.0") {
  utils::globalVariables(c(".", "artifact", "tag", "raster"))
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
#' @param overwrite Logical. Should new object replace (i.e,. overwrite) previous object with same name
#' 
#' @return invisibly returns a character vector of objects saved.
#'
#' @seealso \code{\link{lazyLs}}, \code{\link{lazyRm}}, \code{\link{lazyLoad2}}.
#' @docType methods
#' @rdname lazySave
#' @author Eliot McIntire
#' @export
#' @importFrom archivist deleteRepo createEmptyRepo saveToRepo
#' @importFrom magrittr %>%
#' @examples 
#' a <- rnorm(10)
#' b <- rnorm(20)
#' lazySave(a, b)
#' \dontrun{
#' lazySave(mget(ls()))
#' }
lazySave <- function(..., lazyDir="lazyDir",
                     tags=NULL, clearRepo=FALSE,
                     overwrite=FALSE) {
  objList <- list(...)
  file=NULL

  if(is(objList[[1]], "list")) 
    objList <- objList[[1]]
  if(!is.null(names(objList))) {
    if(length(names(objList))==length(objList)){
      file=names(objList)
    } else {
      stop("If passing a list, it must be a named list")
    }
  }
  
  if(is.null(file)) {
    file=sapply(as.list(substitute(list(...)))[-1L], deparse)
  }
  #objList <- list(obj)
  names(objList) <- file
  
  if(exists(".lazyDir", envir = .lazyREnv)) {
    lazyDir <- get(".lazyDir", envir = .lazyREnv) %>%
      gsub(pattern = "/$", x=., replacement = "")
  }
  
  dir.create(lazyDir, showWarnings = FALSE)
  
  if(!is.null(lazyDir)) {
    if(clearRepo) {
      deleteRepo(lazyDir)
    }
    if(!file.exists(file.path(lazyDir,"backpack.db"))) {
      createEmptyRepo( lazyDir )
    }
    
    lapply(1:length(objList), function(N) {
      shouldSave <- TRUE
      obj <- objList[[N]]
      file <- names(objList[N])
      firstOne <- any(lazyLs(tag=paste0("objectName:",file),
                                lazyDir=lazyDir)==file)
      if(firstOne) {
        if(!overwrite) {
          message("Object ",file," is already in the database. Use overwrite=TRUE to replace it")
          shouldSave <- FALSE
        } else {
          lazyRm(file)
        }
      }
      if(shouldSave) {
        if(is(obj, "Raster")){
          if(nchar(slot(slot(obj, "file"), "name"))==0) {
            
          }
          saveToRepo(file, repoDir = lazyDir, 
                     userTags= c(paste0("objectName:",file), tags,
                                 paste0("class:",is(obj)), 
                                 paste0("filename:",slot(slot(obj, "file"), "name"))
                                 ))
        } else {
          saveToRepo(file, repoDir = lazyDir, 
                   userTags= c(paste0("objectName:",file), tags,
                               paste0("class:",is(obj))))
        }

        md5Hash <- lazyLs(tag=paste0(file), archivistCol = "artifact")
      # Save the actual objects as lazy load databases  
        list2env(x = objList) %>%
          tools:::makeLazyLoadDB(., file.path(lazyDir,"gallery", md5Hash))
      }
    })
  }
  return(invisible(names(objList)))
}


#' List contents of lazy load database, using a tag
#' 
#' This will simply list object names that are contained within an archivist lazy load
#' database. The listing can be done with a tag.
#' 
#' @param tag Character string (not vectorized!). Search objects using this tag as a pattern.
#' 
#' @param lazyDir Character string of directory to be used for the lazy databases. 
#' This creates an archivist repository.
#' 
#' @param tagType Some tags have prefixes, such as "class:". 
#' The search can be thus within a tagType if this is specified
#' 
#' @param archivistCol The name of the column to return from a showLocalRepo call. This is usually
#' "tag" or "artifact".
#' 
#' @return A character vector of object names that match the tag requested.
#'
#' @seealso \code{\link{lazyLs}}, \code{\link{lazyRm}}, \code{\link{lazyLoad2}}.
#' @docType methods
#' @rdname lazyLs
#' @author Eliot McIntire
#' @export
#' @importFrom dplyr select select_ filter distinct left_join
#' @importFrom archivist showLocalRepo
#' @importFrom magrittr %>%
#' @examples 
#' a <- rnorm(10)
#' b <- rnorm(20)
#' lazySave(a,b,lazyDir="~/lazyDir")
#' lazyLs()
#' lazyLs(tag="function")
#' lazyLs(tag="numeric")
lazyLs <- 
  function(tag=NULL, lazyDir="lazyDir",
           tagType="objectName:",
           archivistCol="tag") {
    
    if(exists(".lazyDir", envir = .lazyREnv)) {
      lazyDir <- get(".lazyDir", envir = .lazyREnv) %>%
        gsub(pattern = "/$", x=., replacement = "")
    } 
      
    b <- showLocalRepo(repoDir=lazyDir, method="tags") %>%
      filter(grepl(pattern=tagType, tag)) %>% 
      select_("artifact",archivistCol) 
    
    if(!is.null(tag)) { 
      
      tag2 <- tag # creates confusion in dplyr because tag is a column name in 
                  # showLocalRepo and an argument in this function
      a <- showLocalRepo(repoDir=lazyDir, method="tags") %>%
        filter(grepl(paste0(tagType,tag2,"$"), tag)) %>% 
        select_("artifact")  
      
      b <- left_join(a,b,by="artifact") %>% 
           distinct
    }
    gsub(x = b[,archivistCol], pattern = tagType, replacement = "")
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
#' @return Nothing returned. This function is used for its side effects, i.e,. loading lazy objects
#'
#' @seealso \code{\link{lazyLs}}, \code{\link{lazyRm}}.
#' @docType methods
#' @rdname lazyLoad2
#' @author Eliot McIntire
#' @export
#' @importFrom archivist showLocalRepo
#' @importFrom magrittr %>%
#' @examples 
#' \dontrun{
#' obj <- rnorm(10)
#' # save the obj
#' lazySave(obj, lazyDir="~/lazyDir")
#' # remove the obj
#' rm(obj)
#' any(ls()=="obj") # Is FALSE
#' # load it back in
#' lazyLoad2("obj", lazyDir="~/lazyDir")
#' any(ls()=="obj") # Is TRUE
#' }
lazyLoad2 <- function(objNames=NULL, lazyDir="lazyDir",
                        envir=parent.frame()) {
  
  if(exists(".lazyDir", envir = .lazyREnv)) {
    lazyDir <- get(".lazyDir", envir = .lazyREnv) %>%
      gsub(pattern = "/$", x=., replacement = "")
  } 
  
  if(is.null(objNames)) {
    objNames <- unique(lazyLs(lazyDir = lazyDir))
  }
  
  lapply(objNames, function(y) {
    if(any(y == lazyLs(tag="Raster", lazyDir=lazyDir))) {
      md5Hash <- lazyLs(tag=y, archivistCol="artifact", lazyDir=lazyDir)
       
      rasterFile <- showLocalRepo(repoDir=lazyDir, method="tags") %>%
        filter(artifact==md5Hash) %>%
        filter(grepl("filename:", tag)) %>%
        select(tag) 
      browser()
      # Test if it had a filename associated with it; if not, then load the rdx directly
      if(nchar(gsub(rasterFile, pattern="filename:",replacement = ""))==0) {
        lazyLoad(file.path(lazyDir, "gallery", md5Hash), 
                 envir = envir)
      } else {
        rasterName <- gsub(rasterFile$tag, replacement = "", pattern="filename:")
        assign(y, value=raster(rasterName), envir=envir)
      }
      message(paste("Read Raster", y))
    } else {
      browser()
      md5Hash <- lazyLs(tag=y, archivistCol="artifact", lazyDir=lazyDir)
      
      lazyLoad(file.path(lazyDir, "gallery", md5Hash), 
               envir = envir)
      message(paste("Read Object", y))
    }
    })
  return(invisible()) 
}

#' Load lazy objects from a \code{lazyR} database
#' 
#' Load all named objects into the specified environment.
#' 
#' @param objNames A character vector of names of objects to be removed from a \code{lazyR} database.
#' 
#' @param lazyDir Character string of directory to be used for the lazy databases. 
#' 
#' @return Nothing returned. This function is used for its side effects, i.e,. loading lazy objects
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
#' lazySave(a, lazyDir="~/lazyDir")
#' lazyRm("a", lazyDir="~/lazyDir")
#' }
lazyRm <- function(objNames=NULL, lazyDir="lazyDir") {
  if(exists(".lazyDir", envir = .lazyREnv)) {
    lazyDir <- get(".lazyDir", envir = .lazyREnv) %>%
      gsub(pattern = "/$", x=., replacement = "")
  } 
  
  if(is.null(objNames)) {
    objNames <- unique(lazyLs(lazyDir = lazyDir))
  }
  
  lapply(objNames, function(y) {
    toRm <- lazyLs(y, archivistCol = "artifact", lazyDir=lazyDir) 
    if(length(toRm)>0) {
      rmFromRepo(toRm)
      unlink(file.path(lazyDir, "gallery", paste0(y,".rdx")))
      unlink(file.path(lazyDir, "gallery", paste0(y,".rdb")))
      message(paste("Object removed", y))
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
#' @param lazyDir A character string to the directory where the lazy database should be kept
#' 
#' @return New lazyDir
#'
#' @seealso \code{\link{lazyLs}}, \code{\link{lazyLoad2}}
#' @docType methods
#' @author Eliot McIntire
#' @rdname lazyDir
#' @export
#' @examples 
#' \dontrun{
#' setLazyDir("~/lazyDir")
#' a <- rnorm(10)
#' lazySave(a)
#' lazyRm("a")
#' }
setLazyDir <- function (lazyDir) 
{
  stopifnot(is.character(lazyDir))
  if(!file.exists(lazyDir)) {
    dir.create(lazyDir)
  }
  lazyDir <- normalizePath(lazyDir)
  assign(".lazyDir", lazyDir, envir = .lazyREnv)
  setLocalRepo(lazyDir) # Must set the archivist location too, for internal archivist functions
  return(lazyDir)
}

#' @rdname lazyDir
#' @export
getLazyDir <- function () 
{
  if(exists(".lazyDir", lazyDir, envir = .lazyREnv)) {
    get(".lazyDir", lazyDir, envir = .lazyREnv)
  } else {
    message("lazyDir is not set yet. Use setLazyDir")
  }
}


#' The lazyREnv environment
#'
#' Environment used internally to store active lazyDir
#'
#' @rdname lazyREnv
.lazyREnv <- new.env(parent=emptyenv())
