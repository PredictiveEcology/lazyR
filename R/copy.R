#' Copy a directory using Robocopy on Windows and rsync on linux
#'
#' This will copy an entire directory using the faster copy options in Windows and Linux.
#' The function will default to \code{file.copy} (which is slow). For lazyR databases,
#' this can be useful
#' for copying everything to a new computer, a network location etc.
#'
#' @param fromDir The source lazyDir
#'
#' @param toDir The new lazyDir
#'
#' @param useRobocopy For Windows, this will use a system call to Robocopy which appears to be much
#' faster than the internal \code{file.copy} function. Uses /MIR flag.
#'
#' @param overwrite Passed to \code{file.copy}
#'
#' @param delDestination Logical, whether the destination should have any files deleted, if they don't exist
#' in the source. This is /purge
#'
#' @param create Passed to \code{checkLazyDir}
#'
#' @param silent Should a progress be printed
#'
#' @docType methods
#' @author Eliot McIntire
#' @rdname copyDir
#' @export
#' @examples
#' \dontrun{
#' library(raster)
#' library(rgdal)
#' # make some objects
#' obj1 <- 1:10
#' obj2 <- 11:20
#' r <- raster::raster(matrix(1:9, ncol=3))
#' raster::writeRaster(r, file.path(tempdir(),"r.tif"), overwrite=TRUE)
#' raster::writeRaster(r, file.path(tempdir(),"ras.tif"), overwrite=TRUE)
#' rm(r, ras)
#' r <- raster::raster(file.path(tempdir(),"r.tif"))
#' ras <- raster::raster(file.path(tempdir(),"ras.tif"))
#'
#' # identify a new and old lazyLoad db
#' fromDir <- file.path(tempdir(), "old")
#' toDir <- file.path(tempdir(), "new")
#' lazySave(obj1, r, ras, obj2, lazyDir=fromDir, overwrite=TRUE)
#'
#' # copy to new lazyDir location
#' copyDir(fromDir, toDir, create=TRUE)
#'
#' # remove the objects in memory and the old lazyLoad db
#' rm(obj1, obj2, r, ras)
#' unlink(fromDir, recursive=TRUE)
#'
#' lazyLoad2(lazyDir=toDir) # Objects should be visible after this
#' ls()
#' unlink(toDir, recursive=TRUE)
#' }
copyDir <- function(fromDir=NULL, toDir=NULL, useRobocopy=TRUE,
                    overwrite=TRUE, delDestination=FALSE,
                    #copyRasterFile=TRUE, clearRepo=TRUE,
                    create=TRUE, silent=FALSE) {
  
  origDir <- getwd()
  #fromDir <- checkLazyDir(fromDir)
  #toDir <- checkLazyDir(toDir, create=create)
  setwd(fromDir)
  
  os <- tolower(Sys.info()[["sysname"]])
  if(os=="windows") {
    if(useRobocopy) {
      if(silent){
        system(paste0("robocopy /E ","/purge"[delDestination]," /ETA /NDL /NFL /NJH /NJS ", normalizePath(fromDir, winslash = "\\"),
                      "\\ ", normalizePath(toDir, winslash = "\\")))
      } else {
        system(paste0("robocopy /E ","/purge"[delDestination]," /ETA ", normalizePath(fromDir, winslash = "\\"),
                      "\\ ", normalizePath(toDir, winslash = "\\")))
        #         system(paste0("robocopy /E ","/purge"[delDestination]," /ETA ", normalizePath(fromDir, winslash = "\\"),
        #                       "\\ ", normalizePath(toDir, winslash = "\\"), "\\"))
      }
    } else {
      file.copy(from = dir(fromDir), to = toDir,
                overwrite = overwrite, recursive=TRUE)
    }
  } else if(os=="linux" | os == "darwin") {
    if(silent){
      system(paste0("rsync -aP ","--delete "[delDestination], fromDir, "/ ", toDir))
    } else {
      system(paste0("rsync -avP ","--delete "[delDestination], fromDir, "/ ", toDir))
    }
  }
  setwd(origDir)
  return(invisible(toDir))
}

#' Copy a file using Robocopy on Windows and rsync on linux
#'
#' This will copy an individual file faster Robocopy in Windows and rsync in Linux.
#' The function will default to \code{file.copy} (which is slow).
#'
#' @param from The source file
#'
#' @param to The new file
#'
#' @param useRobocopy For Windows, this will use a system call to Robocopy which appears to be much
#' faster than the internal \code{file.copy} function. Uses /MIR flag.
#'
#' @param overwrite Passed to \code{file.copy}
#'
#' @param delDestination Logical, whether the destination should have any files deleted, if they don't exist
#' in the source. This is /purge
#'
#' @param create Passed to \code{checkLazyDir}
#'
#' @param silent Should a progress be printed
#'
#' @docType methods
#' @author Eliot McIntire
#' @rdname copyFile
#' @export
copyFile <- function(from=NULL, to=NULL, useRobocopy=TRUE,
                     overwrite=TRUE, delDestination=FALSE,
                     #copyRasterFile=TRUE, clearRepo=TRUE,
                     create=TRUE, silent=FALSE) {
  
  origDir <- getwd()
  #fromDir <- checkLazyDir(fromDir)
  #toDir <- checkLazyDir(toDir, create=create)
  if(!dir.exists(to)) to <- dirname(to) # extract just the directory part
  os <- tolower(Sys.info()[["sysname"]])
  if(os=="windows") {
    if(useRobocopy) {
      if(silent){
        system(paste0("robocopy ","/purge"[delDestination]," /ETA /NDL /NFL /NJH /NJS ",
                      normalizePath(dirname(from), winslash = "\\"),
                      "\\ ", normalizePath(to, winslash = "\\"),
                      " ", basename(from)))
      } else {
        system(paste0("robocopy ","/purge"[delDestination]," /ETA ", normalizePath(dirname(from), winslash = "\\"),
                      "\\ ", normalizePath(to, winslash = "\\"),
                      " ", basename(from)))
        #         system(paste0("robocopy /E ","/purge"[delDestination]," /ETA ", normalizePath(fromDir, winslash = "\\"),
        #                       "\\ ", normalizePath(toDir, winslash = "\\"), "\\"))
      }
    } else {
      file.copy(from = from, to = to, overwrite=overwrite, recursive = FALSE)
    }
  } else if(os=="linux" | os == "darwin") {
    if(silent){
      system(paste0("rsync -aP ","--delete "[delDestination], from, " ", to,"/"))
    } else {
      system(paste0("rsync -avP ","--delete "[delDestination], from, " ", to, "/"))
    }
  }
  setwd(origDir)
  return(invisible(to))
}




#' Copy individual objects from one lazy database to another
#'
#' This will copy individual objects from one lazy database to another. This is much slower
#' than copying a whole lazy database using \code{copyDir}, but it will merge objects from
#' one database to another.
#' 
#' @param objNames a character vector of object names to copy from one \code{lazyDir} to another
#'
#' @param from The source \code{lazyDir}
#'
#' @param to The new \code{lazyDir}
#'
#' @param create Passed to \code{checkLazyDir}
#'
#' @param verbose Logical. If notes about rasters whose file backings were corrected on the
#' fly from previous, i.e,. wrong, filenames.
#'
#' @param ... Other arguments passed to lazySave, such as overwrite
#'
#' @docType methods
#' @author Eliot McIntire
#' @rdname copyLazyObjs
#' @export
copyLazyObjs <- function(objNames, from=NULL, to=NULL,
                         create=TRUE, verbose=FALSE, ...) {
  
  obsRead <- character(0)
  obsAlreadyIn <- character(0)
  obsNotInLazyDir <- character(0)
  on.exit(expr = {
    message(length(obsRead), " objects copied of ", length(objNames))
    if(verbose) {
      if (length(obsRead)!=length(objNames)) 
        message("Failed on ", objNames[!(objNames %in% obsRead)][1])
      if (length(obsAlreadyIn)) 
        message(paste(obsAlreadyIn, collapse = ", "), " already in ", to)
      if (length(obsNotInLazyDir)) 
        message(paste(obsNotInLazyDir, collapse = ", "), " not in ", from)
    }
    
  })
  
  E <- new.env()
  loadMess <- capture.output(loaded <- lazyLoad2(objNames, lazyDir=from, envir=E),
                             type="message")
  if(any(grepl(loadMess, pattern="  because not in lazyDir"))) {
    obsNotInLazyDir <- c(obsNotInLazyDir, objNames[!(objNames %in% loaded)])
  } 
  checkLazyDir(lazyDir = to, create=create)
  for(obj in 1:length(objNames[(objNames %in% loaded)])) {
    out <- capture.output(lazySave(get(objNames[obj], envir=E), objNames=objNames[obj], lazyDir=to, ...) ,
                          type="message")
    if(length(grep(out, pattern="is already")>0)) {
      obsAlreadyIn <- c(obsAlreadyIn, objNames[obj])
    } else {
      obsRead <- c(obsRead,  objNames[obj])
      rm(list=objNames[obj], envir=E) # rm objects to keep RAM use low
    }
    rm(out)
  }
  return(obsRead)
}
