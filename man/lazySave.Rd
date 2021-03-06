% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lazyR.R
\docType{methods}
\name{lazySave}
\alias{lazySave}
\title{Save objects to a \code{lazyR} database}
\usage{
lazySave(..., objNames = NULL, lazyDir = NULL, tags = NULL,
  clearRepo = FALSE, overwrite = FALSE, copyRasterFile = TRUE,
  compareRasterFileLength = 1e+06)
}
\arguments{
\item{...}{Objects to save to an R lazy database, with metadata saved via archivist package repository.}

\item{objNames}{Optional vector of names of same length as ... to override the object names.}

\item{lazyDir}{Character string of directory to be used for the lazy databases.
This creates an archivist repository.}

\item{tags}{Character vector of additional tags to add to the objects being saved}

\item{clearRepo}{Logical. Should the repository be deleted before adding new objects}

\item{overwrite}{Logical. Should new object replace (i.e., overwrite) previous object with same name}

\item{copyRasterFile}{Logical. Should the source file for rasters be copied into the lazyDir.
Default TRUE, which may be slow. See Details.}

\item{compareRasterFileLength}{Numeric. This is passed to the length arg in \code{digest}
when determining if the Raster file is already in the database. Default 1e6. See Details.}
}
\value{
invisibly returns a character vector of objects saved.
}
\description{
This function creates an archivist repository that holds metadata ("tags")
associated with the object. Unlike the \code{archivist} package, the objects
themselves are saved as lazy load databases (i.e., via \code{tools:::makeLazyLoadDB)}.
Each object is saved into its own lazy load database, with a filename that
matches the object name. Because it is also an archivist repository, lazy
loading of objects can be done via tags, classes etc..
}
\details{
The main use case for this is large, complicated datasets, such as GIS databases, that
are used within R. Loading them lazily means that the user can have access to all of them,
including their characteristics, without loading them all first (could be very slow).
When \code{copyRasterFile} is \code{TRUE}, it may take a while as the original source
file is copied. Also, the slot in the raster file that indicates the file pointer
is switched to new location. This allows more modular lazyR database, i.e., if the Raster
files are copied into the lazyR database, then the whole database contains everything needed
to copy to another machine. Note: If \code{copyRasterFile} is TRUE, but the file is already
there and is identical (using \code{\link[digest]{digest}}), then no copy is done.

When \code{copyRasterFile} is \code{TRUE}, it will actually run a \code{digest}
command on the file, if it already exists in the rasters directory of the lazyR database.
If this hash resulting from the \code{digest} indicates that the file is already there and
it is identical, then it will not actually be copied. This will be good for speed, but
will miss small changes in files. Set \code{compareRasterFileLength} to \code{Inf} if
truly exact comparisons behaviour is needed.
}
\examples{
a <- rnorm(10)
b <- rnorm(20)
lazyDir(file.path(tempdir(),"lazyDir"), create=TRUE)
lazySave(a, b)
\dontrun{ # may be many objects
lazySave(mget(ls(envir=.GlobalEnv)))
unlink(file.path(tempdir(), "lazyDir"), recursive=TRUE)
}
}
\author{
Eliot McIntire
}
\seealso{
\code{\link{lazyLs}}, \code{\link{lazyRm}}, \code{\link{lazyLoad2}}.
}

