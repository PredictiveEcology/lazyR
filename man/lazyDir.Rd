% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lazyDir.R
\docType{methods}
\name{.setLazyDir}
\alias{.getLazyDir}
\alias{.setLazyDir}
\alias{getLazyDir}
\alias{lazyDir}
\alias{setLazyDir}
\title{Set and get the lazyR database directory}
\usage{
.setLazyDir(lazyDir, create = FALSE)

.getLazyDir()

lazyDir(lazyDir = NA, create = FALSE)

setLazyDir(lazyDir, create = TRUE)

getLazyDir(lazyDir, create = TRUE)
}
\arguments{
\item{lazyDir}{Character string of directory to be used for the lazy databases.
If \code{lazyDir=NULL}, then it removes the active lazy directory. If NA, the default,
returns the currently set lazyDir.}

\item{create}{Logical, passed to checkLazyDir, i.e., it will create dir if it doesn't exist.
Default is FALSE.}
}
\value{
New lazyDir created (for \code{lazyDir}) or currently active lazyDir returned.
}
\description{
This can be set and gotten with these functions, or all functions can take
an argument, lazyDir, manually. \code{lazyDir()} is a shorthand for \code{lazyDir()}.
\code{lazyDir("someDir")} is a shorthand for \code{lazyDir("someDir")}
}
\examples{
lazyDir() # Returns NULL if not set

# Set new lazyDir
lazyDir(file.path(tempdir(), "lazyDir"), create=TRUE)

# Check new value
lazyDir()

a <- rnorm(10)
lazySave(a) # add to lazyDir
lazyRm("a") # remove object from lazyDir

lazyDir(NULL) # removes currently set lazyDir
lazyDir()  # confirm removal
unlink(file.path(tempdir(), "lazyDir"), recursive=TRUE) # remove folder and lazyDir
}
\author{
Eliot McIntire
}
\seealso{
\code{\link{lazyLs}}, \code{\link{lazyLoad2}}
}

