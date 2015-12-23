################################################################################
#' Lazy cache assignment operator
#'
#' This is very experimental. Alternative assignment operator to \code{<-}
#'
#' This does three things in this order:
#'
#' 1. Take a \code{digest::digest} of the right hand side function arguments
#' 2. Similar to \code{cache} in archivist package, it compares this digest to the lazyDir
#' database. If it that exact digest exists already, then it will \code{lazyLoad2}
#' it from the lazyDir. There are 2 differences from the \code{cache} function. First, the
#' object name that is the "assigment" recipient is included in the caching and it is saved
#' to a lazy load database on disk.
#' 3. Assigns in memory the result of the call (\code{y}) to \code{x} as in the normal <- operator.
#'
#' Known features: caching is based on both the left side (the object name assigned to)
#' and the the right hand side operator (the call itself). This may or may not be the best behaviour
#' and will be revisited with usage.
#'
#' Known limitations: currently, the right hand side must be a simple function call, and can't include
#' a magrittr pipe operator.
#'
#' If writing and reading from disk the outputs and inputs, respectively, takes longer than
#' evaluating the expression, then there is no point in caching.
#'
#' Likewise, if the function returns a result that is stochastic (i.e., will be different each time
#' the function is called, even with the same input arguments), caching may not give the desired
#' behaviour. It will always return the same result from a \code{output} %<% \code{rnorm(1)}
#'
#' @param x the left hand side objectName to assign to, analogous to \code{<-}.
#'
#' @param y the right hand side. Anything that R understands,
#'
#' @param lazyDir the lazyDir to use
#'
#' @param notOlderThan see \code{\link[archivist]{cache}}
#'
#' @param envir The environment to be passed to substitute internally.
#' This may help with programming control.
#'
#' @note Because this works as an assignment operator, any arguments other than the x and y
#' are not changeable unless it is used as a function call using back ticks
#' \code{assignCache(x, y, notOlderThan = Sys.time())}
#'
#' @return Evaluation or lazy load of the right hand side (y),
#' assigned to the objectName (x) on the left hand side.
#'
#' @export
#' @docType methods
#' @rdname cacheAssign
#' @aliases assignCache
#' @importFrom archivist cache
#' @importFrom magrittr %>%
#' @importFrom lazyeval lazy_eval
#'
#' @author Eliot McIntire
#' @examples
#' lazyDir(tempdir(), create=TRUE)
#' # First time will evaluate it, create a hash, save to lazy database, return result. Will be slow
#' system.time(a%<%seq(1,1e6))
#'
#' # Second time will return the value in the lazy load database because arguments are identical
#' system.time(a%<%seq(1,1e6))
#'
#' # Third time - but force re-evaluation
#' system.time(assignCache("a", seq(1,1e6), notOlderThan = Sys.time()))
#'
#' # For comparison, normal assignment may be faster if it is a fast function in R
#' system.time(a<-seq(1,1e6))
#'
#' lazyRm("a")
assignCache <- function(x, y, lazyDir=NULL, notOlderThan=NULL, envir=sys.frame(0)) {
  
  if(!is.character(x)) stop("x must be a character")
  
  y <- lazy(y)
  x <- force(x)
  lazyDir <- checkLazyDir(lazyDir = lazyDir)
  if(is.null(y$expr)) {
    y <- lazy(y)
    if(is.null(y$env)) y$env <- envir
  }
  
  if(length(y$expr)==1) { # This is for an object passing
    inputs <- y$expr
  } else {
    inputs <- try(match.call(eval(y$expr[[1]]), call = y$expr)[-1],
                  silent = TRUE)
    if(is(inputs, "try-error"))
      inputs <- match.call(call = y$expr)[-1]
  }
  
  evaluated<-lapply(inputs, function(h1) {
    tryCatch(eval(h1, envir=envir), error=function(h2) {
      lapply(h1, function(h3) {
        tryCatch(eval(h3, envir=envir), error=function(h3) deparse(h3))
      })
    })
  })
  
  digestCall <- digest(evaluated)
  #h1)}),x)) # includes object name, x
  
  localTags <- showLocalRepo(lazyDir, "tags")
  
  isInRepo <- localTags[localTags$tag == paste0("cacheId:", digestCall), , drop = FALSE]
  
  if (nrow(isInRepo) > 0) {
    lastEntry <- max(isInRepo$createdDate)
    if (is.null(notOlderThan) || (notOlderThan < lastEntry)) {
      lastOne <- order(isInRepo$createdDate, decreasing = TRUE)[1]
      objNameInRepo <- lazyLs(digestCall, lazyDir=lazyDir)
      if(length(objNameInRepo)>1) objNameInRepo <- objNameInRepo[objNameInRepo %in% x]
      if(exists(x, envir=envir)) rm(list = x, envir=envir)
      lazyLoad2(objNameInRepo, lazyDir=lazyDir, envir = envir)
      #        delayedAssign(x = x, value = get(lazyLs(digestCall), envir=environment()),
      #                      eval.env = environment(), assign.env = envir)
      return(invisible())
    }
  }
  
  output <- lazy_eval(y)
  lazySave(output, lazyDir=lazyDir, objNames = x, tags=paste0("cacheId:", digestCall),
           overwrite=TRUE)
  delayedAssign(x = x, value = output, eval.env = environment(),
                assign.env = envir)
}

#' @rdname cacheAssign
#' @importFrom lazyeval lazy
#' @export
`%<%` <- function(x, y) {
  x <- match.call()$x %>% as.character
  y1 <- lazy(y)
  assignCache(x, y, envir=y1$env)
}
