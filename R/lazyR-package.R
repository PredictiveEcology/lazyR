#
#  lazyR/R/lazyR-package.R by Eliot J B McIntire
#  Copyright (C) 2015 Her Majesty the Queen in Right of Canada,
#   as represented by the Minister of Natural Resources Canada
#

#' Categorized overview of the \code{lazyR} package
#'
#' @description
#'
#'
#' All lazyR databases act as archivist repositories. This means that any functions 
#' that work with an archivist repository will work with a lazyR database.
#'
#' Online Vignette: \url{https://github.com/PredictiveEcology/lazyR/blob/master/README.md}
#' 
#' Bug reports: \url{https://github.com/PredictiveEcology/lazyR/issues}
#'
#' @name lazyR-package
#' @aliases lazyR
#' @docType package
#' @author Eliot J. B. McIntire \email{Eliot.McIntire@@canada.ca}
#' @keywords package
#'
#' ------------------------------------------------------------------------------------------
#'
#' @section 1 Saving:
#'
#' \tabular{ll}{
#'   \code{\link{lazySave}} \tab save objects to a lazy database\cr
#' }
#'
#' @section 2 Loading:
#'
#' \tabular{ll}{
#'   \code{\link{lazyLoad2}} \tab load lazy databases\cr
#' }
#' 
#' @section 3 Working with lazy load databases:
#'
#' 
#' \tabular{ll}{
#'   \code{\link{lazyLs}} \tab equivalent to ls(), but for lazyR database\cr
#'   \code{\link{lazyIs}} \tab equivalent to is(), but for lazyR database\cr
#'   \code{\link{lazyRm}} \tab equivalent to rm(), but for lazyR database\cr
#' }
#' 
#' @section 4 Caching:
#'
#' \tabular{ll}{
#'   \code{\link{\%<\%}} \tab cache and assign in one step\cr
#' }
#' 
#' @section 5 Set system-wide lazy database:
#'
#' \tabular{ll}{
#'   \code{\link{lazyDir}} \tab set or get system-wide lazyDir\cr
#' }
#' 
#' @section 6 Copy entire lazyR and all files to a new location:
#'
#' \tabular{ll}{
#'   \code{\link{copyDir}} \tab Copy all files to new lazyR database location\cr
#' }
NULL
