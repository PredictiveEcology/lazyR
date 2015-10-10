################################################################################
#' The \code{.spatialObjects} class
#'
#' This class is the union of several spatial objects from raster and sp packages.
#' Notably missing is \code{RasterBrick}, for now.
#'
#' @slot members  SpatialPoints*, SpatialPolygons*, SpatialLines*,
#'                RasterLayer, RasterStack
#'
#' @aliases .spatialObjects
#' @importClassesFrom raster RasterLayer RasterLayerSparse RasterStack
#' @importClassesFrom sp SpatialLines SpatialLinesDataFrame
#' @importClassesFrom sp SpatialPixels SpatialPixelsDataFrame
#' @importClassesFrom sp SpatialPoints SpatialPointsDataFrame
#' @importClassesFrom sp SpatialPolygons SpatialPolygonsDataFrame
#' @name .spatialObjects-class
#' @rdname spatialObjects-class
#' @author Eliot McIntire
setClassUnion(name=".spatialObjects",
              members=c("SpatialPoints", "SpatialPolygons", "SpatialLines",
                        "RasterLayer", "RasterStack")
)
