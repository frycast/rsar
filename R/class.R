#' SAR_matrix
#'
#' Construct a \code{SAR_matrix} object from a matrix \code{m}.
#' Each column of a \code{SAR_matrix} corresponds to a band in
#' a SAR raster brick (i.e., to the depth dimension),
#' and each row corresponds to a pixel.
#'
#' @param m A matrix with raster intensities.
#' @param extent An \code{\link[raster]{extent}} object.
#' @param crs A string specifying the coordinate reference system
#' (as in \code{\link[raster]{crs}}).
#' @param brick_nrow A single integer specifying the number of rows
#' (in pixels) of each band in the brick. Note that
#' \code{brick_nrow * brick_nrow} must equal \code{nrow(m)}.
#' The default value of 2 is for demonstration and
#' is unlikely to be useful.
#' @param brick_ncol A single integer specifying the number of
#' columns (in pixels) of each band in the brick. Note that
#' \code{brick_nrow * brick_nrow} must equal \code{nrow(m)}.
#' @param brick_names A vector of length \code{ncol(m)}
#' labelling each band in the raster \code{\link[raster]{brick}}.
#'
#' @return
#' A \code{SAR_matrix} object; a specialisation
#' of class matrix that includes geospatial
#' and brick dimension attributes.
#'
#' @export
#'
#' @examples
#' SAR_matrix()
#'
SAR_matrix <- function(
  m = matrix(0, 4, 3), extent = raster::extent(raster::raster()),
  crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0",
  brick_nrow = 2, brick_ncol = nrow(m) / brick_nrow,
  brick_names = paste0( "layer.", 1:ncol(m) )) {

  assertthat::assert_that( brick_nrow * brick_ncol == nrow(m) )
  assertthat::assert_that( length(brick_nrow) == 1 )
  assertthat::assert_that( length(brick_ncol) == 1 )
  assertthat::assert_that( length(brick_names) == ncol(m) )

  m <- structure(
    m, class = c("SAR_matrix", class(m)),
    extent = extent,
    crs = crs,
    brick_dim = c( brick_nrow, brick_ncol, ncol(m) ),
    brick_names = brick_names)
  return(m)
}





#' is_SAR_matrix
#'
#' Check if an object is of class \code{\link[rsar]{SAR_matrix}}.
#'
#' @param m An object
#'
#' @return
#' Logical \code{TRUE} if \code{m} is a \code{\link[rsar]{SAR_matrix}}
#' object, otherwise \code{FALSE}.
#'
#' @export
#'
#' @examples
#' filename <- system.file(
#'   "extdata", "MG_CC_sub_norm_testclip.tif", package="rsar")
#' m <- load_SAR_matrix(filename)
#' is_SAR_matrix(m)
#'
is_SAR_matrix <- function(m) {
  inherits(m, "SAR_matrix")
}

