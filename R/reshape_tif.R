
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
  brick_names = paste0( "layer.", 1:ncol(m) ) ) {

  assertthat::assert_that( brick_nrow * brick_ncol == nrow(m) )
  assertthat::assert_that( length(brick_nrow) == 1 )
  assertthat::assert_that( length(brick_ncol) == 1 )
  assertthat::assert_that( length(brick_names) == ncol(m) )

  m <- structure(
    m, class = c("SAR_matrix", class(m)),
    brick_extent = extent,
    brick_crs = crs,
    brick_dim = c( brick_nrow, brick_ncol, ncol(m) ),
    brick_names = brick_names)
  return(m)
}


#' load_SAR_matrix
#'
#' Given a file path to a SAR raster image
#' in \code{tif} format, convert the raster brick of
#' dimensions \code{A x B x C} into a matrix of
#' dimensions \code{A*B x C} while keeping
#' track of the geospatial and dimension attributes of the
#' original \code{\link[raster]{brick}}.
#'
#' @param filename This is usually a string giving absolute or relative
#' path to a \code{tif} file containing a SAR raster brick. This argument is
#' passed to \code{\link[raster]{brick}}.
#'
#' @return
#' An object of class \code{\link[rsar]{SAR_matrix}}; a specialisation
#' of class matrix.
#'
#' @export
#'
#' @examples
#' filename <- system.file(
#'   "extdata", "MG_CC_sub_norm_testclip.tif", package="rsar")
#' load_SAR_matrix(filename)
#'
load_SAR_matrix <- function(filename) {
  return(brick_to_matrix(raster::brick(filename)))
}


#' brick_to_matrix
#'
#' Convert a raster \code{\link[raster]{brick}} of
#' dimensions \code{A x B x C} into a matrix of
#' dimensions \code{A*B x C} while keeping
#' track of the geospatial and dimension attributes of the
#' original brick.
#'
#' @param b A raster \code{\link[raster]{brick}}.
#'
#' @return
#' An object of class \code{\link[rsar]{SAR_matrix}}; a specialisation
#' of class matrix.
#'
#' @export
#'
#' @examples
#' filename <- system.file(
#'   "extdata", "MG_CC_sub_norm_testclip.tif", package="rsar")
#' b <- raster::brick(filename)
#' m <- brick_to_matrix(b)
#'
brick_to_matrix <- function(b) {
  b_dim <- dim(b)
  d <- c(b_dim[1L]*b_dim[2L], b_dim[3L])
  m <- reticulate::array_reshape(raster::as.array(b), dim = d)

  brick_attr <- attributes(b)[c("extent", "crs")]
  m <- SAR_matrix(m, extent = brick_attr$extent,
                  brick_crs = brick_attr$crs,
                  brick_nrow = b_dim[1L],
                  brick_ncol = b_dim[2L],
                  brick_names = names(b))
  return(m)
}


#' matrix_to_brick
#'
#' Convert an \code{\link[rsar]{SAR_matrix}} object \code{m},
#' such as that output by \code{\link[rsar]{load_SAR_matrix}}, into a
#' raster \code{\link[raster]{brick}} with the
#' geospatial and dimension attributes stored in \code{m}.
#'
#' @param m An \code{SAR_matrix}  object, such as that output
#' by \code{\link[rsar]{load_SAR_matrix}}.
#'
#' @return
#' A raster \code{\link[raster]{brick}}.
#'
#' @export
#'
#' @examples
#' filename <- system.file(
#'   "extdata", "MG_CC_sub_norm_testclip.tif", package="rsar")
#' m <- load_SAR_matrix(filename)
#' matrix_to_brick(m)
#'
matrix_to_brick <- function(m) {
  dim <- attr(m, "brick_dim")
  b <- raster::brick(reticulate::array_reshape(m, dim = dim))

  return(b)
}



