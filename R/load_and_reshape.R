
##########################
### WRITE TESTS (X)
##########################
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


##########################
### WRITE TESTS (X)
##########################
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

  m <- SAR_matrix(m, extent = raster::extent(b),
                  crs = raster::crs(b),
                  brick_nrow = b_dim[1L],
                  brick_ncol = b_dim[2L],
                  brick_names = names(b))
  return(m)
}


##########################
### WRITE TESTS (X)
##########################
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
  assertthat::assert_that(is_SAR_matrix(m))

  dim <- attr(m, "brick_dim")
  b <- raster::brick(reticulate::array_reshape(m, dim = dim),
                     crs = attr(m, "crs"))

  raster::extent(b) <- attr(m, "extent")
  names(b) <- attr(m, "names")
  return(b)
}



