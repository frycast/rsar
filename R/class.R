
##########################
### WRITE TESTS (X)
##########################
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
#' @param brick_ncol A single integer specifying the number of
#' columns (in pixels) of each band in the brick. Note that
#' \code{brick_nrow * brick_nrow} must equal \code{nrow(m)}.
#' @param brick_names A vector of length \code{ncol(m)}
#' labelling each band in the raster \code{\link[raster]{brick}}.
#' @param brick_na_indices Integer vector giving row indices for locations
#' where \code{NA} pixels exist in the
#' corresponding raster \code{\link[raster]{brick}}.
#' @param attr_src Either another \code{SAR_matrix} or a
#' raster \code{\link[raster]{brick}}. If this argument is not
#' missing then all other arguments to this function will be sourced
#' from \code{attr_src} (except for the argument \code{brick_names},
#' since \code{attr_src} is understood to possible have fewer
#' columns than \code{m}).
#'
#' @return
#' A \code{SAR_matrix} object; a specialisation
#' of class matrix that includes geospatial
#' and brick dimension attributes.
#'
#' @export
#'
#' @examples
#' SAR_matrix(m = matrix(0, 4, 3),
#'            brick_nrow = 4, brick_ncol = 3)
#'
#'
SAR_matrix <- function(
  m, extent = raster::extent( raster::raster() ),
  crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0",
  brick_nrow, brick_ncol,
  brick_names = paste0( "layer.", 1:ncol( m ) ),
  brick_na_indices = integer(0), attr_src) {

  assertthat::assert_that( is.matrix(m) )
  assertthat::assert_that( length( brick_names ) == ncol( m ) )

  if ( missing(attr_src) ) {
    assertthat::assert_that( length( brick_nrow ) == 1 )
    assertthat::assert_that( length( brick_ncol ) == 1 )
    assertthat::assert_that(
      (nrow(m) + length(brick_na_indices)) %% brick_nrow*brick_ncol == 0  )

    m <- structure(
      m, class = c( "SAR_matrix", class( m ) ),
      extent = extent,
      crs = crs,
      brick_dim = c( brick_nrow, brick_ncol, ncol( m ) ),
      brick_names = brick_names,
      brick_na_indices = brick_na_indices )
  } else {

    if(missing(brick_nrow)) {
      brick_nrow <- attr( attr_src, "brick_dim" )[ 1 ]
    }

    assertthat::assert_that( length( brick_nrow ) == 1 )

    if(missing(brick_ncol)) {
      brick_ncol <- attr( attr_src, "brick_dim" )[ 2 ]
    }

    assertthat::assert_that( length( brick_ncol ) == 1 )

    assertthat::assert_that(
      (nrow(m) + length(brick_na_indices)) %% brick_nrow*brick_ncol == 0  )

    m <- SAR_matrix(
      m,
      extent = attr( attr_src, "extent" ),
      crs = attr( attr_src, "crs" ),
      brick_nrow = brick_nrow,
      brick_ncol = brick_ncol,
      brick_na_indices = attr( attr_src, "brick_na_indices" ))
  }
  return(m)
}



##########################
### WRITE TESTS (X)
##########################
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

