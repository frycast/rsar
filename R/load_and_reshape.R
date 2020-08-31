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
#' original \code{\link[raster]{brick}}. This function
#' uses \code{\link[reticulate]{array_reshape}} with
#' default \code{order = "C"}, meaning
#' that elements are read during
#' rearrangement in row-major order, so
#' that the last index changes the fastest.
#'
#' @param filename This is usually a string giving absolute or relative
#' path to a \code{tif} file containing a SAR raster brick. This argument is
#' passed to \code{\link[raster]{brick}}.
#' @param drop_na A logical. If \code{TRUE} then any
#' \code{NA} pixels are dropped, but a record is kept of their
#' location so that the original raster brick can be easily
#' reconstructed later with \code{\link[rsar]{matrix_to_brick}}.
#' Note, to save time, only one layer is checked for \code{NA}
#' pixels, i.e., it is assumed that an \code{NA} pixel is
#' \code{NA} across all layers.
#' @param na_layer An integer specifying the index of the
#' layer to check for \code{NA} values if \code{drop_na}
#' is \code{TRUE}.
#' @param set_na_indices Optional integer vector specifying a
#' vector of row indices in the \code{SAR_matrix} that should be
#' replaced by \code{NA} pixels.
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
#' load_SAR_matrix(filename, set_na_indices = c(1,2))
#'
load_SAR_matrix <- function(filename, drop_na = TRUE, na_layer = 1L,
                            set_na_indices = integer(0) ) {
  return(brick_to_matrix(
    b = raster::brick( filename ), drop_na = drop_na, na_layer = na_layer,
    set_na_indices = set_na_indices ))
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
#' @param drop_na A logical. If \code{TRUE} then any
#' \code{NA} pixels are dropped, but a record is kept of their
#' location so that the original raster brick can be easily
#' reconstructed later with \code{\link[rsar]{matrix_to_brick}}.
#' Note, to save time, only one layer is checked for \code{NA}
#' pixels, i.e., it is assumed that an \code{NA} pixel is
#' \code{NA} across all layers.
#' @param na_layer An integer specifying the index of the
#' layer to check for \code{NA} values if \code{drop_na}
#' is \code{TRUE}.
#' @param set_na_indices Optional integer vector specifying a
#' vector of row indices in the \code{SAR_matrix} that should be
#' replaced by \code{NA} pixels.
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
#' m1 <- brick_to_matrix(b)
#' m2 <- brick_to_matrix( b, set_na_indices = c(1,2) )
#'
brick_to_matrix <- function(b, drop_na = TRUE, na_layer = 1L,
                            set_na_indices = integer(0) ) {
  b_dim <- dim( b )
  d <- c( b_dim[ 1L ] * b_dim[ 2L ], b_dim[ 3L ] )
  m <- reticulate::array_reshape( raster::as.array( b ), dim = d )

  na_indices <- integer(0)
  if ( drop_na ) {
    na_indices <- sort(c( which( is.na( m[ , na_layer ] ) ), set_na_indices ))
    if ( length( na_indices ) > 0 ) m <- m[ -na_indices, ]
  }

  m <- SAR_matrix(as.matrix( m ), extent = raster::extent( b ),
                  crs = raster::crs( b ),
                  brick_nrow = b_dim[ 1L ],
                  brick_ncol = b_dim[ 2L ],
                  brick_names = names( b ),
                  brick_na_indices = na_indices )

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
#' head(m)
#' matrix_to_brick(m)
#' raster::plot(m)
#'
matrix_to_brick <- function( m ) {
  assertthat::assert_that( is_SAR_matrix( m ) )

  na_indices <- attr( m, "brick_na_indices" )
  na_len <- length( na_indices )

  if ( na_len > 0 ) {
    len <- nrow( m ) + na_len
    m_new <- matrix( NA, nrow = len, ncol = ncol(m) )
    m_new[ -na_indices, ] <- m
  } else {
    m_new <- m
  }

  dim <- attr( m, "brick_dim" )
  b <- raster::brick( reticulate::array_reshape( m_new, dim = dim ),
                      crs = attr( m, "crs" ) )

  raster::extent( b ) <- attr( m, "extent" )
  names( b ) <- attr( m, "brick_names" )
  return( b )
}



#' patchify_SAR_matrix
#'
#' Reduce the resolution of images (columns) in a
#' \code{\link[rsar]{SAR_matrix}} object \code{m},
#' by aggregating all values inside \eqn{d \times d} patches
#' using the aggregation function \code{fun}.
#'
#' @param m An \code{SAR_matrix}  object, such as that output
#' by \code{\link[rsar]{load_SAR_matrix}}.
#' @param d The dimension of patches.
#' @param fun The aggregation function. Default is mean.
#'
#' @return
#' A \code{\link[rsar]{SAR_matrix}} of reduced dimension,
#' with appropriately adjusted raster attributes.
#'
#' @export
#'
#' @examples
#' filename <- system.file(
#'   "extdata", "MG_CC_sub_norm_testclip.tif", package="rsar")
#' m <- load_SAR_matrix(filename)
#' dim(m)
#' mp <- patchify_SAR_matrix(m,4)
#' head(mp)
#' dim(mp)
#' bp <- matrix_to_brick(mp)
#' raster::plot(bp)
#'
patchify_SAR_matrix <- function(m, d, fun = mean) {
  img_dim <- attr(m, "brick_dim")[1:2]

  attribs <- NULL
  patch <- function(x, fun) {
    p <- patchify(x, n = d, m = d, img_dim = img_dim, fun = fun)
    attribs <<- attributes(p)
    return(p)
  }
  m_fun <- apply(m, MARGIN = 2, FUN = patch, fun = fun)
  m_patch <- SAR_matrix( m_fun, attr_src = m,
                         brick_ncol = attribs$padded_brick_ncol,
                         brick_nrow = attribs$padded_brick_nrow )
  attr(m_patch, "d1_pad") <- attribs$d1_pad
  attr(m_patch, "d2_pad") <- attribs$d2_pad
  return(m_patch)
}

# Function takes a vector (column of a SAR_matrix) and
# aggregates within n x m patches (provided that n
# and m are divisors of img_dim[1] and img_dim[2]
# respectively). Returns a (shorter) vector of the aggregates.
# WARNING: Don't remove NA indices before patchify.
patchify <- function(v, n, m, img_dim, fun = sum) {
  d1 <- img_dim[1]
  d2 <- img_dim[2]

  # Padding with zeros if needed
  nrem <- d1 %% n
  mrem <- d2 %% m
  d1_pad <- 0
  d2_pad <- 0
  if ( nrem != 0 || mrem != 0  ) {
    v1m <- matrix(v, nrow = d1, ncol = d2, byrow = T)
    if ( nrem != 0 ) {
      v1m <- rbind( v1m, matrix(0, nrow = n - nrem, ncol = d2) )
      d1_pad <- n - nrem
      d1 <- d1 + d1_pad
    }
    if ( mrem != 0 ) {
      v1m <- cbind( v1m, matrix(0, nrow = d1, ncol = m - mrem) )
      d2_pad <- m - mrem
      d2 <- d2 + d2_pad
    }
    v <- as.vector(t(v1m))
  }

  # All the action happens here
  a <- reticulate::array_reshape(v, dim = c(m, d2/m, n, d1/n), order = "F")
  p <- 1
  patches <- vector(length = (d1*d2)/(n*m), mode = "numeric")
  for ( k in 1:(dim(a)[4]) ) {
    for (l in 1:(dim(a)[2]) ) {
      patches[p] <- fun(a[,l,,k])
      p <- p + 1
    }
  }

  attr(patches, "padded_brick_nrow") <- d1/n
  attr(patches, "padded_brick_ncol") <- d2/m
  attr(patches, "d1_pad") <- d1_pad
  attr(patches, "d2_pad") <- d2_pad
  return(patches)
}
