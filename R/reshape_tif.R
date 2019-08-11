#' reshaper
#'
#' @param br_a A raster brick
#' @param m A matrix
#'
#' @return
#' @export
#'
#' @examples
reshaper <- function(br_a, m) {
  if (missing(m)) {
    return(reticulate::array_reshape(br_a, dim = c(dim(br_a)[1L]*dim(br_a)[2L], dim(br_a)[3L])))

  } else {
    dim <- c(dim(br_a)[1L], dim(br_a)[2L], dim(br_a)[3L])
    br_a2 <- reticulate::array_reshape(m, dim = dim)
    br_b <- raster::brick(br_a2)
    return(br_b)
  }
}

#' give_attrs
#'
#' @param br_b A raster brick
#' @param br A raster brick
#' @param incl_names Logical
#'
#' @return
#' @export
#'
#' @examples
give_attrs <- function(br_b, br, incl_names = TRUE) {
  raster::crs(br_b) <- raster::crs(br)
  raster::extent(br_b) <- raster::extent(br)
  if (incl_names) { names(br_b) <- names(br) }
  return(br_b)
}

#' reshapekDL
#'
#' @param br_a A raster brick
#' @param m A matrix
#' @param k An integer
#'
#' @return
#' @export
#'
#' @examples
reshapekDL <- function(br_a, m, k = 2L) {
  dim <- c(dim(br_a)[1L], dim(br_a)[2L], k)
  br_a2 <- reticulate::array_reshape(m, dim = dim)
  br_b <- raster::brick(br_a2)
  return(br_b)
}
