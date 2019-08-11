#' reshaper
#'
#' @param br_a
#' @param m
#'
#' @return
#' @export
#'
#' @examples
reshaper <- function(br_a, m) {
  if (missing(m)) {
    return(reticulate::array_reshape(br_a, dim = c(dim(br_a)[1]*dim(br_a)[2], dim(br_a)[3])))

  } else {
    dim <- c(dim(br_a)[1], dim(br_a)[2], dim(br_a)[3])
    br_a2 <- reticulate::array_reshape(m, dim = dim)
    br_b <- raster::brick(br_a2)
    return(br_b)
  }
}

#' give_attrs
#'
#' @param br_b
#' @param br
#' @param incl_names
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
#' @param br_a
#' @param m
#' @param k
#'
#' @return
#' @export
#'
#' @examples
reshapekDL <- function(br_a, m, k = 2) {
  dim <- c(dim(br_a)[1], dim(br_a)[2], k)
  br_a2 <- reticulate::array_reshape(m, dim = dim)
  br_b <- raster::brick(br_a2)
  return(br_b)
}
