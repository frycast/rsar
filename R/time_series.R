##########################
### WRITE TESTS (X)
##########################
#' choose_order
#'
#' Take a random sample (of size \code{sample_size})
#' of the rows of \code{m}, then fit an AR model to
#' each row in the sample, using
#' AIC to determine the optimal order.
#' Return the largest order across all sampled
#' rows.
#'
#' @param m A matrix.
#' @param sample_size An integer smaller than
#' \code{nrow(m)}.
#'
#' @return
#' An integer.
#' B
#'
#' @importFrom stats ar
#'
#' @export
#'
#' @examples
#' filepath <- system.file("extdata", "MG_CC_sub_norm_testclip.tif", package="rsar")
#' m <- load_SAR_matrix(filepath)
#' order <- choose_order(m)
#'
#'
choose_order <- function(m, sample_size = 1e3L) {
  if ( nrow( m ) > sample_size ) {
    ms <- m[sample(1:nrow(m), sample_size),]
  } else {
    ms <- m
  }
  max_or <- 0
  for (i in 1:nrow( ms )) {
    or <- ar( ms[i,] )$order
    if ( or > max_or ) {max_or <- or}
  }
  return( as.integer( max_or ) )
}


##########################
### WRITE TESTS (X)
##########################
#' fit_AR_to_SAR
#'
#' Fit an AR model with the chosen \code{order}
#' to each row of \code{m} using \code{\link[stats]{ar}}
#' (so each row of \code{m} is treated as a single
#' time series). The results are returned in a matrix whose
#' columns are the resulting AR coefficients.
#'
#' @param m A matrix.
#' @param order An integer giving the order of the AR
#' model. The default uses \code{\link[rsar]{choose_order}}
#' to choose automatically.
#'
#' @return
#' A matrix of AR coefficients where each row corresponds to
#' one row of \code{m}.
#'
#' @importFrom stats ar
#'
#' @export
#'
#' @examples
#' filepath <- system.file("extdata", "MG_CC_sub_norm_testclip.tif", package="rsar")
#' m <- load_SAR_matrix(filepath)
#' ar_m <- fit_AR_to_SAR(m)
#'
fit_AR_to_SAR  <- function(m, order = choose_order(m)) {
  ar_m <- t(apply(m, MARGIN = 1, function(x){ ar(x, order.max = order, aic = F)$ar }))
  colnames(ar_m) <- paste0("p", 1:order)
  return(ar_m)
}
