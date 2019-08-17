filename_cc <- system.file(
  "extdata", "MG_CC_sub_norm_testclip.tif", package="rsar")
filename_vh <- system.file(
  "extdata", "MG_VH_sub_norm_testclip.tif", package="rsar")

test_that("data loads and reshapes", {
  cc_m1 <- load_SAR_matrix(filename_cc)
  cc_b1 <- raster::brick(filename_cc)
  cc_m2 <- brick_to_matrix(cc_b1)
  cc_b2 <- matrix_to_brick(cc_m1)

  vh_m1 <- load_SAR_matrix(filename_vh)
  vh_b1 <- raster::brick(filename_vh)
  vh_m2 <- brick_to_matrix(vh_b1)
  vh_b2 <- matrix_to_brick(vh_m1)

  m1 <- SAR_matrix()

  expect_equal(dim(cc_m1), dim(cc_m2))
  expect_equal(dim(cc_b1), dim(cc_b2))
  expect_equal(dim(vh_m1), dim(vh_m2))
  expect_equal(dim(vh_b1), dim(vh_b2))

  testthat::expect_gt(dim(cc_m2)[1], 1)
  testthat::expect_gt(dim(cc_m2)[2], 1)
  testthat::expect_gt(dim(vh_m2)[1], 1)
  testthat::expect_gt(dim(vh_m2)[2], 1)

  testthat::expect_gt(dim(cc_b2)[1], 1)
  testthat::expect_gt(dim(cc_b2)[2], 1)
  testthat::expect_gt(dim(cc_b2)[3], 1)
  testthat::expect_gt(dim(vh_b2)[1], 1)
  testthat::expect_gt(dim(vh_b2)[2], 1)
  testthat::expect_gt(dim(vh_b2)[3], 1)

  testthat::expect_gt(dim(m1)[1], 1)
  testthat::expect_gt(dim(m1)[2], 1)
})
