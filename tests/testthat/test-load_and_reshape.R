# attributes(x)$extent@ymin

filename <- system.file(
  "extdata", "MG_VH_sub_norm_testclip.tif", package="rsar")


testthat::test_that('Tif data loads correctly', {

  x <- load_SAR_matrix(filename)
  testthat::expect_equal(class(x), c('SAR_matrix', 'matrix'))
  testthat::expect_equal(typeof(x), 'double')
  testthat::expect_equal(dim(x), c('2150', '30'))
  testthat::expect_equal(attributes(x)$brick_dim, c(43, 50, 30))
  testthat::expect_equal(attributes(x)$crs@projargs, "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
})


test_that("data loads and reshapes", {

  vh_m1 <- load_SAR_matrix(filename)
  vh_b1 <- raster::brick(filename)
  vh_m2 <- brick_to_matrix(vh_b1)
  vh_b2 <- matrix_to_brick(vh_m1)

  m1 <- SAR_matrix()

  expect_equal(dim(vh_m1), dim(vh_m2))
  expect_equal(dim(vh_b1), dim(vh_b2))
  expect_equal(summary(vh_m1)[1], summary(vh_m2)[1])
  expect_equal(summary(vh_b1)[1], summary(vh_b2)[1])

  expect_equal(attr(vh_m1,"brick_dim"), c(43, 50, 30))

  testthat::expect_gt(dim(vh_m2)[1], 1)
  testthat::expect_gt(dim(vh_m2)[2], 1)

  testthat::expect_gt(dim(vh_b2)[1], 1)
  testthat::expect_gt(dim(vh_b2)[2], 1)
  testthat::expect_gt(dim(vh_b2)[3], 1)

  testthat::expect_gt(dim(m1)[1], 1)
  testthat::expect_gt(dim(m1)[2], 1)
})
