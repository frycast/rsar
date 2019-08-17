filename <- system.file(
   "extdata", "MG_CC_sub_norm_testclip.tif", package="rsar")
b <- raster::brick(filename)


m <- rsar::brick_to_matrix(b)
b2 <- rsar::matrix_to_brick(m)
