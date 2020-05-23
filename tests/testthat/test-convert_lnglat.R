test_that("Correct Number of Rows and Columns", {
  df <- as.data.frame(cbind(540291, 168873))
  names(df) <- c("easting", "northing")

  expect_equal(nrow(convert_lnglat(df, "easting", "northing")), nrow(df))
  expect_equal(ncol(convert_lnglat(df, "easting", "northing")), ncol(df) + 2)
})
