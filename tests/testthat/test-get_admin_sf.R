test_that("UK is four countries", {
  expect_equal(nrow(get_admin_sf("NAT")), 4)
})

test_that("England is nine regions", {
  expect_equal(nrow(get_admin_sf("GOR")), 9)
})

test_that("Bounds of lat/long", {
  x <- get_admin_sf("NAT")
  expect_gt(min(x$lat), 49.85)
  expect_gt(min(x$long), -8.64)
  expect_lt(min(x$lat), 60.87)
  expect_lt(min(x$long), 1.77)
})

test_that("Test error messaging", {
  expect_error(get_admin_sf("NAT", year = 2017), "'year' must be either 2018 or 2019")
})
