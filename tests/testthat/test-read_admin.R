test_that("UK is four countries", {
  expect_equal(nrow(read_admin("NAT")), 4)
})

test_that("England is nine regions", {
  expect_equal(nrow(read_admin("GOR")), 9)
})

test_that("Bounds of lat/long", {
  x <- read_admin("NAT")
  expect_gt(sf::st_bbox(x)$ymin, 49.8)
  expect_gt(sf::st_bbox(x)$xmin, -8.7)
  expect_lt(sf::st_bbox(x)$ymax, 60.9)
  expect_lt(sf::st_bbox(x)$xmax, 1.8)
})

test_that("Test error messaging", {
  expect_error(read_admin("NAT", year = 2017), "'year' must be either 2018 or 2019")
})

test_that("There are 314 Local Authority Districts in England in 2020", {
  expect_equal(nrow(read_admin("LAD", year = 2020, nations = "E")), 314)
})
