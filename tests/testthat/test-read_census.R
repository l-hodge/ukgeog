test_that("MSOA", {
  x <- read_census("MSOA")
  expect_equal(nrow(x), 7201)

  expect_gt(min(x$lat), 49.85)
  expect_gt(min(x$long), -8.64)
  expect_lt(min(x$lat), 60.87)
  expect_lt(min(x$long), 1.77)
})

test_that("LSOA", {
  y <- read_census("LSOA")
  expect_equal(nrow(y), 34753)

  expect_gt(min(y$lat), 49.85)
  expect_gt(min(y$long), -8.64)
  expect_lt(min(y$lat), 60.87)
  expect_lt(min(y$long), 1.77)
})

test_that("OA", {
  z <- read_census("OA")
  expect_equal(nrow(z), 181408)

  expect_gt(min(z$lat), 49.85)
  expect_gt(min(z$long), -8.64)
  expect_lt(min(z$lat), 60.87)
  expect_lt(min(z$long), 1.77)
})

