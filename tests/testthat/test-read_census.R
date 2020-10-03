test_that("MSOA", {
  x <- read_census("MSOA")
  expect_equal(nrow(x), 7201)

  expect_gt(sf::st_bbox(x)$ymin, 49.8)
  expect_gt(sf::st_bbox(x)$xmin, -8.7)
  expect_lt(sf::st_bbox(x)$ymax, 60.9)
  expect_lt(sf::st_bbox(x)$xmax, 1.8)
})

test_that("LSOA", {
  y <- read_census("LSOA")
  expect_equal(nrow(y), 34753)

  expect_gt(sf::st_bbox(y)$ymin, 49.8)
  expect_gt(sf::st_bbox(y)$xmin, -8.7)
  expect_lt(sf::st_bbox(y)$ymax, 60.9)
  expect_lt(sf::st_bbox(y)$xmax, 1.8)
})

test_that("OA", {
  z <- read_census("OA")
  expect_equal(nrow(z), 181408)

  expect_gt(sf::st_bbox(z)$ymin, 49.8)
  expect_gt(sf::st_bbox(z)$xmin, -8.7)
  expect_lt(sf::st_bbox(z)$ymax, 60.9)
  expect_lt(sf::st_bbox(z)$xmax, 1.8)
})

