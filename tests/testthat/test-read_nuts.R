test_that("NUTS1", {
  x <- read_nuts("NUTS1", year = 2018)
  y <- read_nuts("NUTS1", year = 2015)
  expect_equal(nrow(x), 12)
  expect_equal(nrow(y), 10)
  
  expect_gt(sf::st_bbox(x)$ymin, 49.8)
  expect_gt(sf::st_bbox(x)$xmin, -8.7)
  expect_lt(sf::st_bbox(x)$ymax, 60.9)
  expect_lt(sf::st_bbox(x)$xmax, 1.8)
  
  
})

test_that("NUTS2", {
  x <- read_nuts("NUTS2", year = 2018)
  expect_equal(nrow(x), 41)
  
  expect_gt(sf::st_bbox(x)$ymin, 49.8)
  expect_gt(sf::st_bbox(x)$xmin, -8.7)
  expect_lt(sf::st_bbox(x)$ymax, 60.9)
  expect_lt(sf::st_bbox(x)$xmax, 1.8)
})

test_that("NUTS3", {
  x <- read_nuts("NUTS3", year = 2018)
  expect_equal(nrow(x), 179)
  
  expect_gt(sf::st_bbox(x)$ymin, 49.8)
  expect_gt(sf::st_bbox(x)$xmin, -8.7)
  expect_lt(sf::st_bbox(x)$ymax, 60.9)
  expect_lt(sf::st_bbox(x)$xmax, 1.8)
})