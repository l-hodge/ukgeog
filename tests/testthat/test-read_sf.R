# Admin

test_that("UK is four countries", {
  expect_equal(nrow(read_sf("NAT")), 4)
})

test_that("England is nine regions", {
  expect_equal(nrow(read_sf("GOR")), 9)
})

test_that("Bounds of lat/long", {
  x <- read_sf("NAT")
  expect_gt(sf::st_bbox(x)$ymin, 49.8)
  expect_gt(sf::st_bbox(x)$xmin, -8.7)
  expect_lt(sf::st_bbox(x)$ymax, 60.9)
  expect_lt(sf::st_bbox(x)$xmax, 1.8)
})

test_that("There are 314 Local Authority Districts in England in 2020", {
  expect_equal(nrow(read_sf("LAD", year = 2020, nations = "E")), 314)
})

# Census

test_that("MSOA", {
  x <- read_sf("MSOA", year = 2011)
  expect_equal(nrow(x), 7201)

  expect_gt(sf::st_bbox(x)$ymin, 49.8)
  expect_gt(sf::st_bbox(x)$xmin, -8.7)
  expect_lt(sf::st_bbox(x)$ymax, 60.9)
  expect_lt(sf::st_bbox(x)$xmax, 1.8)
})

test_that("LSOA", {
  y <- read_sf("LSOA", year = 2011)
  expect_equal(nrow(y), 34753)

  expect_gt(sf::st_bbox(y)$ymin, 49.8)
  expect_gt(sf::st_bbox(y)$xmin, -8.7)
  expect_lt(sf::st_bbox(y)$ymax, 60.9)
  expect_lt(sf::st_bbox(y)$xmax, 1.8)
})

# Electoral
test_that("UK has 650 westminster constituencies", {
  expect_equal(nrow(read_sf("WM", year = 2018)), 650)
})

# Eurostat
test_that("sf1", {
  x <- read_sf("NUTS1", year = 2018)
  y <- read_sf("NUTS1", year = 2015)
  expect_equal(nrow(x), 12)
  expect_equal(nrow(y), 10)

  expect_gt(sf::st_bbox(x)$ymin, 49.8)
  expect_gt(sf::st_bbox(x)$xmin, -8.7)
  expect_lt(sf::st_bbox(x)$ymax, 60.9)
  expect_lt(sf::st_bbox(x)$xmax, 1.8)


})

test_that("NUTS2", {
  x <- read_sf("NUTS2", year = 2018)
  expect_equal(nrow(x), 41)

  expect_gt(sf::st_bbox(x)$ymin, 49.8)
  expect_gt(sf::st_bbox(x)$xmin, -8.7)
  expect_lt(sf::st_bbox(x)$ymax, 60.9)
  expect_lt(sf::st_bbox(x)$xmax, 1.8)
})

test_that("NUTS3", {
  x <- read_sf("NUTS3", year = 2018)
  expect_equal(nrow(x), 179)

  expect_gt(sf::st_bbox(x)$ymin, 49.8)
  expect_gt(sf::st_bbox(x)$xmin, -8.7)
  expect_lt(sf::st_bbox(x)$ymax, 60.9)
  expect_lt(sf::st_bbox(x)$xmax, 1.8)
})

test_that("Namespace clashes resolve", {

sf <- read_sf(geog = "NAT", year = 2021)
sf2 <- read_sf(geog = "GOR", year = 2021)
expect_false(identical(sf, sf2))

})
