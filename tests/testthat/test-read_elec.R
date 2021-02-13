test_that("UK has 650 westminster constituencies", {
  expect_equal(nrow(read_elec("WM")), 650)
})
