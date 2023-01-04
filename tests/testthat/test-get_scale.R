test_that("get_scale works", {
  expect_equal(nrow(get_scale("1:20,000")), 2)
  expect_equal(nrow(get_scale("1:20,000", standard = "USGS")), 1)
  expect_equal(nrow(get_scale(series = "Puerto Rico 7.5 minute")), 1)
  expect_error(get_scale("1:20,000", standard = "USGSS"))
  expect_error(get_scale("1:20,000", standard = "USGSS"))
  expect_error(get_scale(series = "Puerto Rico 7.5 minutes"))
})
