test_that("convert_dist_scale works", {
  expect_equal(
    convert_dist_scale(1, scale = "1 in = 10 feet"),
    units::as_units(10, "ft")
  )

  expect_equal(
    convert_dist_scale(paper = "letter", scale = "1 in = 10 feet")[["actual_width"]],
    units::as_units(85, "ft")
  )

  expect_equal(
    convert_dist_scale(dist = 100, scale_unit = "mm", actual_unit = "km", scale_factor = 1),
    units::as_units(10, "km")
  )
})

test_that("convert_dist_scale warns", {
  expect_warning(
    convert_dist_scale(1, actual_unit = "km", scale = "1 in = 10 feet")
  )
})
