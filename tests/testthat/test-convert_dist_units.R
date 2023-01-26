test_that("convert_dist_units works", {
  expect_identical(
    convert_dist_units(1, "km"),
    units::set_units(1000, "m")
  )
  expect_identical(
    convert_dist_units(1, from = "mile", to = "km"),
    units::set_units(1.609344, "km")
  )
  expect_identical(
    convert_dist_units(1, from = "mile", to = "km", digits = 1),
    units::set_units(1.6, "km")
  )
  expect_identical(
    convert_dist_units(1, from = NULL, to = NULL),
    1
  )
  expect_identical(
    as_dist_units(units::set_units(1, "km"), "m"),
    units::set_units(1000, "m")
  )
  expect_identical(
    convert_dist_units(3, from = "ft", to = "yard"),
    units::set_units(1, "yard")
  )
  expect_identical(
    as_dist_units(units::set_units(1, "mi"), "km"),
    set_dist_units(1.609344, "km")
  )
  expect_error(
    convert_dist_units(TRUE)
  )
  expect_error(
    convert_dist_units(as_unit(1, "npc"))
  )
  expect_error(
    try_as_units("international in")
  )
  expect_warning(
    convert_dist_units(units::set_units(1, "mi"), from = "km", to = "m")
  )
})
