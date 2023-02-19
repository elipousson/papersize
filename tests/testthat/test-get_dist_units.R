test_that("is_dist_units works", {
  is_dist_units(units::as_units(1, "m"))
  is_dist_units(units::as_units(1, "km^2"))
})

test_that("get_dist_units works", {
  expect_identical(get_dist_units(NULL), NULL)
  expect_identical(get_dist_units(c("yards", "miles")), c("yards", "miles"))
  expect_identical(get_dist_units(as_dist_units(1, "mile")), "mile")
  expect_identical(get_dist_units(as_unit(1, "cm")), "cm")

  expect_warning(get_dist_units(1))
  expect_error(get_dist_units(TRUE))

  skip_if_not_installed("sf")
  nc <- sf::read_sf(system.file("shape/nc.shp", package = "sf"))
  expect_identical(get_dist_units(nc), "degree")
  expect_identical(get_dist_units(sf::st_bbox(nc)), "degree")
})

test_that("as_dist_units works", {
  expect_error(as_dist_units(1, c("mi", "m")))

  skip_if_not_installed("sf")
  nc <- sf::read_sf(system.file("shape/nc.shp", package = "sf"))
  expect_identical(as_dist_units(1, units = nc), units::set_units(1, "degree"))
})

test_that("is_same_units works", {
  expect_false(
    is_same_units("mi")
  )
  expect_true(
    is_same_units("mi", "mile")
  )
  expect_true(
    is_same_units("mi", as_dist_units(1, "mile"))
  )
  expect_true(
    is_same_units("in", "international inch")
  )
})
