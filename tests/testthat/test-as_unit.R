test_that("as_unit_type works", {
  inch <- as_unit(1, "in")

  expect_equal(as_unit_type(inch), "inches")

  expect_equal(as_unit_type(as_unit(10, inch)), "inches")

  expect_equal(as_unit_type(as_unit(inch, "cm")), "cm")

  expect_true(is_same_unit_type("pt", "points"))

  expect_error(as_unit_type(NA))

  expect_warning(convert_unit_type(1, to = "in"))
})

test_that("convert_unit_type works", {
  x <- 1

  expect_equal(convert_unit_type(x), x)

  expect_equal(names(convert_unit_type(c("dist" = x), "cm", "in")), "dist")

  expect_warning(
    convert_unit_type(
      as_unit(x, "in"),
      as_unit(x, "cm"),
      "in"
    )
  )
  expect_identical(
    as_unit(grid::unit(1, "in")),
    grid::unit(1, "in")
  )
  expect_identical(
    as_unit_type(margins(1, unit = "cm")),
    "cm"
  )
  expect_false(
    is_unit_type(NULL)
  )
  expect_true(
    is_unit_type(grid_units[c(1:4)])
  )
  expect_error(
    as_unit(1, "inchs")
  )
  expect_error(
    convert_unit_type(
      "x",
      from = "in",
      to = "cm"
    )
  )
})
