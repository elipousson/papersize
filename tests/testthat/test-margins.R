test_that("margins and is_margin works", {
  skip_if_not_installed("ggplot2")
  standard_margin <- ggplot2::margin(
    t = 1, r = 1, b = 1, l = 1, unit = "in"
  )
  expect_identical(
    margins(1, unit = "in"),
    standard_margin
  )
  expect_identical(
    margins(c(2, 2), unit = "in"),
    standard_margin
  )
  expect_identical(
    margins(list(1, 1, 1.5, 1), unit = "cm"),
    ggplot2::margin(
      t = 1, r = 1, b = 1.5, l = 1, unit = "cm"
    )
  )
  expect_identical(
    get_margin("standard", unit = "in"),
    standard_margin
  )
  expect_true(
    is_margin(standard_margin)
  )
})
