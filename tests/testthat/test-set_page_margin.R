test_that("set_page_margin works", {
  expect_identical(
    set_page_margin(
      page = "letter",
      margins = "standard"
    )[[get_body_col(suffix = "_width")]],
    6.5
  )
  expect_identical(
    set_page_margin(
      page = "a4",
      margins = "standard"
    )[[get_body_col(suffix = "_width")]],
    159.2
  )
})


test_that("set_page_margin errors", {
  expect_error(
    set_page_margin(
      page = "letter"
    )
  )
  expect_error(
    set_page_margin(
      page = "letter",
      margins = 11
    )
  )
  expect_error(
    set_page_margin(
      page = "letter",
      margins = list(t = 10, l = 1, r = 1, b = 10)
    )
  )
})
