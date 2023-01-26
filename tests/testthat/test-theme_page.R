test_that("theme_page works", {
  skip_if_not_installed("ggplot2")
  expect_s3_class(
    theme_page("letter"),
    c("theme", "gg")
  )
})
