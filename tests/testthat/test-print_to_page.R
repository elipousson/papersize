test_that("print_to_page works", {
  skip_if_not_installed("ggplot2")
  p <- ggplot2::ggplot(ggplot2::mpg, ggplot2::aes(class)) +
    ggplot2::geom_bar()

  expect_s3_class(
    print_to_page(
      p,
      page = get_page_size("Tarot card")
    ),
    "gg"
  )
})
