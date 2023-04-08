test_that("page_layout works", {
  expect_s3_class(
    page_layout(
      plots = plot_cards("Poker", 6),
      page = "letter"
    )[[1]],
    "gg"
  )
})
