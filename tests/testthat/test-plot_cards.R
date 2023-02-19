test_that("plot_cards works", {
  expect_s3_class(
    plot_cards("Tarot", n = 2, number = TRUE)[[2]],
    "gg"
  )
  expect_s3_class(
    plot_cards("Poker", n = 1, number = TRUE, text = "♡")[[1]],
    "gg"
  )
})
