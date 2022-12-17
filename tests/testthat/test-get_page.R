test_that("get_page works", {
  expect_equal(
    get_paper("letter")$width,
    8.5
  )
  expect_equal(
    get_paper("letter", orientation = "landscape")$width,
    11
  )
  expect_identical(
    get_page_size(orientation = "square", reorient = FALSE)$orientation,
    c("square", "square", "square", "square")
  )
  expect_equal(
    get_card("Tarot")$name,
    list("Tarot card")
  )
  expect_equal(
    get_page_size("ledger", units = "cm")$units,
    "cm"
  )
  expect_equal(
    get_page_dims(get_page_size("ledger")),
    c("width" = 11, "height" = 17)
  )
})
