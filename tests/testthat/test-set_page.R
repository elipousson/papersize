test_that("set_page_dims works", {
  expect_identical(
    set_page_dims(
      page = get_paper("ledger"),
      dims = list(
        width = c(unit(8.5, "in")),
        height = c(unit(11, "in"))
      )
    )[["width"]],
    8.5
  )
})
