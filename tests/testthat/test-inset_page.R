test_that("multiplication works", {
  pg <- get_page_size("letter")
  inset_pg <- inset_page(page)
  expect_identical(
    c(inset_pg[["width"]], inset_pg[["height"]]),
    c(8.3, 10.8)
  )
  inset_pg <- inset_page(page, pct_inset = .1)
  expect_identical(
    c(inset_pg[["width"]], inset_pg[["height"]]),
    c(7.65, 9.9)
  )
  inset_pg <- inset_page(page, inset = as_unit(1, "in"))
  expect_identical(
    c(inset_pg[["width"]], inset_pg[["height"]]),
    c(6.5, 9)
  )
})
