test_that("page_to_layout works", {
  expect_s3_class(
    page_to_layout(get_page_size("letter")),
    "layout"
  )
})
