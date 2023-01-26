test_that("page_to_viewport works", {
  expect_s3_class(
    page_to_viewport(get_page_size("letter")),
    "viewport"
  )
})
