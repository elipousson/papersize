test_that("set_ggsave_dims helper works", {
  expect_identical(
    set_ggsave_dims(
      width = 8.5,
      height = 11
    ),
    list("width" = 8.5, "height" = 11, "units" = "in")
  )
  expect_identical(
    set_ggsave_dims(
      paper = "letter"
    ),
    list("width" = 8.5, "height" = 11, "units" = "in")
  )
  expect_identical(
    set_ggsave_dims(
      width = 8.5,
      asp = 1
    ),
    list("width" = 8.5, "height" = 8.5, "units" = "in")
  )
  expect_identical(
    set_ggsave_dims(
      width = 210,
      height = 297,
      units = "millimeters"
    ),
    list("width" = 210, "height" = 297, "units" = "mm")
  )
})
