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
})

test_that("get_page_dims works", {
  expect_equal(
    get_page_dims("ledger"),
    c("width" = 11, "height" = 17)
  )

  expect_equal(
    get_page_dims(get_page_size("ledger")),
    c("width" = 11, "height" = 17)
  )
  expect_equal(
    get_page_dims(width = 11, height = 17),
    c("width" = 11, "height" = 17)
  )
  expect_equal(
    get_page_dims(c(11, 17)),
    c("width" = 11, "height" = 17)
  )
  expect_warning(
    get_page_dims(width = 11, height = 17, cols = c("X", "Y", "Z"))
  )
  expect_error(
    get_page_dims(NA)
  )
})

test_that("get_page errors", {
  expect_error(
    get_page_size("lettr", orientation = "landscape")
  )

  expect_error(
    get_page_size("letter", type = "papr")
  )
})

test_that("convert_page_units works", {
  expect_warning(
    check_dims_cols(
      cols = c("w", "h", "d"),
      width = 8.5,
      height = 11
    )
  )
  expect_identical(
    convert_page_units("letter"),
    "letter"
  )
})

test_that("helpers work", {
  expect_warning(
    check_dims_cols(
      cols = c("w", "h", "d"),
      width = 8.5,
      height = 11
    )
  )
  expect_identical(
    check_dims_cols(
      cols = c("w", "h", "d"),
      default = c("w", "h", "d")
    ),
    c("w", "h", "d")
  )

  expect_identical(
    set_page_orientation(get_paper("letter")),
    get_paper("letter")
  )

  expect_identical(
    set_page_orientation(get_paper("letter"), "portrait"),
    get_paper("letter")
  )

  test_df <-
    data.frame(
      name = "letter",
      w = 8.5,
      h = 11 # ,
      # units = "in"
    )

  expect_error(
    check_page(test_df$name)
  )
  expect_error(
    check_page(
      test_df
    )
  )
  expect_identical(
    set_page_dims(
      test_df,
      dims = c(
        "w" = test_df$w,
        "h" = test_df$h
      ),
      cols = c("w", "h")
    ),
    test_df
  )

  expect_identical(
    get_inset_dims(
      get_page_dims(test_df, cols = c("w", "h")),
      nm = c("w", "h")
    ),
    c("w" = 6.5, "h" = 9)
  )
})
