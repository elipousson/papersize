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
    "Tarot card"
  )
  expect_equal(
    get_page_size("ledger", units = "cm")$units,
    "cm"
  )
  expect_warning(
    get_page_size("Large Post Quarto", orientation = "landscape")
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
    get_page_dims(c(11, 17), units = "in"),
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
    convert_page_units(get_paper("a4"), "mm"),
    get_paper("a4")
  )
  # FIXME: This should probably error instead of just pass the character string
  # as paper
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
      h = 11
    )
  expect_error(
    check_page(test_df$name)
  )
  expect_error(
    check_page(
      test_df
    )
  )
  expect_error(
    check_page(
      get_page_size(width = 8),
      n = 1
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
      get_page_dims(
        test_df,
        cols = c("w", "h"),
        units = "in"
      ),
      nm = c("w", "h")
    ),
    c("w" = 6.5, "h" = 9)
  )
})

test_that("make_page_size works", {
  expect_identical(
    make_page_size(48, 24, "in", name = "Tabletop map")[["name"]],
    "Tabletop map"
  )
  expect_identical(
    make_page_size(8.5, 11, "in", name = "letter", class = "list"),
    list(
      "name" = "letter",
      "width" = 8.5,
      "height" = 11,
      "units" = "inches",
      "orientation" = "portrait",
      "asp" = 8.5 / 11
    )
  )
})
