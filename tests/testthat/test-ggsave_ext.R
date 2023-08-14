test_that("ggsave_ext works", {
  skip_if_not_installed("ggplot2")
  p <- ggplot2::ggplot(ggplot2::mpg, ggplot2::aes(class)) +
    ggplot2::geom_bar()

  withr::with_tempdir({
    ggsave_ext(
      plot = p,
      filename = "p_asp.png",
      width = 5,
      asp = 1,
      units = "in"
    )
    expect_true(
      file.exists("p_asp.png")
    )
    ggsave_ext(
      plot = p,
      filename = "p_paper.png",
      paper = "letter"
    )
    expect_true(
      file.exists("p_paper.png")
    )
    ggsave_social(
      plot = p,
      name = "P Social"
    )
    expect_true(
      file.exists("p_social.jpeg")
    )
  })
})

test_that("ggsave_ext w/ magick works", {
  withr::with_tempdir({
    skip_if_not_installed("magick")
    ggsave_ext(
      plot = magick::logo,
      filename = "logo_image-magick.jpeg",
      width = 500,
      height = 500,
      unit = "px"
    )
    expect_true(
      file.exists("logo_image-magick.jpeg")
    )
  })
})

test_that("map_ggsave_ext w/ gridExtra works", {
  skip_if_not_installed("ggplot2")
  p <- ggplot2::ggplot(ggplot2::mpg, ggplot2::aes(class)) +
    ggplot2::geom_bar()

  withr::with_tempdir({
    skip_if_not_installed("gridExtra")
    map_ggsave_ext(
      plot = list(p, p),
      filename = "p_paper.pdf",
      paper = "letter"
    )
    expect_true(
      file.exists("p_paper.pdf")
    )
  })
})

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

test_that("map_ggsave_ext warns and errors", {
  skip_if_not_installed("ggplot2")
  p <- ggplot2::ggplot(ggplot2::mpg, ggplot2::aes(class)) +
    ggplot2::geom_bar()

  withr::with_tempdir({
    expect_error(
      map_ggsave_ext(
        plot = p,
        filename = "expect-error.png",
        width = 6,
        height = 4
      )
    )
    expect_message(
      map_ggsave_ext(
        plot = list(p),
        filename = "expect-warning.png",
        width = 6,
        height = 4,
        single_file = FALSE,
        onefile = TRUE
      )
    )
  })
})
