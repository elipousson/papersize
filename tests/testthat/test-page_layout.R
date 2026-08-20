test_that("page_layout works", {
  expect_s3_class(
    page_layout(
      plots = plot_cards("Poker", 6),
      page = "letter"
    )[[1]],
    "gg"
  )
})

test_that("page_layout returns a plot_layout when plots is NULL", {
  expect_s3_class(
    page_layout(page = "letter", ncol = 2, nrow = 3),
    "plot_layout"
  )
})

test_that("page_layout paginates when plots exceed the grid capacity", {
  layouts <- page_layout(
    plots = plot_cards("Poker", 6),
    page = "letter",
    ncol = 2,
    nrow = 2
  )

  expect_type(layouts, "list")
  expect_length(layouts, 2)
  expect_s3_class(layouts[[1]], "patchwork")
  expect_s3_class(layouts[[2]], "patchwork")
})

test_that("page_layout with paginate = FALSE returns a single patchwork", {
  layout <- page_layout(
    plots = plot_cards("Poker", 6),
    page = "letter",
    ncol = 2,
    nrow = 2,
    paginate = FALSE
  )

  expect_s3_class(layout, "patchwork")
})

test_that("page_layout errors if marks is TRUE without margin", {
  expect_error(
    page_layout(
      plots = plot_cards("Poker", 1),
      page = "letter",
      marks = TRUE
    ),
    "margin"
  )
})

test_that("page_layout supports margin and marks together", {
  layout <- page_layout(
    plots = plot_cards("Poker", 4),
    page = make_page_size(width = 8.5, height = 8.5, units = "in"),
    orientation = "square",
    ncol = 2,
    nrow = 2,
    margin = margins(t = 0.75, r = 0.5, b = 0.75, l = 0.5, unit = "in"),
    marks = TRUE
  )

  expect_type(layout, "list")
  expect_s3_class(layout[[1]], "patchwork")
})

test_that("page_layout supports gutter spacing between plots", {
  layout <- page_layout(
    plots = plot_cards("Poker", 4),
    page = make_page_size(width = 7.7, height = 7.1, units = "in"),
    ncol = 2,
    nrow = 2,
    gutter = 0.1,
    paginate = FALSE
  )

  expect_s3_class(layout, "patchwork")
})

test_that("set_page_grid uses ncol/nrow directly and warns if dims is also supplied", {
  expect_identical(
    set_page_grid(page = "letter", ncol = 2, nrow = 3),
    c(2, 3)
  )

  expect_message(
    set_page_grid(page = "letter", ncol = 2, nrow = 3, dims = c(1, 1)),
    "dims.*ignored"
  )
})

test_that("set_page_grid errors on non-whole ncol/nrow", {
  expect_error(
    set_page_grid(page = "letter", ncol = 2.5, nrow = 2)
  )
})

test_that("set_page_grid infers grid dimensions from plots", {
  expect_message(
    grid <- set_page_grid(plots = plot_cards("Poker", 1), page = "letter"),
    "Using.*dims"
  )
  expect_true(all(grid > 0))
})

test_that("get_gutter parses gutter specifications", {
  expect_identical(get_gutter(NULL), c(row = 0, col = 0))
  expect_identical(get_gutter(0.1), c(row = 0.1, col = 0.1))
  expect_identical(get_gutter(c(0.1, 0.2)), c(row = 0.1, col = 0.2))
  expect_identical(
    get_gutter(c(row = 0.1, col = 0.2)),
    c(row = 0.1, col = 0.2)
  )
  expect_identical(
    get_gutter(list(row = 0.1, col = 0.2)),
    c(row = 0.1, col = 0.2)
  )
})

test_that("get_gutter errors on invalid length", {
  expect_error(get_gutter(c(1, 2, 3)))
})

test_that("add_gutter_margins assigns interior-facing margins by grid position", {
  plots <- suppressMessages(plot_cards("Poker", 4))

  out <- add_gutter_margins(
    plots,
    ncol = 2,
    nrow = 2,
    gutter = c(row = 0.2, col = 0.4),
    unit = "in"
  )

  # byrow = FALSE fills column-major: plot 1 = (row 1, col 1)
  expect_equal(as.numeric(out[[1]]$theme$plot.margin), c(0, 0.2, 0.1, 0))
  # plot 2 = (row 2, col 1)
  expect_equal(as.numeric(out[[2]]$theme$plot.margin), c(0.1, 0.2, 0, 0))
  # plot 3 = (row 1, col 2)
  expect_equal(as.numeric(out[[3]]$theme$plot.margin), c(0, 0, 0.1, 0.2))
  # plot 4 = (row 2, col 2)
  expect_equal(as.numeric(out[[4]]$theme$plot.margin), c(0.1, 0, 0, 0.2))
})

test_that("add_gutter_margins is a no-op when gutter is 0", {
  plots <- suppressMessages(plot_cards("Poker", 2))

  expect_identical(
    add_gutter_margins(plots, ncol = 2, nrow = 1),
    plots
  )
})

test_that("add_page_margin returns patch unchanged when margin is NULL", {
  patch <- patchwork::wrap_plots(suppressMessages(plot_cards("Poker", 1)))

  expect_identical(add_page_margin(patch, NULL), patch)
})

test_that("add_page_margin applies a plot.margin to the composed page", {
  patch <- patchwork::wrap_plots(suppressMessages(plot_cards("Poker", 1)))

  patch_margin <- add_page_margin(
    patch,
    margins(1, unit = "in"),
    unit = "in"
  )

  expect_s3_class(patch_margin, "patchwork")

  get_patches <- getFromNamespace("get_patches", "patchwork")
  expect_equal(
    as.numeric(get_patches(patch_margin)$annotation$theme$plot.margin),
    c(1, 1, 1, 1)
  )
})
