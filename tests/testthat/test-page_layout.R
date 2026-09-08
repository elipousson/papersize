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

test_that("page_layout errors if marks is TRUE without margin or known dims", {
  expect_error(
    page_layout(
      plots = plot_cards("Poker", 1),
      page = "letter",
      ncol = 1,
      nrow = 1,
      marks = TRUE
    ),
    "margin"
  )
})

test_that("page_layout allows marks without margin when dims are known", {
  # dims auto-detected from the first plot, so a margin can be computed from
  # `position` instead of requiring one explicitly
  layout <- page_layout(
    plots = plot_cards("Poker", 1),
    page = "letter",
    marks = TRUE
  )

  expect_type(layout, "list")
  expect_s3_class(layout[[1]], "patchwork")
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

  expect_equal(
    as.numeric(get_patches(patch_margin)$annotation$theme$plot.margin),
    c(1, 1, 1, 1)
  )
})

test_that("fit_page_grid caps ncol/nrow to n, prioritizing columns", {
  expect_equal(fit_page_grid(c(4, 2), 1), c(1, 1))
  expect_equal(fit_page_grid(c(4, 2), 8), c(4, 2))
  expect_equal(fit_page_grid(c(1, 7), 6), c(1, 6))
  expect_equal(fit_page_grid(c(4, 2), 5), c(4, 2))
  expect_equal(fit_page_grid(c(3, 3), 8), c(3, 3))
})

test_that("fit_page_grid never returns less than 1x1", {
  expect_equal(fit_page_grid(c(4, 2), 0), c(1, 1))
})

test_that("position_margin assigns leftover space by anchor", {
  expect_equal(as.numeric(position_margin("top-left", 2, 4)), c(0, 2, 4, 0))
  expect_equal(as.numeric(position_margin("top", 2, 4)), c(0, 1, 4, 1))
  expect_equal(as.numeric(position_margin("top-right", 2, 4)), c(0, 0, 4, 2))
  expect_equal(as.numeric(position_margin("left", 2, 4)), c(2, 2, 2, 0))
  expect_equal(as.numeric(position_margin("center", 2, 4)), c(2, 1, 2, 1))
  expect_equal(as.numeric(position_margin("right", 2, 4)), c(2, 0, 2, 2))
  expect_equal(as.numeric(position_margin("bottom-left", 2, 4)), c(4, 2, 0, 0))
  expect_equal(as.numeric(position_margin("bottom", 2, 4)), c(4, 1, 0, 1))
  expect_equal(
    as.numeric(position_margin("bottom-right", 2, 4)),
    c(4, 0, 0, 2)
  )
})

test_that("position_margin clamps negative leftover space to 0", {
  expect_equal(as.numeric(position_margin("center", -1, -1)), c(0, 0, 0, 0))
})

test_that("page_layout errors on an invalid position", {
  expect_error(
    page_layout(
      plots = plot_cards("Poker", 1),
      page = "letter",
      position = "nowhere"
    ),
    "position"
  )
})

test_that("page_layout position defaults to top-left (no margin)", {
  layout <- page_layout(plots = plot_cards("Poker", 1), page = "letter")
  margin <- as.numeric(get_patches(layout[[1]])$annotation$theme$plot.margin)

  expect_equal(margin[[1]], 0)
  expect_equal(margin[[4]], 0)
})

test_that("page_layout position = center splits leftover space evenly", {
  layout <- page_layout(
    plots = plot_cards("Poker", 1),
    page = "letter",
    position = "center"
  )
  margin <- as.numeric(get_patches(layout[[1]])$annotation$theme$plot.margin)

  expect_equal(margin[[1]], margin[[3]])
  expect_equal(margin[[2]], margin[[4]])
  expect_true(all(margin > 0))
})

test_that("page_layout position = bottom-right pushes content to that corner", {
  layout <- page_layout(
    plots = plot_cards("Poker", 1),
    page = "letter",
    position = "bottom-right"
  )
  margin <- as.numeric(get_patches(layout[[1]])$annotation$theme$plot.margin)

  expect_equal(margin[[2]], 0)
  expect_equal(margin[[3]], 0)
  expect_true(margin[[1]] > 0)
  expect_true(margin[[4]] > 0)
})

test_that("page_layout position is ignored when an explicit margin is supplied", {
  explicit_margin <- margins(t = 1, r = 1, b = 1, l = 1, unit = "in")
  layout <- page_layout(
    plots = plot_cards("Poker", 1),
    page = "letter",
    position = "center",
    margin = explicit_margin
  )
  margin <- as.numeric(get_patches(layout[[1]])$annotation$theme$plot.margin)

  expect_equal(margin, c(1, 1, 1, 1))
})

test_that("page_layout shrinks the grid to fit fewer plots than the page holds", {
  # 1 Poker card on letter (landscape) has room for a 4x2 grid, but only 1
  # plot is supplied — the render should use a 1x1 grid, not stretch or
  # shrink the card to fill an unused 4x2 grid
  layout <- page_layout(plots = plot_cards("Poker", 1), page = "letter")

  built <- ggplot2::layer_data(layout[[1]][[1]], 1)
  expect_equal(built[["xmax"]] - built[["xmin"]], 2.5)
  expect_equal(built[["ymax"]] - built[["ymin"]], 3.5)
})

test_that("page_layout centers each pagination group independently", {
  # 9 Poker cards auto-fit an 4x2 = 8 capacity grid on letter (landscape),
  # paginating into a full group of 8 and a 1-card remainder — the
  # remainder page should still be centered on its own, not stretched to
  # the full 4x2 grid
  layout <- page_layout(
    plots = plot_cards("Poker", 9),
    page = "letter",
    position = "center"
  )

  expect_length(layout, 2)

  margin1 <- as.numeric(get_patches(layout[[1]])$annotation$theme$plot.margin)
  expect_equal(margin1[[1]], margin1[[3]])
  expect_equal(margin1[[2]], margin1[[4]])

  margin2 <- as.numeric(get_patches(layout[[2]])$annotation$theme$plot.margin)
  expect_equal(margin2[[1]], margin2[[3]])
  expect_equal(margin2[[2]], margin2[[4]])
})

test_that("set_page_grid reconciles dims units against page units", {
  # 5cm =~ 1.9685in; page (letter, portrait 8.5x11in, no orientation
  # override) fits floor(8.5/1.9685) = 4 columns, floor(11/1.9685) = 5 rows
  dims_cm <- make_page_size(width = 5, height = 5, units = "cm")

  grid <- set_page_grid(
    plots = plot_cards("Poker", 1),
    page = "letter",
    dims = dims_cm
  )

  expect_equal(as.numeric(grid), c(4, 5))
})

test_that("page_layout works with dims in different units than page", {
  dims_cm <- make_page_size(width = 5, height = 5, units = "cm")

  layout <- page_layout(
    plots = plot_cards("Poker", 1),
    page = "letter",
    dims = dims_cm
  )

  expect_type(layout, "list")
  expect_s3_class(layout[[1]], "patchwork")
})

test_that("page_layout errors clearly when the grid can't fit on the page", {
  expect_error(
    suppressWarnings(
      page_layout(
        plots = plot_cards("Poker", 1),
        page = make_page_size(width = 1, height = 1, units = "in")
      )
    ),
    "units"
  )
})

test_that("page_layout warns when page units aren't inches and dims are auto-detected", {
  expect_warning(
    page_layout(
      plots = plot_cards("Poker", 1),
      page = make_page_size(width = 20, height = 30, units = "cm")
    ),
    "inches"
  )
})

test_that("page_layout doesn't warn about units when page is already in inches", {
  expect_no_warning(
    page_layout(plots = plot_cards("Poker", 1), page = "letter")
  )
})

test_that("capacity_grid ignores gutter when it's 0 (matches plain division)", {
  expect_equal(capacity_grid(c(8, 8), c(2, 2)), c(4, 4))
})

test_that("capacity_grid accounts for gutter between cells", {
  # 3 cells of 2in + 2 gutters of 0.5in = 7in <= 8in; a 4th would need 9.5in
  expect_equal(
    capacity_grid(c(8, 8), c(2, 2), gutter = c(row = 0.5, col = 0.5)),
    c(3, 3)
  )
})

test_that("set_page_grid's auto-detected capacity accounts for gutter", {
  grid_no_gutter <- set_page_grid(
    page = make_page_size(width = 8, height = 8, units = "in"),
    dims = c(2, 2)
  )
  grid_gutter <- set_page_grid(
    page = make_page_size(width = 8, height = 8, units = "in"),
    dims = c(2, 2),
    gutter = c(row = 0.5, col = 0.5)
  )

  expect_equal(as.numeric(grid_no_gutter), c(4, 4))
  expect_equal(as.numeric(grid_gutter), c(3, 3))
})

test_that("page_layout with gutter no longer overflows the page (regression)", {
  # previously: capacity ignored gutter, fit_page_grid picked ncol = 4 (its
  # true footprint with gutter, 4*2 + 3*0.5 = 9.5in, exceeds the 8in page),
  # and the resulting negative leftover was silently clamped to a 0 margin,
  # so the grid rendered wider than the page and got clipped by ggsave()
  plots <- suppressMessages(plot_cards("Poker", 4))
  layout <- suppressWarnings(
    page_layout(
      plots = plots,
      page = make_page_size(width = 8, height = 8, units = "in"),
      dims = c(2, 2),
      gutter = 0.5,
      position = "center"
    )
  )

  margin <- as.numeric(get_patches(layout[[1]])$annotation$theme$plot.margin)

  # a real (non-zero, symmetric) margin means the grid actually fit
  expect_true(all(margin > 0))
  expect_equal(margin[[1]], margin[[3]])
  expect_equal(margin[[2]], margin[[4]])
})
