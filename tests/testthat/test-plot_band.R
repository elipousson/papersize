test_that("as_thickness calculates thickness from n and pt", {
  thickness <- as_thickness(n = 54)

  expect_true(is_unit(thickness))
  expect_equal(as.numeric(thickness), 0.54)
  expect_equal(as_unit_type(thickness), "inches")
})

test_that("as_thickness uses x directly, ignoring n and pt", {
  thickness <- as_thickness(n = 999, pt = 999, x = 0.75, units = "in")

  expect_equal(as.numeric(thickness), 0.75)
  expect_equal(as_unit_type(thickness), "inches")
})

test_that("as_thickness respects units when x is supplied", {
  thickness <- as_thickness(x = 2, units = "cm")

  expect_equal(as.numeric(thickness), 2)
  expect_equal(as_unit_type(thickness), "cm")
})

test_that("plot_band errors if thickness, n, and x are all missing", {
  expect_error(
    plot_band(make_page_size(width = 2.5, height = 3.5, units = "in")),
    "thickness"
  )
})

test_that("plot_band errors on an invalid orientation", {
  expect_error(
    plot_band(
      make_page_size(width = 2.5, height = 3.5, units = "in"),
      orientation = "diagonal",
      n = 54
    ),
    "orientation"
  )
})

test_that("plot_band segments use pt-based ease when thickness is calculated", {
  paper <- make_page_size(width = 2.5, height = 3.5, units = "in")
  band <- plot_band(paper, n = 54)

  seg <- ggplot2::layer_data(band, 1)

  # main (2.5), side (thickness 0.54 + pt/1000 ease 0.01 = 0.55), main, side,
  # glue (overlap 0.5)
  expect_equal(seg[["xmin"]], c(0, 2.5, 3.05, 5.55, 6.10))
  expect_equal(seg[["xmax"]], c(2.5, 3.05, 5.55, 6.10, 6.60))
  expect_equal(seg[["fill"]], c("white", "white", "white", "white", "grey85"))
})

test_that("plot_band uses thickness exactly, with no ease, when supplied", {
  paper <- make_page_size(width = 2.5, height = 3.5, units = "in")
  band <- plot_band(paper, thickness = 0.54, overlap = 0.5)

  seg <- ggplot2::layer_data(band, 1)

  # side segments should be exactly 0.54 wide, not 0.55
  expect_equal(diff(c(seg[["xmin"]][[2]], seg[["xmax"]][[2]])), 0.54)
  expect_equal(max(seg[["xmax"]]), 6.58)
})

test_that("plot_band ignores n/pt/x when thickness is supplied", {
  paper <- make_page_size(width = 2.5, height = 3.5, units = "in")

  band_a <- plot_band(paper, thickness = 0.75, n = 999, pt = 999, x = 999)
  band_b <- plot_band(paper, thickness = 0.75)

  expect_equal(
    ggplot2::layer_data(band_a, 1)[["xmax"]],
    ggplot2::layer_data(band_b, 1)[["xmax"]]
  )
})

test_that("plot_band orientation = vertical uses height as main and width for band_width", {
  paper <- make_page_size(width = 2.5, height = 3.5, units = "in")
  band <- plot_band(paper, n = 54, orientation = "vertical")

  seg <- ggplot2::layer_data(band, 1)

  # main (3.5), side (0.55), main, side, glue (0.5)
  expect_equal(seg[["ymin"]], c(0, 3.5, 4.05, 7.55, 8.10))
  expect_equal(seg[["ymax"]], c(3.5, 4.05, 7.55, 8.10, 8.60))
  # band_width (cross dimension) defaults to 40% of width (2.5 * 0.4 = 1)
  expect_equal(unique(seg[["xmin"]]), 0)
  expect_equal(unique(seg[["xmax"]]), 1)
})

test_that("plot_band band_width defaults to 40% of the cross dimension", {
  paper <- make_page_size(width = 2.5, height = 3.5, units = "in")
  band <- plot_band(paper, n = 54)

  seg <- ggplot2::layer_data(band, 1)

  expect_equal(unique(seg[["ymax"]]), 3.5 * 0.4)
})

test_that("plot_band band_width can be set explicitly", {
  paper <- make_page_size(width = 2.5, height = 3.5, units = "in")
  band <- plot_band(paper, n = 54, band_width = 1)

  seg <- ggplot2::layer_data(band, 1)

  expect_equal(unique(seg[["ymax"]]), 1)
})

test_that("plot_band returns a ggplot object", {
  band <- plot_band(
    make_page_size(width = 2.5, height = 3.5, units = "in"),
    n = 54
  )

  expect_s3_class(band, "gg")
})

test_that("plot_band_dims matches plot_band's own segment extents", {
  paper <- make_page_size(width = 2.5, height = 3.5, units = "in")
  band <- plot_band(paper, n = 54)
  dims <- plot_band_dims(paper, n = 54)

  seg <- ggplot2::layer_data(band, 1)

  expect_equal(dims[["width"]], max(seg[["xmax"]]))
  expect_equal(dims[["height"]], unique(seg[["ymax"]]))
  expect_equal(dims[["units"]], "inches")
})

test_that("plot_band_dims horizontal vs vertical swap width/height", {
  paper <- make_page_size(width = 2.5, height = 3.5, units = "in")

  dims_h <- plot_band_dims(paper, n = 54)
  dims_v <- plot_band_dims(paper, n = 54, orientation = "vertical")

  expect_equal(dims_h[["width"]], 6.6)
  expect_equal(dims_h[["height"]], 1.4)

  expect_equal(dims_v[["width"]], 1)
  expect_equal(dims_v[["height"]], 8.6)
})

test_that("plot_band_dims uses thickness exactly, with no ease, when supplied", {
  paper <- make_page_size(width = 2.5, height = 3.5, units = "in")

  dims <- plot_band_dims(paper, thickness = 0.54, overlap = 0.5)

  expect_equal(dims[["width"]], 6.58)
  expect_equal(dims[["height"]], 1.4)
})

test_that("plot_band_dims returns a data.frame with width, height, and units", {
  dims <- plot_band_dims(
    make_page_size(width = 2.5, height = 3.5, units = "in"),
    n = 54
  )

  expect_s3_class(dims, "data.frame")
  expect_true(all(c("width", "height", "units") %in% names(dims)))
})

test_that("plot_band_page returns a list of patchwork objects", {
  layout <- plot_band_page(get_card("Poker"), n = 54)

  expect_type(layout, "list")
  expect_length(layout, 1)
  expect_s3_class(layout[[1]], "patchwork")
})

test_that("plot_band_page paginates when n_bands exceeds one page's capacity", {
  layout <- plot_band_page(get_card("Poker"), n = 54, n_bands = 20)

  expect_length(layout, 3)
  expect_s3_class(layout[[1]], "patchwork")
  expect_s3_class(layout[[3]], "patchwork")
})

test_that("plot_band_page paginates using explicit ncol/nrow", {
  layout <- plot_band_page(
    get_card("Poker"),
    n = 54,
    n_bands = 3,
    ncol = 1,
    nrow = 1
  )

  expect_length(layout, 3)
})

test_that("plot_band_page defaults to centering the band on the page", {
  get_patches <- getFromNamespace("get_patches", "patchwork")

  layout <- plot_band_page(get_card("Poker"), n = 54)
  margin <- as.numeric(get_patches(layout[[1]])$annotation$theme$plot.margin)

  # t == b and l == r for a centered single band
  expect_equal(margin[[1]], margin[[3]])
  expect_equal(margin[[2]], margin[[4]])
  expect_true(all(margin > 0))
})

test_that("plot_band_page position overrides the default centering", {
  get_patches <- getFromNamespace("get_patches", "patchwork")

  layout <- plot_band_page(
    get_card("Poker"),
    n = 54,
    position = "top-left"
  )
  margin <- as.numeric(get_patches(layout[[1]])$annotation$theme$plot.margin)

  # top-left: no margin on the top or left, all leftover on bottom/right
  expect_equal(margin[[1]], 0)
  expect_equal(margin[[4]], 0)
  expect_true(margin[[2]] > 0)
  expect_true(margin[[3]] > 0)
})
