# Helper to add a border to each card plot in a list

`add_card_border()` is a helper function used internally by
[`plot_cards()`](https://elipousson.github.io/papersize/reference/plot_cards.md)
and can be used to further modify output plots.

## Usage

``` r
add_card_border(
  plots,
  card = NULL,
  inset = grid::unit(c(5, 5), "mm"),
  fill = NA,
  color = "white",
  linetype = "dashed",
  linewidth = 1
)
```

## Arguments

- inset:

  Numeric distance to inset an input page.

- fill, color, linetype, linewidth:

  Fixed aesthetics passsed to
  [`ggplot2::geom_tile()`](https://ggplot2.tidyverse.org/reference/geom_tile.html)
