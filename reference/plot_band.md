# Plot a folded paper band to bundle a stack of cards or paper

`plot_band()` creates a ggplot2 plot of a flat, foldable paper band
sized to wrap around a stack of cards or paper and hold it together,
similar to a currency or belly band. `paper` sets the width and height
of the item being bundled, `orientation` sets which of those two
dimensions the band wraps around, and the folded sides of the band (that
wrap around the two edges of the stack) are sized to match the stack
thickness calculated by
[`as_thickness()`](https://elipousson.github.io/papersize/reference/as_thickness.md).
The band extends past the far edge of the stack with an additional
segment so it can be glued or taped closed after wrapping.

## Usage

``` r
plot_band(
  paper,
  orientation = c("horizontal", "vertical"),
  n = NULL,
  pt = 10,
  x = NULL,
  thickness = NULL,
  overlap = 0.5,
  band_width = NULL,
  fill = "white",
  color = "black",
  linewidth = 0.5,
  glue_fill = "grey85",
  fold_linetype = "dashed",
  fold_linewidth = linewidth/2
)
```

## Arguments

- paper:

  Paper, page, or card name, or a data.frame with width and height
  columns (as returned by
  [`get_page_size()`](https://elipousson.github.io/papersize/reference/get_page_size.md)
  or
  [`make_page_size()`](https://elipousson.github.io/papersize/reference/make_page_size.md)),
  passed to
  [`as_page()`](https://elipousson.github.io/papersize/reference/as_page.md).
  Sets the width and height of the stack of cards or paper the band
  wraps around.

- orientation:

  Band orientation, either `"horizontal"` or `"vertical"`.
  `"horizontal"` wraps the band left-to-right, crossing the middle of
  the stack, so the band's panels use the `width` of `paper` and the
  band's own height is set by `band_width`. `"vertical"` wraps the band
  top-to-bottom instead, so the panels use `height` and the band's own
  width is set by `band_width`. Default: `"horizontal"`

- n, pt, x:

  Passed to
  [`as_thickness()`](https://elipousson.github.io/papersize/reference/as_thickness.md)
  to calculate the thickness of the stack. Ignored if `thickness` is
  supplied. `pt` is also used to add a small ease allowance (the caliper
  of a single sheet or card) to the folded side segments of the band,
  since a flat band can't wrap tightly around a stack at its exact
  thickness. Ignored if `thickness` is supplied.

- thickness:

  Optional. A `unit` object or number (in the units of `paper`) with the
  thickness of the stack, overriding `n`, `pt`, and `x`. If supplied, no
  ease allowance is added and `thickness` sets the folded side segments
  of the band exactly. Default: `NULL`

- overlap:

  Distance the band extends past the far edge of the stack, forming a
  segment that can be glued or taped closed. A `unit` object or a number
  (in the units of `paper`). Default: `0.5` (inches, if `paper` has no
  units)

- band_width:

  Height of the band if `orientation` is `"horizontal"`, or width of the
  band if `orientation` is `"vertical"` — the dimension of the band
  perpendicular to the direction it wraps around the stack. A `unit`
  object or a number (in the units of `paper`). Default: `NULL`, which
  uses 40% of the `height` of `paper` (if `orientation` is
  `"horizontal"`) or 40% of the `width` of `paper` (if `orientation` is
  `"vertical"`).

- fill, color, linewidth:

  Fill, outline color, and outline linewidth for the band, Default:
  `"white"`, `"black"`, and `0.5`

- glue_fill:

  Fill color used to distinguish the overlapping segment of the band set
  by `overlap`, Default: `"grey85"`

- fold_linetype, fold_linewidth:

  Linetype and linewidth for the lines marking where the band should be
  folded, Default: `"dashed"` and `linewidth / 2`

## Value

A ggplot2 object.

## See also

[`as_thickness()`](https://elipousson.github.io/papersize/reference/as_thickness.md)

[`plot_band_dims()`](https://elipousson.github.io/papersize/reference/plot_band_dims.md)

## Examples

``` r
if (FALSE) { # \dontrun{
if (interactive() && is_installed("ggplot2")) {
  plot_band(get_card("Poker"), n = 54)

  plot_band(get_card("Tarot"), n = 78, orientation = "vertical")
}
} # }
```
