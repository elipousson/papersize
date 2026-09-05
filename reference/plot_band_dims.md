# Calculate the printed dimensions of a folded paper band

`plot_band_dims()` calculates the overall printed width and height of
the band
[`plot_band()`](https://elipousson.github.io/papersize/reference/plot_band.md)
draws for a given stack of cards or paper, including the fold-ease
[`plot_band()`](https://elipousson.github.io/papersize/reference/plot_band.md)
adds to the stack thickness (see
[`plot_band()`](https://elipousson.github.io/papersize/reference/plot_band.md)'s
`pt` parameter) and the `overlap` segment. Use it to get the band's
actual size — e.g. to center
[`plot_band()`](https://elipousson.github.io/papersize/reference/plot_band.md)'s
plot on a larger page with
[`print_to_page()`](https://elipousson.github.io/papersize/reference/print_to_page.md)
— without re-deriving the layout by hand.

## Usage

``` r
plot_band_dims(
  paper,
  orientation = c("horizontal", "vertical"),
  n = NULL,
  pt = 10,
  x = NULL,
  thickness = NULL,
  overlap = 0.5,
  band_width = NULL
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

## Value

A data.frame with the band's overall `width`, `height`, and `units`, as
returned by
[`make_page_size()`](https://elipousson.github.io/papersize/reference/make_page_size.md).

## See also

[`plot_band()`](https://elipousson.github.io/papersize/reference/plot_band.md)

## Examples

``` r
plot_band_dims(get_card("Poker"), n = 54)
#>   width height  units orientation      asp
#> 1   6.6    1.4 inches   landscape 4.714286

plot_band_dims(get_card("Tarot"), n = 78, orientation = "vertical")
#>   width height  units orientation        asp
#> 1   1.1  11.58 inches    portrait 0.09499136
```
