# Convert a page data.frame to a `viewport` class object

Create a `viewport` class object with a width and height matching the
dimensions of a page data.frame and default.units that match the page
units.

## Usage

``` r
page_to_viewport(page, name = NULL, cols = c("width", "height"), ...)
```

## Arguments

- page:

  A page data.frame from
  [`get_page_size()`](https://elipousson.github.io/papersize/reference/get_page_size.md)
  or
  [`make_page_size()`](https://elipousson.github.io/papersize/reference/make_page_size.md).

- name:

  A character value to uniquely identify the viewport once it has been
  pushed onto the viewport tree.

- cols:

  Column names to use for width and height columns. Defaults to
  c("width", "height"). Must be length 2 and the first value is always
  used as as the width name and the second as the height.

- ...:

  Arguments passed on to
  [`grid::viewport`](https://rdrr.io/r/grid/viewport.html)

  `x`

  :   A numeric vector or unit object specifying x-location.

  `y`

  :   A numeric vector or unit object specifying y-location.

  `just`

  :   A string or numeric vector specifying the justification of the
      viewport relative to its (x, y) location. If there are two values,
      the first value specifies horizontal justification and the second
      value specifies vertical justification. Possible string values
      are: `"left"`, `"right"`, `"centre"`, `"center"`, `"bottom"`, and
      `"top"`. For numeric values, 0 means left alignment and 1 means
      right alignment.

  `gp`

  :   An object of class `"gpar"`, typically the output from a call to
      the function [`gpar`](https://rdrr.io/r/grid/gpar.html). This is
      basically a list of graphical parameter settings.

  `clip`

  :   One of `"on"`, `"inherit"`, or `"off"`, indicating whether to clip
      to the extent of this viewport, inherit the clipping region from
      the parent viewport, or turn clipping off altogether. For
      back-compatibility, a logical value of `TRUE` corresponds to
      `"on"` and `FALSE` corresponds to `"inherit"`.

      May also be a grob (or a gTree) that describes a clipping path or
      the result of a call to
      [`as.path`](https://rdrr.io/r/grid/grid.stroke.html).

  `mask`

  :   One of `"none"` (or `FALSE`) or `"inherit"` (or `TRUE`) or a grob
      (or a gTree) or the result of call to
      [`as.mask`](https://rdrr.io/r/grid/as.mask.html). This specifies
      that the viewport should have no mask, or it should inherit the
      mask of its parent, or it should have its own mask, as described
      by the grob.

  `xscale`

  :   A numeric vector of length two indicating the minimum and maximum
      on the x-scale. The limits may not be identical.

  `yscale`

  :   A numeric vector of length two indicating the minimum and maximum
      on the y-scale. The limits may not be identical.

  `angle`

  :   A numeric value indicating the angle of rotation of the viewport.
      Positive values indicate the amount of rotation, in degrees,
      anticlockwise from the positive x-axis.

  `layout`

  :   A Grid layout object which splits the viewport into subregions.

  `layout.pos.row`

  :   A numeric vector giving the rows occupied by this viewport in its
      parent's layout.

  `layout.pos.col`

  :   A numeric vector giving the columns occupied by this viewport in
      its parent's layout.

## Value

A `viewport` class object with the same width and height as the input
page size.

## Examples

``` r
vp <- page_to_viewport(get_paper("Poker card"))

grid::grid.show.viewport(vp)

```
