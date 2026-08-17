# Convert distance from scale to actual units

This function converts scale distances to actual units based on named
[standard_scales](https://elipousson.github.io/papersize/reference/standard_scales.md).

## Usage

``` r
convert_dist_scale(
  dist = NULL,
  scale = NULL,
  standard = NULL,
  series = NULL,
  scale_unit = "in",
  scale_factor = NULL,
  actual_unit = NULL,
  dpi = 120,
  paper = NULL,
  orientation = NULL,
  ...
)
```

## Arguments

- dist:

  distance to convert. If paper is provided, dist is optional and paper
  width and height are used as dist.

- scale:

  Scale name from `standard_scales[["scale"]]`.

- standard:

  Scale standard. Options include "USGS", "Engineering", or
  "Architectural".

- series:

  Map series from `standard_scales[["series"]]`. Series is only
  available for USGS scales.

- scale_unit:

  "mm" (converted to cm by dividing by 10), "cm", "px" (converted to
  inches by dividing by dpi), or "in".

- scale_factor:

  factor for converting from scale_unit to actual_unit, e.g. if 1" = 1',
  the scale factor is 12. optional if scale if provided; defaults to
  `NULL`.

- actual_unit:

  any unit supported by
  [`convert_dist_units()`](https://elipousson.github.io/papersize/reference/convert_dist_units.md)

- dpi:

  dots per square inch (used as conversion factor for "px" to "in")

- paper:

  Name of paper passed to
  [`get_paper()`](https://elipousson.github.io/papersize/reference/get_page_size.md)

- orientation:

  Page orientation, Default: `NULL`. Supported options are "portrait",
  "landscape", or "square".

- ...:

  Arguments passed on to
  [`get_paper`](https://elipousson.github.io/papersize/reference/get_page_size.md)

  `name`

  :   Page name, e.g. "letter", not case sensitive, Default: `NULL`

  `width`

  :   Page width in "in", "px" or "mm" units. Default: `NULL`

  `height`

  :   Page height in "in", "px" or "mm" units. Default: `NULL`

  `reorient`

  :   If `TRUE` and orientation is not `NULL`, flip width and height
      dimensions for any pages that do not match the provided
      orientation. Set `reorient = FALSE` to filter pages by
      orientation.

  `type`

  :   Page type, Options include "paper", "social", "postcard", "print",
      "card", or "screen". Default: `NULL`

  `ignore.case`

  :   If `FALSE`, filtering for page and type are case sensitive.
      Defaults to `TRUE`.

  `page`

  :   Used by
      [`get_page_dims()`](https://elipousson.github.io/papersize/reference/get_page_size.md),
      page is either a character vector passed to the name parameter of
      [`get_page_size()`](https://elipousson.github.io/papersize/reference/get_page_size.md),
      a data.frame with column names matching the cols parameter, or a
      length 2 numeric vector with the page width and height.

  `cols`

  :   Length 2 character vector with column names for page dimensions.
      Defaults to c("width", "height").

  `arg,call`

  :   Passed to
      [`cli_abort()`](https://cli.r-lib.org/reference/cli_abort.html) to
      improve internal error messages.

  `units`

  :   Units to convert page dimensions to using
      [`convert_unit_type()`](https://elipousson.github.io/papersize/reference/as_unit.md).

  `valueOnly`

  :   Passed to valueOnly parameter of
      [`grid::convertUnit()`](https://rdrr.io/r/grid/grid.convert.html).
      Defaults to `FALSE`.

## Value

- If paper is not provided, return a vector of dist values converted
  from scale_unit to actual_unit based on scale_factor or information
  from
  [standard_scales](https://elipousson.github.io/papersize/reference/standard_scales.md)
  data.

- If paper is provided, return a data.frame with converted distances
  appends as columns named actual_width and actual_height.

## See also

Other dist:
[`convert_dist_units()`](https://elipousson.github.io/papersize/reference/convert_dist_units.md),
[`is_dist_units()`](https://elipousson.github.io/papersize/reference/is_dist_units.md)
