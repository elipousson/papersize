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
