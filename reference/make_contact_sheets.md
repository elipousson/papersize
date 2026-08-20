# Use `magick::image_ggplot()` to make contact sheets for images

**\[experimental\]** Wraps
[`filenamr::read_exif()`](https://elipousson.github.io/filenamr/reference/read_exif.html),
[`magick::image_ggplot()`](https://docs.ropensci.org/magick/reference/image_ggplot.html),
and
[`page_layout()`](https://elipousson.github.io/papersize/reference/page_layout.md)
to create contact shets for a folder of images.

## Usage

``` r
make_contact_sheets(
  images,
  dims = NULL,
  ncol = NULL,
  nrow = NULL,
  captions = "{file_name}\n{date_created}",
  caption_size = 12,
  caption_position = "panel",
  image_margin = margins(0.1, unit = "in"),
  page = "letter",
  orientation = "portrait",
  image_max = NULL,
  image_fileext = NULL,
  tags = NULL,
  tz = NULL,
  save = FALSE,
  filename = NULL,
  ...
)
```

## Arguments

- images:

  File path or data.frame from
  [`filenamr::read_exif()`](https://elipousson.github.io/filenamr/reference/read_exif.html)

- dims:

  Image dimensions in same dimensions as page. Required.

- ncol, nrow:

  The dimensions of the grid to create. If both are `NULL`, dims will be
  used or dims will be determined based on the plot dimensions.

- captions:

  Template for caption, passed to
  [`glue::glue_data()`](https://glue.tidyverse.org/reference/glue.html)
  using the images data.frame as .x. Note that this template may vary if
  you are using a custom tags parameter or modify the
  "filenamr.exif_xwalk" option. See
  [`filenamr::read_exif()`](https://elipousson.github.io/filenamr/reference/read_exif.html)
  for more details. Default: "{file_name}\n{date_created}"

- caption_size:

  Caption size, passed to
  [`ggplot2::element_text()`](https://ggplot2.tidyverse.org/reference/element.html)
  for plot.caption for theme, Default: 12

- caption_position:

  Caption position, passed to plot.caption.position for theme, Default:
  'panel'

- image_margin:

  Image margin passed Default: `margins(0.1, unit = "in")`

- page:

  Paper name or a data.frame with width and height columns. Optional if
  width and height are both provided, Default: `NULL`. If `ncol` and
  `nrow` are also supplied, `page` is not used to determine the grid
  size — but if `margin` or `marks` are used, `page` must still be set
  to the exact final output size (the same width/height you plan to pass
  to
  [`ggplot2::ggsave()`](https://ggplot2.tidyverse.org/reference/ggsave.html)),
  not just the combined size of the grid of plots. See `margin` and
  `marks` for why this matters.

- orientation:

  Paper orientation, Optional if width and height are both provided,
  Default: 'landscape'

- image_max:

  Maximum number of images to use for contact sheets.

- image_fileext:

  Passed to fileext parameter of
  [`filenamr::read_exif()`](https://elipousson.github.io/filenamr/reference/read_exif.html),
  Default: `NULL`

- tags:

  List of EXIF tags to read from files. If `NULL` (default), set to
  option "filenamr.exif_tags" or default `default_exif_tags`.

- tz:

  Time zone to pass to
  [`lubridate::ymd_hms()`](https://lubridate.tidyverse.org/reference/ymd_hms.html)
  if format_exif is `TRUE`. Typically set to
  [`Sys.timezone()`](https://rdrr.io/r/base/timezones.html) to convert
  date/time columns.

- save:

  If `TRUE`, save contact sheet to a file. filename may be required if
  save is `TRUE`. Default: `FALSE`

- filename:

  File name to create on disk.

- ...:

  Additional parameters passed to
  [`map_ggsave_ext()`](https://elipousson.github.io/papersize/reference/ggsave_ext.md)
  excluding width, height, and units.

## Value

A list of patchwork object or (if save = TRUE) invisibly return the list
and save a file.

## See also

[`filenamr::read_exif()`](https://elipousson.github.io/filenamr/reference/read_exif.html)
[`glue::glue()`](https://glue.tidyverse.org/reference/glue.html)
[`magick::editing()`](https://docs.ropensci.org/magick/reference/editing.html),
[`magick::image_ggplot()`](https://docs.ropensci.org/magick/reference/image_ggplot.html)
[`ggplot2::labs()`](https://ggplot2.tidyverse.org/reference/labs.html),
[`ggplot2::theme()`](https://ggplot2.tidyverse.org/reference/theme.html),
[`ggplot2::margin()`](https://ggplot2.tidyverse.org/reference/element.html)
