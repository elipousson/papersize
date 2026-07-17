# Changelog

## papersize 0.1.1 (2024-10-08)

- Add `increment` parameter to
  [`ggsave_ext()`](https://elipousson.github.io/papersize/reference/ggsave_ext.md).
- Avoid using `.onLoad` to load package data (use namespacing instead
  per [guidance in R Packages
  book](https://r-pkgs.org/data.html#sec-data-data))
- Fix input checks for
  [`make_page_size()`](https://elipousson.github.io/papersize/reference/make_page_size.md)
  and `set_page_grid()`
- Export
  [`inset_page()`](https://elipousson.github.io/papersize/reference/inset_page.md),
  [`add_card_border()`](https://elipousson.github.io/papersize/reference/add_card_border.md),
  [`add_card_text()`](https://elipousson.github.io/papersize/reference/add_card_text.md),
  and
  [`add_card_number()`](https://elipousson.github.io/papersize/reference/add_card_number.md)
  as internal functions.

## papersize 0.1.0.9001 (2023-04-07)

- Add
  [`set_page_dims()`](https://elipousson.github.io/papersize/reference/set_page_dims.md)and
  related functions.
- Add
  [`page_to_layout()`](https://elipousson.github.io/papersize/reference/page_to_layout.md),
  and
  [`page_to_viewport()`](https://elipousson.github.io/papersize/reference/page_to_viewport.md).
- Add
  [`as_page()`](https://elipousson.github.io/papersize/reference/as_page.md)
  function.
- Add
  [`as_asp()`](https://elipousson.github.io/papersize/reference/as_asp.md)
  function.
- Add
  [`margins()`](https://elipousson.github.io/papersize/reference/margins.md),
  [`get_margin()`](https://elipousson.github.io/papersize/reference/margins.md),
  [`is_margin()`](https://elipousson.github.io/papersize/reference/margins.md)
  and `set_page_margins()` functions.
- Add
  [`is_unit_type()`](https://elipousson.github.io/papersize/reference/as_unit.md)
  function
- Add `page_extras` + `grid_units` reference data.
- Add
  [`get_social_size()`](https://elipousson.github.io/papersize/reference/get_social_size.md)
  function.
- Allow
  [`ggsave_ext()`](https://elipousson.github.io/papersize/reference/ggsave_ext.md)
  and
  [`map_ggsave_ext()`](https://elipousson.github.io/papersize/reference/ggsave_ext.md)
  to returns plot invisibly. Add “fileext” parameter to both functions.
- Allow
  [`convert_dist_units()`](https://elipousson.github.io/papersize/reference/convert_dist_units.md)
  support units class objects and grid unit objects as “from” or “to”
  parameters.
- Add
  [`as_dist_units()`](https://elipousson.github.io/papersize/reference/is_dist_units.md)
  function.
- Move ggplot2 and patchwork units to Suggests
- Add gridExtra, magick, qpdf, and filenamr to Suggests

## papersize 0.1.0.9000 (2023-01-06)

- feat: add make_page_size function
- refactor: update set_page_orientation to add orientation col if it is
  not provided
- docs: fill in missing parameter definitions
- feat: add asp column to pages + set_asp_col() helper
- refactor: add cols arg for more flexibility in naming of paper
  data.frames
- refactor: replace page_filter w/ more general filter_col helper
- refactor(plot_cards): drop center arg from helper functions w/
  assumption that x and y columns are added to card
- refactor(get_page): improve handling of orientation w/ new reorient
  parameter
- feat(get_page_dims): export new function for retrieving page
  dimensions
- feat(convert_page_units): export new function for converting page
  units (also add units parameter to get_page)
