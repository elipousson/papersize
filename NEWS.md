# papersize (development version)

- `add_card_number()` gains a `sequence` argument to control whether `n` is
  treated as a count (the default, expanded with `seq(n)`) or used as-is.
- `annotate_wave()` is a new function for adding a wavy (sine or zigzag) line
  annotation to a plot.
- `as_page()` now errors on `NA` input for `x`, and supports `NULL` input
  (used internally by `get_page_dims()` for unitless width/height).
- `as_thickness()` is a new helper for specifying material thickness, used by
  the `plot_band()` family of functions.
- `as_unit_type()` now errors on `NULL` input instead of failing later with a
  less informative error.
- `ggsave_ext()` no longer mangles the directory portion of an absolute
  `filename` (it was being run through name-cleaning meant for basenames
  only), and no longer errors when saving a `patchwork` plot as a PDF.
- `make_page_size()` gains a `require_units` argument (default `TRUE`) that
  allows `units` to be omitted when set to `FALSE`.
- `map_ggsave_ext()` fixes the same absolute-path mangling issue as
  `ggsave_ext()`, fixes an error from an undefined `output_path` variable
  when saving a list of `patchwork` plots to a non-PDF file, and fixes
  page-number padding in multi-plot filenames (e.g. `pg_01`, `pg_02`, ...
  instead of a broken suffix caused by an unintended line break).
- `margin()` is no longer exported in favor of `ggplot2::margin()`.
  `margins()` and `get_margin()` now build on `ggplot2::margin()` and
  `ggplot2::margin_auto()`; a single-value `margins()` input now uses
  `ggplot2::margin_auto()` instead of repeating the value on all four sides.
- `page_layout()` gains a `gutter` argument to add spacing between plots in
  the grid.
- `plot_band()`, `plot_band_dims()`, and `plot_band_page()` are new functions
  for creating a printable band or strip of repeated card-style content, with
  fixes for handling mismatched units between inputs.

# papersize 0.1.1 (2024-10-08)

- Add `increment` parameter to `ggsave_ext()`.
- Avoid using `.onLoad` to load package data (use namespacing instead per [guidance in R Packages book](https://r-pkgs.org/data.html#sec-data-data))
- Fix input checks for `make_page_size()` and `set_page_grid()`
- Export `inset_page()`, `add_card_border()`, `add_card_text()`, and `add_card_number()` as internal functions.

# papersize 0.1.0.9001 (2023-04-07)

- Add `set_page_dims() `and related functions.
- Add `page_to_layout()`, and `page_to_viewport()`.
- Add `as_page()` function.
- Add `as_asp()` function.
- Add `margins()`, `get_margin()`, `is_margin()` and `set_page_margins()` functions.
- Add `is_unit_type()` function
- Add `page_extras` + `grid_units` reference data.
- Add `get_social_size()` function.
- Allow `ggsave_ext()` and `map_ggsave_ext()` to returns plot invisibly. Add "fileext" parameter to both functions.
- Allow `convert_dist_units()` support units class objects and grid unit objects as "from" or "to" parameters.
- Add `as_dist_units()` function.
- Move ggplot2 and patchwork units to Suggests
- Add gridExtra, magick, qpdf, and filenamr to Suggests

# papersize 0.1.0.9000 (2023-01-06)

- feat: add make_page_size function
- refactor: update set_page_orientation to add orientation col if it is not provided
- docs: fill in missing parameter definitions
- feat: add asp column to pages + set_asp_col() helper
- refactor: add cols arg for more flexibility in naming of paper data.frames
- refactor: replace page_filter w/ more general filter_col helper
- refactor(plot_cards): drop center arg from helper functions w/ assumption that x and y columns are added to card
- refactor(get_page): improve handling of orientation w/ new reorient parameter
- feat(get_page_dims): export new function for retrieving page dimensions
- feat(convert_page_units): export new function for converting page units (also add units parameter to get_page)

