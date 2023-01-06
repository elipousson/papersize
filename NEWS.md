<!-- NEWS.md is maintained by https://cynkra.github.io/fledge, do not edit -->

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


