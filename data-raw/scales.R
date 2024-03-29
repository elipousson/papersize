standard_scales <-
  tibble::tribble(
    ~scale, ~standard, ~series, ~actual_ft, ~actual_ft_unit, ~scale_in, ~scale_in_unit, ~scale_in_accuracy, ~scale_cm, ~scale_cm_unit, ~scale_cm_accuracy, ~size_latlon, ~size_latlon_unit, ~area_approx, ~area_approx_unit, ~series_status,
    "1:20,000", "USGS", "Puerto Rico 7.5 minute", 6e-04, "in", 1670, "ft", "approximate", 200, "m", "exact", "7.5 by 7.5 minute", "minute", "7.10E+01", "sq mi", NA,
    "1:24,000", "USGS", "7.5 minute", 5e-04, "in", 2000, "ft", "exact", 240, "m", "exact", "7.5 by 7.5 minute", "minute", "49 to 70", "sq mi", NA,
    "1:25,000", "USGS", "7.5 minute", 0.00048, "in", 2080, "ft", "approximate", 250, "m", "exact", "7.5 by 7.5 minute", "minute", "49 to 70", "sq mi", NA,
    "1:25,000", "USGS", "7.5 by 15 minute", 0.00048, "in", 2080, "ft", "approximate", 250, "m", "exact", "7.5 by 15 minute", "minute", "98 to 140", "sq mi", NA,
    "1:50,000", "USGS", "USGS-DMA 15 minute", 0.00024, "in", 4170, "ft", "approximate", 500, "m", "exact", "15 by 15 minute", "minute", "197 to 282", "sq mi", NA,
    "1:62,500", "USGS", "15 minute*", NA, NA, 1, "mi", "approximate", 625, "m", "exact", "15 by 15 minute", "minute", "197 to 282", "sq mi", "abandoned",
    "1:63,360", "USGS", "Alaska Maps", NA, NA, 1, "mi", "exact", 634, "m", "exact", "15 by 20 to 36 minute", "minute", "207 to 281", "sq mi", NA,
    "1:50,000", "USGS", "County Maps", 0.00024, "in", 4170, "ft", "approximate", 500, "m", "exact", "County area", NA, "Varies", NA, NA,
    "1:100,000", "USGS", "County Maps", NA, NA, 1.6, "mi", "approximate", 1, "km", "exact", "County area", NA, "Varies", NA, NA,
    "1:100,000", "USGS", "30 by 60 minute", NA, NA, 1.6, "mi", "approximate", 1, "km", "exact", "30 by 60 minute", "minute", "1,568 to 2,240", "sq mi", NA,
    "1:125,000", "USGS", "30 minute*", NA, NA, 2, "mi", "approximate", 1.25, "km", "exact", "30 by 30 minute", "minute", "786 to 1,124", "sq mi", "abandoned",
    "1:250,000", "USGS", "1 degree by 2 degrees or 3 degrees", NA, NA, 4, "mi", "approximate", 2.5, "km", "exact", "1° by 2° or 3°", "degree", "4,580 to 8,669", "sq mi", NA,
    "1:500,000", "USGS", "State Maps", NA, NA, 8, "mi", "approximate", 5, "km", "exact", "State area", NA, "Varies", NA, NA,
    "1:1,000,000", "USGS", "State Maps", NA, NA, 16, "mi", "approximate", 10, "km", "exact", "State area", NA, "Varies", NA, NA,
    "1:2,000,000", "USGS", "U.S. Sectional Maps", NA, NA, 32, "mi", "approximate", 20, "km", "exact", "State groups", NA, "Varies", NA, NA,
    "1:250,000", "USGS", "Antarctica Maps", NA, NA, 4, "mi", "approximate", 2.5, "km", "exact", "1° by 3° to 15°", "degree", "4,089 to 8,336", "sq mi", NA,
    "1:500,000", "USGS", "Antarctica Maps", NA, NA, 8, "mi", "approximate", 5, "km", "exact", "2° by 7.5°", "degree", "28,174 to 30,462", "sq mi", NA,
    "1 in = 10 feet", "Engineering", NA, 0.1, "in", 10, "ft", "exact", 3.05, "m", "approximate", NA, NA, NA, NA, "common",
    "1 in = 20 feet", "Engineering", NA, 0.05, "in", 20, "ft", "exact", 6.1, "m", "approximate", NA, NA, NA, NA, "common",
    "1 in = 30 feet", "Engineering", NA, 0.0333, "in", 30, "ft", "exact", 9.14, "m", "approximate", NA, NA, NA, NA, "common",
    "1 in = 40 feet", "Engineering", NA, 0.025, "in", 40, "ft", "exact", 12.2, "m", "approximate", NA, NA, NA, NA, "common",
    "1 in = 50 feet", "Engineering", NA, 0.02, "in", 50, "ft", "exact", 15.2, "m", "approximate", NA, NA, NA, NA, "common",
    "1 in = 60 feet", "Engineering", NA, 0.0167, "in", 60, "ft", "exact", 18.3, "m", "approximate", NA, NA, NA, NA, "common",
    "3/32 in = 1 foot", "Architectural", NA, 0.0938, "in", 10.7, "ft", "approximate", 3.25, "m", "approximate", NA, NA, NA, NA, "common",
    "3/16 in = 1 foot", "Architectural", NA, 0.188, "in", 5.33, "ft", "approximate", 1.63, "m", "approximate", NA, NA, NA, NA, "common",
    "1/8 in = 1 foot", "Architectural", NA, 0.125, "in", 8, "ft", "exact", 2.44, "m", "approximate", NA, NA, NA, NA, "common",
    "1/4 in = 1 foot", "Architectural", NA, 0.25, "in", 4, "ft", "exact", 1.22, "m", "approximate", NA, NA, NA, NA, "common",
    "3/8 in = 1 foot", "Architectural", NA, 0.375, "in", 2.67, "ft", "approximate", 0.813, "m", "approximate", NA, NA, NA, NA, "common",
    "1/2 in = 1 foot", "Architectural", NA, 0.5, "in", 2, "ft", "exact", 0.61, "m", "approximate", NA, NA, NA, NA, "common",
    "3/4 in = 1 foot", "Architectural", NA, 0.75, "in", 1.33, "ft", "approximate", 0.406, "m", "approximate", NA, NA, NA, NA, "common",
    "1 in = 1 foot", "Architectural", NA, 1, "in", 1, "ft", "exact", 0.305, "m", "approximate", NA, NA, NA, NA, "common",
    "1½ in = 1 foot", "Architectural", NA, 1.5, "in", 0.667, "ft", "approximate", 0.203, "m", "approximate", NA, NA, NA, NA, "common",
    "1:5,000", "British military", "Local operations maps, used for similar purposes to the trench maps", NA, NA, 0.08, "mi", "exact", 24.4, "km", "approximate", NA, NA, NA, NA, NA,
    "1:10,000", "British military", "Trench maps, used for patrols and raids at a local level", NA, NA, 0.16, "mi", "exact", 48.8, "km", "approximate", NA, NA, NA, NA, NA,
    "1:20,000", "British military", "Heavy/Medium artillery maps, used as a precise locational aid for pinpointing particular targets", NA, NA, 0.31, "mi", "exact", 94.5, "km", "approximate", NA, NA, NA, NA, NA,
    "1:40,000", "British military", "Administration maps", NA, NA, 0.63, "mi", "exact", 192, "km", "approximate", NA, NA, NA, NA, NA
  )

usethis::use_data(
  standard_scales,
  overwrite = TRUE
)


in_scales <-
  map2(
    standard_scales$scale_in,
    standard_scales$scale_in_unit,
    function(x, y) {
      units::as_units(x, y)
    }
  )

cm_scales <-
  map2(
    standard_scales$scale_cm,
    standard_scales$scale_cm_unit,
    function(x, y) {
      units::as_units(x, y)
    }
  )

names(in_scales) <- standard_scales$scale
names(cm_scales) <- standard_scales$scale

usethis::use_data(in_scales, cm_scales, internal = TRUE)
