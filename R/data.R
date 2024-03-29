#'  Standard paper and image sizes
#'
#' Reference table of standard paper, postcard, photo print, social media image
#' sizes, and playing card sizes for [get_page_size()]. Derived from
#' [visioguy/PaperSizes](https://github.com/visioguy/PaperSizes/) repo, [Adobe
#' UK guide to photo
#' sizes](https://www.adobe.com/uk/creativecloud/photography/discover/standard-photo-sizes.html)
#' and other sources. Data is identical to data included with `{sfext}` package.
#'
#' @format A data frame with 125 rows and 9 variables:
#' \describe{
#'   \item{`name`}{Name of paper}
#'   \item{`series`}{Series}
#'   \item{`standard`}{Standard}
#'   \item{`size`}{Size in series}
#'   \item{`units`}{Units ("in", "mm", or "px") for dimensions}
#'   \item{`width`}{Width in units}
#'   \item{`height`}{Height in units}
#'   \item{`orientation`}{Portrait (width less than height), landscape, or
#'   square}
#'   \item{`type`}{Type (paper, postcard, print, or social)}
#' }
"paper_sizes"

#' Standard card sizes
#'
#' Reference table of common playing card sizes for [get_card()]. Data is a
#' subset of `paper_sizes` which is also included with `{sfext}` package.
#'
#' @format A data frame with 5 rows and 6 variables:
#' \describe{
#'   \item{`name`}{Name of card}
#'   \item{`units`}{Units ("in" or "mm") for dimensions}
#'   \item{`width`}{Width in units}
#'   \item{`height`}{Height in units}
#'   \item{`orientation`}{Portrait (width less than height), landscape, or
#'   square}
#' }
"card_sizes"

#' Extra reference data for page layouts
#'
#' A named list of additional reference data that currently includes only one
#' data.frame: a reference table of margin sizes in "in" and "cm".
#'
#' @format A length 1 list.
"page_extras"

#' Standard map, architectural, and engineering scales
#'
#' Standard map scales derived from USGS 2002 report on map scales
#' <https://pubs.usgs.gov/fs/2002/0015/report.pdf>
#'
#' Common architectural and engineering scales derived from FEMA guide to using
#' scales
#' <https://www.usfa.fema.gov/downloads/pdf/nfa/engineer-architect-scales.pdf>
#'
#' @format A data frame with 36 rows and 16 variables:
#' \describe{
#'   \item{`scale`}{Scale name}
#'   \item{`standard`}{Standard (USGS, architectural, or engineering)}
#'   \item{`series`}{Series name (USGS map scales only)}
#'   \item{`actual_ft`}{Scale distance for 1 ft actual.}
#'   \item{`actual_ft_unit`}{Unit of scale for 1 ft actual.}
#'   \item{`scale_in`}{Actual distance for 1 in scale.}
#'   \item{`scale_in_unit`}{Unit of actual distance for 1 in scale.}
#'   \item{`scale_in_accuracy`}{Accuracy of 1 in scale (approximate or exact)}
#'   \item{`scale_cm`}{Actual distance for 1 cm scale.}
#'   \item{`scale_cm_unit`}{Unit of actual distance for 1 cm scale.}
#'   \item{`scale_cm_accuracy`}{Accuracy of 1 cm scale (approximate or exact)}
#'   \item{`size_latlon`}{Standard size in latitude/longitude}
#'   \item{`size_latlon_unit`}{Unit of latitude/longitude size (minutes or
#'   degrees)}
#'   \item{`area_approx`}{Approximate actual area}
#'   \item{`area_approx_unit`}{Approximate area unit}
#'   \item{`series_status`}{Series status (select USGS map series are
#'   "abandoned")}
#' }
"standard_scales"

#' Distance units (data frame)
#'
#' A subset of units supported by the units package accessible through the
#' [units::valid_udunits()] function.
#'
#' @format A data frame with 33 rows and 12 variables:
#' \describe{
#'   \item{`symbol`}{symbols}
#'   \item{`symbol_aliases`}{symbol aliases}
#'   \item{`name_singular`}{singular names}
#'   \item{`name_singular_aliases`}{singular name aliases}
#'   \item{`name_plural`}{character plural names}
#'   \item{`name_plural_aliases`}{plural name aliases}
#'   \item{`def`}{short definition}
#'   \item{`definition`}{definition}
#'   \item{`comment`}{comment}
#'   \item{`dimensionless`}{logical indicator for dimensionless units}
#'   \item{`source_xml`}{source XML}
#'   \item{`unit_opts`}{character vector with symbols, singular, and plural
#'   names for the unit}
#' }
"dist_units"

#' Distance units (vector)
#'
#' A vector of supported distance units pulled from `dist_units`.
#'
#' @format A character vector with 86 names, plural names, aliases, and symbols
#'   for distance units.
"dist_unit_options"

#' Area units (vector)
#'
#' A vector of supported area units derived from `dist_units` and
#' [units::valid_udunits()].
#'
#' @format A character vector with 41 names, plural names, and aliases for area
#'   units.
"area_unit_options"

#' Grid units (vector)
#'
#' A vector of units supported by [grid::unit()]
#'
#' @format A character vector with 34 abbreviated, singular, and plural unit
#'   names.
"grid_units"
