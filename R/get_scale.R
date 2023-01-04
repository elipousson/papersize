#' Get standard scales and convert to scale distances
#'
#' This function returns a scale from [standard_scales] based on a provided
#' name, standard, and/or series.
#'
#' @name get_scale
#' @aliases get_standard_scale
#' @param scale Scale name from `standard_scales[["scale"]]`.
#' @param standard Scale standard. Options include "USGS", "Engineering", or
#'   "Architectural".
#' @param series Map series from `standard_scales[["series"]]`. Series is only
#'   available for USGS scales.
#' @return A tibble based on [standard_scales] with rows filtered to values that
#'   match parameters.
#' @export
#' @importFrom rlang arg_match
get_scale <- function(scale = NULL,
                      standard = NULL,
                      series = NULL) {
  scales <- filter_col(standard_scales, scale)

  if (!is.null(standard)) {
    standard <- rlang::arg_match(
      standard,
      c("USGS", "Engineering", "Architectural"),
      multiple = TRUE
    )
    scales <- filter_col(scales, standard)
  }

  if (!is.null(series)) {
    series <- rlang::arg_match(
      series,
      unique(standard_scales$series),
      multiple = TRUE
    )
    scales <- filter_col(scales, series)
  }

  scales
}
