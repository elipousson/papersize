#' General utility functions for working with distance units objects
#'
#' - [is_dist_units()]: Is x a distance unit object?
#' - [get_dist_units()]: Get the distance units from x (if x is a sf or units
#' objects or a character string from [dist_unit_options])
#' - [as_dist_units()]: Convert x to units using [units::as_units]
#'
#' @name is_dist_units
#' @param x,y objects to check
#' @family dist
#' @export
is_dist_units <- function(x) {
  is_units(x) && (get_dist_units(x) %in% c(dist_unit_options, area_unit_options))
}

#' @name get_dist_units
#' @rdname is_dist_units
#' @param multiple If `TRUE` and x is a character vector with distance/area
#'   units, [get_dist_units()] may return multiple units. Passed to
#'   [rlang::arg_match].
#' @param quiet If `TRUE`, suppress warning messages.
#' @export
#' @importFrom rlang check_installed arg_match
#' @importFrom cliExtras cli_warn_ifnot cli_abort_ifnot
get_dist_units <- function(x, multiple = TRUE, quiet = FALSE) {
  if (is.null(x)) {
    return(x)
  }

  if (is_sf_ext(x)) {
    rlang::check_installed("sf")
    return(sf::st_crs(x)$units_gdal)
  }

  if (is.character(x)) {
    x <- underscore(x)
    x <- rlang::arg_match(
      x,
      c(dist_unit_options, area_unit_options),
      multiple = multiple
    )

    return(x)
  }

  if (is_units(x)) {
    x_is_dist_unit <-
      all(as.character(units(x)[["numerator"]]) %in% dist_unit_options)

    x_not_area_unit <-
      !(as.character(units(x)) %in% area_unit_options)

    if (x_is_dist_unit && x_not_area_unit) {
      return(as.character(units(x)[["numerator"]]))
    }

    return(as.character(units(x)))
  }

  if (is.numeric(x)) {
    cliExtras::cli_warn_ifnot(
      "{.var units} can't be determined for a numeric vector with
      no {.arg units} attribute.",
      condition = quiet
    )

    return(invisible(NULL))
  }

  cliExtras::cli_abort_ifnot(
    "{.var units} must be a {.cls character} string from
    {.code dist_unit_options} or {.code area_unit_options}, a {.cls units}
    object, or a {.cls sf} object with a valid crs.",
    condition = inherits(x, c("character", "units", "sf"))
  )

  rlang::arg_match(x, c(dist_unit_options, area_unit_options),
    multiple = multiple
  )
}


#' @name as_dist_units
#' @rdname is_dist_units
#' @param units Distance units to convert to. Must be one of dist_unit_options
#'   or area_unit_options.
#' @param call Passed to error_call for [rlang::arg_match()]
#' @export
#' @importFrom rlang arg_match
#' @importFrom cliExtras cli_yesno
as_dist_units <- function(x,
                          units = NULL,
                          call = caller_env()) {
  units <- get_dist_units(units)

  units <-
    rlang::arg_match(
      units,
      c(dist_unit_options, area_unit_options),
      error_call = call
    )

  if (is.numeric(x) && !is_dist_units(x)) {
    rlang::check_installed("units")
    return(units::as_units(x, units))
  }

  if (cliExtras::cli_yesno(
    "Did you mean to convert {.var x} to {.val {units}}?"
  )) {
    convert_dist_units(
      dist = x,
      to = units
    )
  }
}
