#' General utility functions for working with distance units objects
#'
#' - [is_dist_units()]: Is x a units object with a units attribute in
#' `dist_unit_options` or `area_unit_options`?
#' - [get_dist_units()]: Get the distance units from x (if x is a sf or units
#' objects or a character string from [dist_unit_options])
#' - [as_dist_units()]: Convert x to units using [units::as_units]
#' - [is_same_units()]: Do x and y have the same distance units attribute?
#' Names or symbols of valid distance units are allowed.
#'
#' @name is_dist_units
#' @param x,y objects to check
#' @param arg Used internally and passed to [rlang::arg_match()] as error arg or
#'   used by [cli::cli_abort()] to improve error messages.
#' @inheritParams rlang::args_error_context
#' @family dist
#' @examples
#'
#' mile <- units::set_units(1, "mi")
#'
#' is_dist_units("mi")
#'
#' is_dist_units(mile)
#'
#' is_same_units(mile, "mile")
#'
#' get_dist_units(mile)
#'
#' as_dist_units(1, "mi")
#'
#' as_dist_units(2, mile)
#'
#' @export
#' @importFrom rlang caller_arg
is_dist_units <- function(x, arg = caller_arg(x)) {
  allowed_units <- c(
    papersize::dist_unit_options,
    papersize::area_unit_options
  )

  is_units(x) && (get_dist_units(x, arg) %in% allowed_units)
}

#' @name get_dist_units
#' @rdname is_dist_units
#' @export
#' @importFrom rlang check_installed arg_match caller_arg
#' @importFrom cliExtras cli_warn_ifnot cli_abort_ifnot
get_dist_units <- function(x,
                           arg = caller_arg(x),
                           call = parent.frame()) {
  rlang::check_required(x)
  if (is_null(x)) {
    return(x)
  }

  if (is_character(x)) {
    x <- underscore(x)
    allowed_units <- c(
      papersize::dist_unit_options,
      papersize::area_unit_options
    )

    x <- arg_match(
      arg = x,
      values = allowed_units,
      error_arg = arg,
      multiple = TRUE,
      call = call
    )

    return(x)
  }

  if (is_sf_ext(x)) {
    check_installed("sf")
    return(sf::st_crs(x)$units_gdal)
  }

  if (is_units(x)) {
    return(as.character(units(x)))
  }

  if (is_unit(x)) {
    unit_type <- as_unit_type(x)

    unit_type <-
      rlang::try_fetch(
        rlang::arg_match(
          arg = unit_type,
          values = papersize::grid_units[c(2:13)],
          error_arg = arg,
          error_call = call
        ),
        error = function(cnd) {
          cli_abort(
            c("i" = "{.arg {arg}} is a {.cls unit} object and the provided
            {.val {unit_type}} unit type can't be used."),
            parent = cnd,
            call = call
          )
        }
      )

    return(unit_type)
  }

  if (is_bare_numeric(x)) {
    cli_warn(
      "units can't be determined for a numeric vector
      that is not a {.cls unit} or {.cls units} object."
    )

    return(invisible(NULL))
  }

  cli_abort(
    c("{.arg {arg}} must be a {.cls character} string matching a value from
    {.code dist_unit_options} or {.code area_unit_options}, a {.cls units}
    object, or a {.cls sf} object with a valid crs.",
      "i" = "{.arg {arg}} is a {.cls {class(x)}} class object."
    ),
    call = call
  )
}

#' @name as_dist_units
#' @rdname is_dist_units
#' @param units Distance units to convert to. Must be one of dist_unit_options
#'   or area_unit_options.
#' @export
#' @importFrom rlang caller_arg check_installed is_interactive
#' @importFrom cliExtras cli_abort_if cli_yesno
#' @importFrom units as_units
as_dist_units <- function(x,
                          units = NULL,
                          arg = caller_arg(x),
                          call = parent.frame()) {
  units <- get_dist_units(units, call = call)

  if (is_bare_numeric(x) && !is_units(x)) {
    cliExtras::cli_abort_if(
      "{.arg units} must be length 1,
      not length {length(units)}." = length(units) > 1
    )

    return(units::as_units(x, units))
  }

  convert_dist <- TRUE
  if (is_interactive()) {
    convert_dist <- cliExtras::cli_yesno(
      "Did you mean to convert {.arg {arg}} to {.val {units}}?"
    )
  }

  if (convert_dist) {
    convert_dist_units(
      dist = x,
      to = units
    )
  }
}

#' @name is_same_units
#' @rdname  is_dist_units
#' @export
is_same_units <- function(x, y = NULL) {
  if (is_null(x) || is_null(y)) {
    return(FALSE)
  }

  x <- as_units_attr(x)
  y <- as_units_attr(y)

  in_opts <- c("in", "inch", "inches", "international_inch", "international_inches")
  ft_opts <- c("ft", "foot", "feet", "international_foot", "international_feet")
  yd_opts <- c("yd", "yard", "yards", "international_yard", "international_yards")

  nums <- c(x[["numerator"]], y[["numerator"]])
  dens <- c(x[["denominator"]], y[["denominator"]])

  if (any(
    c(all(nums %in% in_opts), all(nums %in% ft_opts), all(nums %in% yd_opts))
  ) && (
    all(dens == character(0)) || (dens[1] == dens[2])
  )) {
    return(TRUE)
  }

  try_as_units(x) == try_as_units(y)
}

#' @noRd
as_units_attr <- function(x) {
  if (is_character(x)) {
    x <- underscore(x)
    x <- try_as_units(x)
  }

  units(x)
}

#' @noRd
# is_dist_unit_option <- function(x) {
#   all(as.character(units(x)[["numerator"]]) %in% papersize::dist_unit_options) & !is_area_unit_option(x)
# }

#' @noRd
# is_area_unit_option <- function(x) {
#   as.character(units(x)) %in% papersize::area_unit_options
# }
