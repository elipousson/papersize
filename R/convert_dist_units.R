#' Convert distance (and area) values between different units
#'
#' @param dist Numeric or units object
#' @param from Existing unit for dist, Default: `NULL`. If dist is a units
#'   object, the numerator is used as "from"
#' @param to Unit to convert distance to, Default: 'meter'
#' @param drop If `TRUE`, return numeric. If `FALSE`, return class units object.
#' @param digits Number of digits to include in result; defaults to `NULL`.
#' @return Object created by [units::set_units()]
#' @rdname convert_dist_units
#' @family dist
#' @examples
#' convert_dist_units(1, from = "mile", to = "km")
#'
#' convert_dist_units(3, from = "ft", to = "yard")
#'
#' mile <- units::set_units(1, "mi")
#'
#' convert_dist_units(mile, to = "feet")
#'
#' is_same_units(mile, "mile")
#'
#' @seealso [is_same_unit_type()]
#' @export
convert_dist_units <- function(dist,
                               from = NULL,
                               to = "meter",
                               drop = FALSE,
                               digits = NULL) {
  cliExtras::cli_abort_ifnot(
    "{.arg dist} must be a numeric or units class object.",
    condition = (is.numeric(dist) || is_units(dist))
  )

  if (is_units(dist)) {
    rlang::check_installed("units")

    dist_from <- get_dist_units(dist)

    if (!is.null(from) && (dist_from != from)) {
      cli::cli_warn(
        c("{.arg dist} is class {.cls units} and has different units than
        {.arg from} ({.val {from}}).",
          "*" = "Replacing {.arg from} with {.val {dist_from}}."
        )
      )
    }

    from <- dist_from
    dist <- units::drop_units(dist)
  }

  if (!is.null(from)) {
    dist <- set_dist_units(dist, value = from)
  }

  dist <- set_dist_units(dist, value = to)

  if (!is.null(digits)) {
    dist <- round(dist, digits = digits)
  }

  if (!drop) {
    return(dist)
  }

  as.numeric(dist)
}


#' Set distance units
#'
#' @noRd
#' @importFrom rlang caller_env arg_match
set_dist_units <- function(x = NULL,
                           value = NULL,
                           mode = "standard",
                           call = caller_env()) {
  if (is.null(value)) {
    return(x)
  }

  value <- underscore(value)

  value <-
    rlang::arg_match(
      value,
      c(dist_unit_options, area_unit_options),
      error_call = call
    )

  rlang::check_installed("units")
  units::set_units(
    x = x,
    value = value,
    mode = mode
  )
}

#' @noRd
as_units_attr <- function(x) {
  if (is.character(x)) {
    rlang::check_installed("units")
    x <- units::as_units(x)
  }

  units(x)
}

#' @name is_same_units
#' @rdname  is_dist_units
#' @export
is_same_units <- function(x, y = NULL) {
  if (is.null(x) | is.null(y)) {
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
    all(dens == character(0)) | (dens[1] == dens[2])
  )) {
    return(TRUE)
  }

  rlang::check_installed("units")
  units::as_units(x) == units::as_units(y)
}

#' Replace spaces with underscores
#'
#' @noRd
underscore <- function(x) {
  gsub(" ", "_", x)
}
