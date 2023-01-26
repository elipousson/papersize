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
#' @importFrom units drop_units
convert_dist_units <- function(dist,
                               from = NULL,
                               to = "meter",
                               drop = FALSE,
                               digits = NULL) {
  cliExtras::cli_abort_ifnot(
    "{.arg dist} must be a {.cls numeric} or
    {.cls units} object." = any(c(is.numeric(dist), is_units(dist)))
  )

  if (!is.null(from) & !is.character(from)) {
    from <- get_dist_units(from)
  }

  if (!is.null(to) & !is.character(to)) {
    to <- get_dist_units(to)
  }

  if (is_units(dist) | is_unit(dist)) {
    dist_from <- get_dist_units(dist)

    if (!is.null(from)) {
      cliExtras::cli_warn_ifnot(
        message = c("{.arg dist} is class {.cls units} and has different units than
        {.arg from} ({.val {from}}).",
          "*" = "Replacing {.arg from} with {.val {dist_from}}."
        ),
        condition = (dist_from == from)
      )
    }

    from <- dist_from

    if (is_units(dist)) {
      dist <- units::drop_units(dist)
    } else {
      dist <- as.numeric(dist)
    }
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
#' @importFrom units set_units
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

  units::set_units(
    x = x,
    value = value,
    mode = mode
  )
}

#' @noRd
try_as_units <- function(x, arg = caller_arg(x), call = parent.frame()) {
  rlang::try_fetch(
    units::as_units(x),
    error = function(cnd) {
      cli::cli_abort(
        "{.arg {arg}} must be a valid unit symbol or name.",
        parent = cnd,
        call = call
      )
    }
  )
}

#' Replace spaces with underscores
#'
#' @noRd
underscore <- function(x) {
  gsub(" ", "_", x)
}
