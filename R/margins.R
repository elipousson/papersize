
#' Specify the margins of a page or element
#'
#' An extended version of [ggplot2::margin()] with more flexibility in
#' specification. If margin is a margin class object it is returned as is. If
#' margin is length 1, the t, r, b, and l values are all set to that value. If
#' margin is length 2, the t and b are set to half the first value and the r and
#' l are set to half the second value. The ... parameters also allow you to use
#' the same syntax as margin.
#'
#' @param margin A numeric list or vector or a margin class object. For
#'   [get_margin()] only, margin can be a margin name from
#'   `page_extras$margins`. Defaults to `NULL`.
#' @param ... Additional numeric values combined with margin if provided.
#' @param unit Default units for margins (ignored if margin is a margin class
#'   object). Passed to as_unit_type() so units class objects as well as unit
#'   names supported [grid::unit()] are allowed. Defaults to "in" except for
#'   [margin()] where unit defaults to "pt" to match [ggplot2::margin()].
#' @examples
#'
#' margins(1, unit = "in")
#'
#' margins(c(2, 2), unit = "in")
#'
#' margins(list(1, 1, 1.5, 1), unit = "cm")
#'
#' margins(t = 1, r = 3, b = 1.5, l = 3, unit = "in")
#'
#' standard_margin <- get_margin("standard", unit = "in")
#'
#' is_margin(standard_margin)
#'
#' standard_margin
#'
#' @export
#' @importFrom rlang has_name
#' @importFrom cli cli_abort
margins <- function(margin = NULL, ..., unit = "in") {
  if (!missing(...)) {
    margin <- c(margin, ...)
  }

  if (is_margin(margin)) {
    if (!is_same_unit_type(margin, unit)) {
      cli::cli_warn(
        "{.arg margin} uses {.val {as_unit_type(margin)}} but the provided
        {.arg unit} is {.val {unit}}."
      )
    }

    return(margin)
  }

  if (is.list(margin)) {
    margin <- unlist(margin)
  }

  if (all(rlang::has_name(margin, c("t", "r", "b", "l")))) {
    margin <- c(margin["t"], margin["r"], margin["b"], margin["l"])
  }

  if (length(margin) == 1) {
    margin <- rep(margin, 4)
  }

  unit <- as_unit_type(unit)

  if (length(margin) == 4) {
    return(margin(margin[1], margin[2], margin[3], margin[4], unit))
  }

  if (length(margin) == 2) {
    margin <- margin / 2
    return(margin(margin[1], margin[2], margin[1], margin[2], unit))
  }

  cli::cli_abort(
    "{.arg margin} must be a length 1, 2, or 4 {.cls numeric} vector."
  )
}

#' @name is_margin
#' @rdname margins
#' @param x Object to check for class margin and unit
#' @export
is_margin <- function(x) {
  inherits(x, "margin") & inherits(x, "unit")
}

#' @name margin
#' @rdname margins
#' @param t,r,b,l Dimensions of each margin: top, right, bottom, and left. (To
#'   remember order, think trouble).
#' @source ggplot2 package
#' @export
#' @importFrom grid unit
margin <- function(t = 0, r = 0, b = 0, l = 0, unit = "pt") {
  u <- grid::unit(c(t, r, b, l), units = unit)
  class(u) <- c("margin", class(u))
  u
}

#' @name get_margin
#' @rdname margins
#' @export
#' @importFrom rlang arg_match
get_margin <- function(margin = NULL, ..., unit = "in") {
  if (is_margin(margin)) {
    return(margin)
  }

  if (is.character(margin)) {
    margin <- rlang::arg_match(margin, unique(page_extras$margins$name))
    margin <- filter_data(page_extras$margins, margin, "name")
    margin <- filter_data(margin, unit)
    margin <- c(margin["t"], margin["r"], margin["b"], margin["l"])
  }

  margins(margin, ..., unit = unit)
}
