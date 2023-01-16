#' @noRd
get_units_col <- function(default = "units") {
  getOption("papersize.units_col", default = default)
}

#' @noRd
set_units_col <- function(x = "units") {
  options("papersize.units_col" = x)
}

#' @noRd
get_orientation_col <- function(default = "orientation") {
  getOption("papersize.orientation_col", default = default)
}

#' @noRd
set_orientation_col <- function(x = "orientation") {
  options("papersize.orientation_col" = x)
}

#' @noRd
get_asp_col <- function(default = "asp") {
  getOption("papersize.asp_col", default = default)
}

#' @noRd
set_asp_col <- function(x = "asp") {
  options("papersize.asp_col" = x)
}

#' @noRd
get_body_col <- function(default = "body") {
  getOption("papersize.body_col", default = default)
}

#' @noRd
set_body_col <- function(x = "body") {
  options("papersize.body_col" = x)
}
