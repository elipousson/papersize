#' @noRd
get_units_col <- function(default = "units") {
  getOption("papersize.units_col", default = "units")
}

#' @noRd
set_units_col <- function(x = "units") {
  options("papersize.units_col" = x)
}

#' @noRd
get_orientation_col <- function(default = "orientation") {
  getOption("papersize.orientation_col", default = "orientation")
}

#' @noRd
set_orientation_col <- function(x = "orientation") {
  options("papersize.orientation_col" = x)
}
