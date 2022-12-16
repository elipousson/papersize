.onLoad <- function(lib, pkg) {
  rlang::run_on_load()

  utils::data(
    list = c(
      "paper_sizes"
    ),
    package = pkg,
    envir = parent.env(environment())
  )
}

utils::globalVariables(
  c(
    "height", "label", "width", "x", "y"
  )
)


# @staticimports pkg:staticimports
#  map map_chr

# @staticimports pkg:stringstatic
#  str_detect

# @staticimports pkg:isstatic
#  is_unit is_gg

#' Are x and y the same grid unit value?
#'
#' @noRd
#' @importFrom grid unit unitType
gg_is_same_unit <- function(x, y) {
  if (is.character(x)) {
    x <- grid::unit(1, x)
  }

  if (is.character(y)) {
    y <- grid::unit(1, y)
  }

  grid::unitType(x) == grid::unitType(y)
}

#' @noRd
#' @importFrom grid unit convertUnit
gg_convert_unit <- function(x, from = NULL, to = NULL, type_from = "dimension", value_only = FALSE, ...) {
  if (is.numeric(x) && is.character(from)) {
    x <- grid::unit(x, from)
  }

  stopifnot(
    is_unit(x)
  )

  grid::convertUnit(x, unitTo = to, valueOnly = value_only, typeFrom = type_from, ...)
}

#' Helper to apply an additional ggproto object to each item in a list of ggplot
#' plots
#'
#' @noRd
map_gg <- function(.x, gg = NULL) {
  map(
    .x,
    function(x) {
      x + gg
    }
  )
}

#' Helper to passed family as fixed aesthetic conditionally
#'
#' @noRd
geom_text_if_family <- function(
    data,
    mapping =  ggplot2::aes(x = x, y = y, label = label),
    family = NULL,
    ...) {
  if (!is.null(family)) {
    ggplot2::geom_text(
      data = data,
      mapping = mapping,
      family = family, ...
    )
  } else {
    ggplot2::geom_text(
      data = data,
      ggplot2::aes(x = x, y = y, label = label),
      ...
    )
  }
}

