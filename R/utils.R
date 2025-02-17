utils::globalVariables(
  c(
    "height", "label", "width", "x", "y"
  )
)

# @staticimports pkg:staticimports
#  map map_chr

# @staticimports pkg:stringstatic
#  str_detect str_extract str_remove str_pad

# @staticimports pkg:isstatic
#  is_unit is_gg is_units is_sf is_sf_ext is_patchwork as_orientation
#  str_add_fileext str_remove_fileext str_extract_fileext is_gg_list


#' Get the names of geoms from a ggplot
#'
#' Adapted from `ggcheck::get_geoms()`
#'
#' @noRd
get_geoms <- function(p) {
  vapply(
    seq_along(p$layers),
    function(x) {
      class(p$layers[[x]]$geom)[1]
    },
    character(1)
  )
}

#' Does this plot use a raster annotation?
#'
#' Adapted from ggcheck::get_geoms
#'
#' @noRd
has_annotation <- function(p) {
  any("GeomRasterAnn" %in% get_geoms(p))
}

#' @noRd
#' @importFrom rlang caller_arg
filter_data <- function(x, y = NULL, col = rlang::caller_arg(y), ignore.case = TRUE) {
  if (is_null(y)) {
    return(x)
  }

  xcol <- x[[col]]
  if (ignore.case) {
    xcol <- tolower(xcol)
    y <- tolower(y)
  }

  x[xcol %in% y, ]
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

#' @noRd
sum_num <- function(x) {
  sum(as.numeric(x))
}

# diff_num <- function(x) {
#   diff(as.numeric(x))
# }

#' Helper to passed family as fixed aesthetic conditionally
#'
#' @noRd
geom_text_if_family <- function(data,
                                mapping = ggplot2::aes(
                                  x = x,
                                  y = y,
                                  label = label
                                ),
                                family = NULL,
                                ...) {
  check_installed("ggplot2")

  if (!is_null(family)) {
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


#' Modify function parameters
#'
#' @noRd
#' @importFrom rlang fn_fmls is_missing list2
#' @importFrom utils modifyList
modify_fn_fmls <- function(params,
                           fn,
                           keep_missing = FALSE,
                           keep.null = FALSE,
                           ...) {
  fmls <- fn_fmls(fn)

  if (!keep_missing) {
    fmls <- discard(fmls, rlang::is_missing)
  }

  params <- c(list2(...), params)

  utils::modifyList(
    fmls,
    params,
    keep.null = keep.null
  )
}
