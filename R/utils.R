.onLoad <- function(lib, pkg) {
  rlang::run_on_load()

  utils::data(
    list = c(
      "paper_sizes",
      "dist_unit_options",
      "area_unit_options",
      "grid_units",
      "page_extras",
      "standard_scales"
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
#  str_detect str_extract str_remove

# @staticimports pkg:isstatic
#  is_unit is_gg is_units is_sf is_sf_ext is_patchwork as_orientation
#  str_add_fileext str_remove_fileext str_extract_fileext

#' @noRd
#' @importFrom rlang caller_arg
filter_data <- function(x, y = NULL, col = rlang::caller_arg(y), ignore.case = TRUE) {
  if (is.null(y)) {
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

#' Helper to passed family as fixed aesthetic conditionally
#'
#' https://github.com/yonicd/bplyr/blob/master/R/mutate.R
#'
#' @author Jonathan Sidi
#' @source bplyr package
#' @noRd
#' @importFrom rlang quos quo_squash
mutate_data <- function(.data, ...) {
  FNS <- lapply(rlang::quos(...), rlang::quo_squash)

  EXPRS <- lapply(names(FNS), function(x) {
    sprintf("%s <- %s", x, deparse(FNS[[x]]))
  })

  within(
    .data,
    eval(
      parse(text = paste0(unlist(EXPRS), collapse = "\n"))
    )
  )
}

#' @noRd
sum_num <- function(x) {
  sum(as.numeric(x))
}

#' @noRd
diff_num <- function(x) {
  diff(as.numeric(x))
}

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
  rlang::check_installed("ggplot2")

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
