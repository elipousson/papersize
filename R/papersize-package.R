#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @import rlang
#' @importFrom cli cli_abort cli_warn
#' @importFrom glue glue
#' @importFrom lifecycle deprecated
#' @importFrom rlang on_load
#' @importFrom rlang local_use_cli
#' @importFrom rlang run_on_load
#' @importFrom rlang caller_arg
#' @importFrom rlang `%||%`
#' @importFrom rlang has_name
#' @importFrom rlang has_length
#' @importFrom rlang check_installed
## usethis namespace: end
rlang::on_load(rlang::local_use_cli())
