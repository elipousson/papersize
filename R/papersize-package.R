#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @import rlang
#' @importFrom cli cli_abort cli_warn
#' @importFrom glue glue
#' @importFrom lifecycle deprecated
#' @importFrom rlang on_load local_use_cli run_on_load caller_arg %||%
## usethis namespace: end
rlang::on_load(rlang::local_use_cli())
