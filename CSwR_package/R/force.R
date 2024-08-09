#' Force evaluation of all arguments to a function
#'
#' Forces the evaluation of all arguments in the calling
#' environment of this function.
#'
#' Use `force_all()` as syntactic sugar to force evaluation of
#' all arguments to a function and thereby circumvent lazy evaluation.
#' If called from within a function with `...` as formal argument,
#' use `force_all(...)` to force evaluation of arguments captured
#' by `...`.
#'
#' @param ... arguments captured by ... are also evaluated if passed via ...
#'
#' @return NULL invisibly
#' @seealso [force()]
#' @export
#'
#' @examples
#'
#' affine <- function(a, b) {
#'   function(x) a * x + b
#' }
#' a <- 1
#' b <- 1
#' affine_11 <- affine(a, b)
#' a <- 2
#' b <- 2
#' affine_11(1) # Gives 4 and not 2 due to lazy evaluation
#'
#' affine_forced <- function(a, b) {
#'   force_all()
#'   function(x) a * x + b
#' }
#' a <- 1
#' b <- 1
#' affine_11 <- affine_forced(a, b)
#' a <- 2
#' b <- 2
#' affine_11(1) # Gives 2
#'
force_all <- function(...) {
  as.list(parent.frame())
  list(...)
  invisible(NULL)
}
