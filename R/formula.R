#' Parse symbols to LHS and RHS of formula
#'
#' Most common use cases would be passing string to `lhr` and `rhs` of the formula.
#'
#'
#' @param fun Function that accept formula object as first argument
#' @param lhs (Quote or Unquoted) Expression for left-hand sided of the formula
#' @param rhs (Quote or Unquoted) Expression for right-hand sided of the formula
#' @param ... pass to `fun`
#'
#' @return object as return by `fun`
#' @export
#'
#' @examples
#' parse_to_formula(lm, "Sepal.Length", "Sepal.Width", data = iris)
parse_to_formula <- function(fun, lhs, rhs, ...) {

  lhs <- rlang::ensym(lhs)
  rhs <- rlang::ensym(rhs)
  fun <- rlang::ensym(fun)
  dot <- rlang::enexprs(...)

  eval(rlang::expr((!!fun)(!!lhs ~ !!rhs, !!!dot)))

}
