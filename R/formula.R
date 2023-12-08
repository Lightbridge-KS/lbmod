#' Parse Strings to LHS and RHS of formula
#'
#' Most generalized approach to parse formula
#'
#' @param fun Function to call
#' @param lhs (Symbol) left-hand sided of the formula
#' @param rhs (Symbol) right-hand sided of the formula
#' @param args A list of arguments to `fun`
#' @param pkg Package to call `fun`
#'
#' @return object as return by `fun`
#' @export
#'
#' @examples
#' parse_to_formula_2s(fun = "lm", "Sepal.Length", "Species", pkg = "stats", args = list(data = iris))
parse_to_formula_2s <- function(fun = "lm", lhs, rhs,
                                args = list(),
                                pkg = "stats") {

  lhs <- rlang::ensym(lhs)
  rhs <- rlang::ensym(rhs)
  form2 <- rlang::new_formula(lhs, rhs)
  call <- rlang::call2(fun, form2, .ns = pkg, !!!args)
  eval(call)

}


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



parse_to_formula_2 <- function(df, fun, lhs, rhs, args = NULL) { # args as list pass to fun

  lhs <- dplyr::ensym(lhs)
  rhs <- dplyr::ensym(rhs)
  fun <- dplyr::ensym(fun)

  eval( dplyr::expr((!!fun)(!!lhs ~ !!rhs, data = df, !!!args)) )

}
