

#' Multiple Pairs of Univariate Analysis (No Processing)
#'
#' Fitting multiple univariate models of one `y_var` on one or more `x_vars` using any `fun`.
#' The output is bare (no modified)
#'
#' @param data A data frame
#' @param fun (character) A modeling function
#' @param y_var (character) Outcome variable
#' @param x_vars (character) One or more predictor variable(s)
#' @param pkg (character) Package of `fun`
#' @param ... Passing to `fun`
#'
#' @return A list with names as `x_vars`
#' @export
#'
#' @examples
#' iris |> univar_multi_bare(fun = "lm", "Sepal.Length", "Species")
#'
univar_multi_bare <- function(data, fun = "lm", y_var, x_vars,
                              pkg = "stats", ...) {

  dot <- rlang::list2(...)
  setNames(x_vars, x_vars) |>
    purrr::map(
      ~ parse_to_formula_2s(fun, lhs = !!y_var, rhs = !!.x, pkg = pkg,
                            args = rlang::list2(data = data, !!!dot))
    )

}

# Multi Univar with Tidy --------------------------------------------------



#' Multiple Pairs of Univariate Analysis with Tidied Output
#'
#' Fitting multiple univariate models of one `y_var` on one or more `x_vars` using any `fun`.
#' The output will be `broom::tidy`.
#'
#' @param data A data frame
#' @param fun (character) A modeling function
#' @param y_var (character) Outcome variable
#' @param x_vars (character) One or more predictor variable(s)
#' @param pkg (character) Package of `fun`
#' @param ... Passing to `fun`
#'
#' @return A `broom::tidy` data frame
#' @export
#'
#' @examples
#' iris |> univar_multi_tidy(fun = "lm", "Sepal.Length", c("Species", "Petal.Width"))
#'
univar_multi_tidy <- function(data, fun = "lm", y_var, x_vars,
                              pkg = "stats", ...) {

  dot <- rlang::list2(...)
  setNames(x_vars, x_vars) |>
    purrr::map(
      ~ parse_to_formula_2s(fun, lhs = !!y_var, rhs = !!.x, pkg = pkg,
                            args = rlang::list2(data = data, !!!dot))
    ) |>
    purrr::map_dfr(broom::tidy, .id = "x_vars") |>
    dplyr::filter(term != "(Intercept)")

}


# Multi Univar with glance ------------------------------------------------



#' Multiple Pairs of Univariate Analysis with Glanced Output
#'
#' Fitting multiple univariate models of one `y_var` on one or more `x_vars` using any `fun`.
#' The output will be `broom::glance`.
#'
#' @param data A data frame
#' @param fun (character) A modeling function
#' @param y_var (character) Outcome variable
#' @param x_vars (character) One or more predictor variable(s)
#' @param pkg (character) Package of `fun`
#' @param ... Passing to `fun`
#'
#' @return A `broom::glance` data frame
#' @export
#'
#' @examples
#' iris |> univar_multi_glance(fun = "lm", "Sepal.Length", c("Species", "Petal.Width"))
#'
univar_multi_glance <- function(data, fun = "lm", y_var, x_vars,
                                pkg = "stats", ...) {

  dot <- rlang::list2(...)
  setNames(x_vars, x_vars) |>
    purrr::map(
      ~ parse_to_formula_2s(fun, lhs = !!y_var, rhs = !!.x, pkg = pkg,
                            args = rlang::list2(data = data, !!!dot))
    ) |>
    purrr::map_dfr(broom::glance, .id = "x_vars")

}
