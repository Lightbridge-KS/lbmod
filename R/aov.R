# Compare means -----------------------------------------------------------

# if levels = 2 -> t.test
# if levels ≥ 3
### non-normal = kruskal.test
### normal
###### homo variance = ANOVA, non-homo variance = Welch test

### require pkg: car

# Main Wrapper ------------------------------------------------------------


#' Automatically Compute Parametric or Non-parametric t-test or ANOVA
#'
#' Automatically perform t-test or ANOVA according to the number of levels of factors.
#' Assumptions were checked prior to choosing parametric or non-parametric methods.
#' All combination of factor and numeric variables in data.frame will be computed.
#'
#' @param df A data.frame that has factor and numeric variables
#' @param type (Character) If `type = "params"`: It will only compute shapiro and levene test
#' @param na.rm_type (Character) Specify how to remove `NA` values i.e., by rows ("rows") or columns ("cols").
#' @param norm_test (Character)
#' \itemize{
#'  \item \strong{"all"}: perform normality test on numeric variables: \code{shapiro.test(num_var)}.
#'  \item \strong{"res"}: perform normality test on residual of linear model: \code{shapiro.test(residual(lm(num_var ~ fct_var)))}.
#' }
#' @param p.adjust.methods (Character) P-value adjustment method pass to p.adjust()
#' @param quiet (logical) Give message about removed variables or not?
#'
#' @return A data.frame
#' @export
#' @examples
#' aov_auto(iris)
#'
aov_auto <- function(df,
                     type = c("all","params"), # type = "params" - will only compute shapiro and levene test
                     na.rm_type = c("cols","rows"), # Remove NA's by cols or rows?
                     norm_test = c("all", "res"),   # "all" perform shapiro.test(num_var) ;
                     # "res" perform shapiro.test(residual(lm(num_var ~ fct_var)))
                     p.adjust.methods = "bonferroni", # P-value adjustment pass to p.adjust()
                     quiet = F
) {

  # require(dplyr)
  # require(purrr)
  type <- match.arg(type)
  na.rm_type <- match.arg(na.rm_type)
  norm_test <- match.arg(norm_test)
  ### Find logical index of cols or rows that has NA's

  col_has_na_i <- df %>% purrr::map_lgl(~any(is.na(.x)))
  row_has_na_i <- vector("logical",nrow(df))

  for (i in seq_len(nrow(df))) {

    row_has_na_i[i] <- purrr::some(df[i, ], ~is.na(.x))

  }
  ### Remove NA's by rows or columns
  switch (na.rm_type,
          "cols" = {
            df_filt <- df[ ,!col_has_na_i]
            if(quiet == F && any(col_has_na_i)){
              message("Removed column that has NA's :")
              print_messages( names(purrr::keep(col_has_na_i, ~.x == T)))
            }
          },
          "rows" = {
            df_filt <- df[!row_has_na_i, ]
            if(quiet == F && any(col_has_na_i)) message("Removed ", nrow(df[row_has_na_i, ])," rows contained NA")
          },
          stop("arg `na.rm_type` must be 'cols' or 'rows'")
  )
  ### Select column names that is numeric or factor
  num_vars <- df_filt %>% purrr::keep(is.numeric) %>% names()
  fct_vars <- df_filt %>% purrr::keep(is.factor) %>% names()

  #### Call Fn
  switch (type,
          "all" = aov_cross(df, num_vars, fct_vars, norm_test = norm_test, p.adjust.methods = p.adjust.methods),
          "params" = aov_cross_params(df, num_vars, fct_vars, norm_test = norm_test),
          stop("arg `type` must be 'all' or 'params'")
  )

}

# AOV Cross ---------------------------------------------------------------


aov_cross <- function(df,
                      num_vars,
                      fct_vars,
                      norm_test = "all",
                      p.adjust.methods = "bonferroni"
) {

  # require(dplyr)
  # require(purrr)

  params_df <- aov_cross_params(df, num_vars, fct_vars, norm_test = norm_test)

  model <- vector("list", length = nrow(params_df))

  p.adj.met <- subset(p.adjust.methods, p.adjust.methods == p.adjust.methods)
  if(length(p.adj.met) == 0) stop("P-value adjust method is not valid")
  p.adj_nm <- glue::glue("p.value_adj_{p.adj.met}")

  for (i in seq_len(nrow(params_df))) {

    num <- params_df$num_vars[[i]]
    fct <-  params_df$fct_vars[[i]]
    fun <-  params_df$fun[[i]]
    args_nm <- params_df$args[[i]]
    args <- .args_l[[args_nm]]

    model[[i]] <- parse_to_formula_2(df, fun = !!fun,lhs = !!num, rhs = !!fct, args = args)

  }

  model %>%
    setNames(params_df$formula) %>%
    purrr::map(broom::tidy) %>%
    dplyr::bind_rows(.id = "formula") %>%
    dplyr::mutate(!!p.adj_nm := p.adjust(p.value, method = p.adj.met), .after = p.value) %>%
    dplyr::left_join(params_df, by = "formula") %>%
    dplyr::relocate(num_vars, fct_vars, formula, method, p.value)

}

.args_l <- list(var_equal = c(var.equal = T),
                var_not_equal = c(var.equal = F),
                none = NULL)

#iris %>% aov_cross(iris_num, iris_fct)


# Params of shapiro.test , levene test, fun ------------------------------------


aov_cross_params <- function(df, num_vars, fct_vars, norm_test = "all") {

  # require(dplyr)
  # require(purrr)

  vars_filt_l <- filter_num_fct_vars(df, num_vars, fct_vars)
  num_vars <- vars_filt_l$num_vars
  fct_vars <- vars_filt_l$fct_vars

  if(length(num_vars) == 0) stop("`num_vars` is length = 0")
  if(length(fct_vars) == 0) stop("`fct_vars` is length = 0")

  combi <- switch (norm_test,
                   "all" = { shapiro(df, num_vars) %>%
                       dplyr::left_join(df %>% levene(num_vars, fct_vars, exp_type = "all"), by = "num_vars")
                   },
                   "res" = {shapiro_lm_res(df, num_vars, fct_vars) %>%
                       dplyr::left_join(df %>% levene(num_vars, fct_vars, exp_type = "res"), by = "formula") },
                   stop("`norm_test` must be 'all' or 'res'")
  )

  ### Filter out num_vars ~ fct_vars combination that has number of numeric value at each group levels ≤ 3
  num_fct_group_i <- purrr::map2_lgl(combi$num_vars, combi$fct_vars,
                                     ~!is_any_group_less_than(df, .x, .y, 3))
  if( !all(num_fct_group_i)){
    message("filter out combination that has inadequate observation of each group: ");
    print_messages(combi$formula[!num_fct_group_i])
  }
  combi <- combi[num_fct_group_i, ]

  combi %>%
    dplyr::relocate(num_vars, fct_vars, formula) %>%
    dplyr::mutate(levels = purrr::map_dbl(combi$fct_vars, ~ get_fct_lvs(df, .x)), .after = fct_vars) %>%
    dplyr::mutate(fun = dplyr::case_when(
      levels == 2 ~ "t.test",
      (levels >= 3) & (shapiro_p.value <= 0.05) ~ "kruskal.test",
      (levels >= 3) & (shapiro_p.value > 0.05) ~ "oneway.test",
    )) %>%
    dplyr::mutate(
      args = dplyr::case_when(
        fun == "kruskal.test" ~ "none",
        levene_p.value <= 0.05 ~ "var_not_equal",
        levene_p.value > 0.05 ~ "var_equal"
      )
    )

}



# Filter variable ---------------------------------------------------------


filter_num_fct_vars <- function(df, num_vars, fct_vars) {

  # Filter numeric vars
  ### filter only num_vars that has length between 3 to 5000 ; to avoid error in shapiro.test()
  num_btw_i <- df %>%
    dplyr::select(tidyselect::all_of(num_vars)) %>%
    purrr::map(length) %>%
    purrr::map_lgl( ~dplyr::between(.x, 3, 5000))

  if( !all(num_btw_i)){
    message("filter out num_var that is not between 3 to 5000: ")
    print_messages(num_vars[!num_btw_i])
  }

  num_vars <- num_vars[num_btw_i]
  ### filter out num_vars that has all identical values ; to avoid error in shapiro.test()
  num_not_iden_i <- purrr::map_lgl(num_vars, ~(!is_num_values_identical(df, .x)))

  if( !all(num_not_iden_i)){
    message("filter out num that has all identical value: ")
    print_messages(num_vars[!num_not_iden_i])
  }
  num_vars <- num_vars[num_not_iden_i]

  # Filter factor vars
  ### filter only fct_vars that has ≥ 1 levels ; avoid error in oneway.test()
  fct_lvs_i <- df %>% dplyr::select(tidyselect::all_of(fct_vars)) %>% purrr::map_lgl(~length(levels(.x)) > 1)

  if( !all(fct_lvs_i)){
    message("filter out fct that has less than 2 levels: ")
    print_messages(fct_vars[!fct_lvs_i])
  }

  fct_vars <- fct_vars[fct_lvs_i]

  list(num_vars = num_vars, fct_vars = fct_vars)
}


# Logical tests -----------------------------------------------------------


### Test if var has n of count(var) less than ...

is_some_count_less_than <- function(df, var, value) {

  df %>%
    dplyr::count(.data[[var]]) %>% dplyr::pull(n) %>% purrr::some(~.x < value)

}

### Test if each combination of fct-num vars result in number of observation of each group
#### less than ....

is_any_group_less_than <- function(df, num_var, fct_var, value) {

  any(tapply(df[[num_var]], df[[fct_var]], length) < value)

}


### Test if all value of num_var is identical


is_num_values_identical <- function(df, num_var) {

  df %>%
    dplyr::pull(.data[[num_var]]) %>%
    purrr::when(
      min(. , na.rm = T) == max(., na.rm = T) ~ TRUE,
      ~ FALSE
    )
}

### Test if residual of lm(num_var ~ fct_var) identical

is_res_identical <- function(df, num_var, fct_var) {

  res <- parse_to_formula(df, lm, !!num_var, !!fct_var)$residuals %>% unname()
  res %>%
    purrr::when(
      min(. , na.rm = T) == max(., na.rm = T) ~ TRUE,
      ~ FALSE
    )

}

# Parse variable to formula -----------------------------------------------

parse_to_formula_2 <- function(df, fun, lhs, rhs, args = NULL) { # args as list pass to fun

  lhs <- dplyr::ensym(lhs)
  rhs <- dplyr::ensym(rhs)
  fun <- dplyr::ensym(fun)

  eval( dplyr::expr((!!fun)(!!lhs ~ !!rhs, data = df, !!!args)) )

}

parse_to_formula <- function(df, fun, lhs, rhs, ...) {


  lhs <- dplyr::ensym(lhs)
  rhs <- dplyr::ensym(rhs)
  fun <- dplyr::ensym(fun)
  dot <- dplyr::enexprs(...)

  eval( dplyr::expr((!!fun)(!!lhs ~ !!rhs, data = df, !!!dot)) )

}


# Get factor levels -------------------------------------------------------

get_fct_lvs <- function(df, fct_var) {

  fct_var <- dplyr::enquo(fct_var)
  fct_vcts <- dplyr::pull(df, !!fct_var)
  length(levels(fct_vcts))

}


# Expand variables to combination  --------------------------------------------------------


expand_vars <- function(df, num_vars, fct_vars) {

  vars <- purrr::cross_df(list(num_vars = num_vars, fct_vars = fct_vars)) %>%
    dplyr::mutate(formula = glue::glue("{num_vars} ~ {fct_vars}"))

  vars_i <- purrr::map2_lgl(vars$num_vars, vars$fct_vars ,~!is_res_identical(df, .x , .y))
  if( !all(vars_i)){
    message("filter out vars combination that has idential lm(residual): ")
    print_messages(vars$formula[!vars_i])
  }
  vars[vars_i, ]
}

# shapiro test (vectorized) -----------------------------------------------



shapiro <- function(df, num_vars){

  names(num_vars) <- num_vars
  shapiro_p.value <- num_vars %>% purrr::map_dbl(~shapiro.test(df[[.x]])$p.value )
  shapiro_statistic <- num_vars %>% purrr::map_dbl(~shapiro.test(df[[.x]])$statistic )

  p_df <- tibble::as_tibble_row( shapiro_p.value) %>% tidyr::gather(num_vars, shapiro_p.value)
  stat_df <- tibble::as_tibble_row( shapiro_statistic) %>% tidyr::gather(num_vars, shapiro_statistic)
  dplyr::full_join(stat_df, p_df, by = "num_vars")

}


# Shapiro test of residual of lm( num ~ fct ) ---------------------------------


shapiro_lm_res_s <- function(df, num_var, fct_var) {

  res <- parse_to_formula(df, lm, !!num_var, !!fct_var)$residuals
  shapiro.test(res)

}

shapiro_lm_res <- function(df, num_vars, fct_vars) {



  vars_l <- filter_num_fct_vars(df, num_vars, fct_vars)

  vars <- expand_vars(df, vars_l$num_vars, vars_l$fct_vars)

  shapiro_res_p.value <- purrr::map2(vars$num_vars, vars$fct_vars,
                                     ~shapiro_lm_res_s(df, .x, .y)[["p.value"]])
  shapiro_res_statistic <- purrr::map2(vars$num_vars, vars$fct_vars,
                                       ~shapiro_lm_res_s(df, .x, .y)[["statistic"]] %>% unname())

  names(shapiro_res_p.value) <-  vars$formula
  names(shapiro_res_statistic) <-  vars$formula

  p_df <- tibble::as_tibble_row(shapiro_res_p.value) %>% tidyr::gather(formula, shapiro_p.value)
  stat_df <- tibble::as_tibble_row(shapiro_res_statistic) %>% tidyr::gather(formula, shapiro_statistic)
  dplyr::full_join(p_df, stat_df, by = "formula")

}
#iris %>% shapiro_lm_res_vec(iris_num, iris_fct)

#iris %>% shapiro(c("Sepal.Length","Petal.Width"))

# levene test (vectorized) -----------------------------------------------

levene <- function(df, num_vars, fct_vars, exp_type = "all") {

  # require(tibble)
  # require(dplyr)
  # require(purrr)
  # require(tidyr)

  vars_l <- filter_num_fct_vars(df, num_vars, fct_vars)
  vars <- switch (exp_type,
                  "all" = {purrr::cross_df(list(num_vars = num_vars, fct_vars = fct_vars)) %>%
                      dplyr::mutate(formula = glue::glue("{num_vars} ~ {fct_vars}"))
                  },
                  "res" = {expand_vars(df, vars_l$num_vars, vars_l$fct_vars)},
                  stop("`exp_type` must be 'all' or 'res'")
  )

  levene_p.value <- purrr::map2(vars$num_vars, vars$fct_vars,
                                ~levene_s(df,!!.x, !!.y)[["Pr(>F)"]][1]) %>% setNames(vars$formula)
  levene_F.value <- purrr::map2(vars$num_vars, vars$fct_vars,
                                ~levene_s(df,!!.x, !!.y)[["F value"]][1]) %>% setNames(vars$formula)


  p_df <- tibble::as_tibble_row(levene_p.value) %>% tidyr::gather(formula, levene_p.value)
  f_df <- tibble::as_tibble_row(levene_F.value) %>% tidyr::gather(formula, levene_F.value)

  vars %>%
    dplyr::left_join(p_df, by = "formula") %>%
    dplyr::left_join(f_df, by = "formula")


}

levene_s <- function(df, num_var, fct_var, ...) {


  num_var <- dplyr::ensym(num_var)
  fct_var <- dplyr::ensym(fct_var)

  dplyr::expr(car::leveneTest(!!num_var ~ !!fct_var, data = df, ...)) %>% eval()

}

#iris %>% levene(c("Sepal.Length","Petal.Width"), "Species")


