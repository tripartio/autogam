#' Automate the creation of a Generalized Additive Model (GAM)
#'
#' This is a wrapper package for `mgcv` that makes it easier to create high-performing Generalized Additive Models (GAMs). By entering just a dataset and the name of the outcome column as inputs, `autogam()` tries to automate as much as possible the procedure of configuring a highly accurate GAM at reasonably high speed, even for large datasets.
#'
#' @export
#'
#' @param data dataframe. All the variables in `data` will be used to predict `y_col`. To exclude any variables, assign as `data` only the subset of variables desired.
#' @param y_col character(1). Name of the y outcome variable.
#'
#' @returns Returns an `mgcv::gam` object, the result of predicting `y_col` from all other variables in `data`.
#'
#' @examples
#' autogam(mtcars, 'mpg')
#'
#' @import mgcv
#'
autogam <- function(
  data,
  y_col
) {
  fmla <- smooth_formula_string(data, y_col) |>
    stats::as.formula()

  ag <- gam(
    formula = fmla,
    data = data
  )

  return(ag)
}
