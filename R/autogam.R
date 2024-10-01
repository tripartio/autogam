#' Automate the creation of a Generalized Additive Model (GAM)
#'
#' `autogam()` is a wrapper for 'mgcv::gam()' that makes it easier to create high-performing Generalized Additive Models (GAMs). By entering just a dataset and the name of the outcome column as inputs, `autogam()` tries to automate the procedure of configuring a highly accurate GAM which performs at reasonably high speed, even for large datasets.
#'
#'
#' @export
#'
#' @param data dataframe. All the variables in `data` will be used to predict `y_col`. To exclude any variables, assign as `data` only the subset of variables desired.
#' @param y_col character(1). Name of the y outcome variable.
#' @param ... Arguments passed on to [mgcv::gam()].
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
  y_col,
  ...
) {
  args <- list(...)

  # Explicitly assign data to the arguments list. Note that data cannot be overridden because it is a named input to autogam()
  args$data <- data

  # Create a default smooth formula if the user doesn't specify the GAM formula (which is probably most of the time)
  if (is.null(args$formula)) {
    args$formula <- smooth_formula_string(data, y_col) |>
      stats::as.formula()
  }

  ag <- do.call(gam, args)

  return(ag)
}
