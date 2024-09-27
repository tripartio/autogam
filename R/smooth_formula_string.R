#' Create a character string for a mgcv::gam formula
#'
#' Create a character string that wraps appropriate variables in a dataframe with `s()` smooth functions. Based on the datatype of each variable, it determines whether it is a numeric variable to be smoothed:
#' * Non-numeric: no smoothing.
#' * Numeric: determine knots based on the number of unique values for that variable:
#'     * ⩽ 4: no smoothing
#'     * 5 to 19 (inclusive): smooth function with knots equal to the floored half of the number of unique values. E.g., 6 unique values receive 3 knots, 7 will receive 3 knots, and 8 will receive 4 knots.
#'     * ⩾ 20: smooth function with no specified number of knots, allowing the `gam()` function to detect the appropriate number.
#'
# TODO: In the end, rearrange terms in the order they occur in col_names regardless of their numeric status. This only applies if expand_parametric==TRUE.
#'
#' @export
#'
#' @param data dataframe. All the variables in `data` except `y_col` will be listed in the resulting formula string. To exclude any variables, assign as `data` only the subset of variables desired.
#' @param y_col character(1). Name of the y outcome variable.
#' @param smooth_fun character(1). Function to use for smooth wraps; default is 's' for the `s()` function.
#' @param expand_parametric logical(1). If `TRUE` (default), explicitly list each non-smooth (parametric) term. If `FALSE`, use `.` to lump together all non-smooth terms.
#'
#' @returns Returns a single character string that represents a formula with `y_col` on the left and all other variables in `data` on the right, each formatted with an appropriate `s()` function when applicable.
#'
#' @examples
#' smooth_formula_string(mtcars, 'mpg')
#'
#' @import dplyr
#' @importFrom purrr map_chr
#'
smooth_formula_string <- function(
  data,
  y_col,
  smooth_fun = 's',
  expand_parametric = TRUE
) {
  col_names <- names(data)

  # Ensure y_col is a column in data
  if (!y_col %in% col_names) {
    ##TODO: use cli and internal data validation
    stop("y_col not found in data")
  }

  # Identify numeric predictor columns
  num_cols <- data |>
    select(-all_of(y_col)) |>
    select(where(is.numeric)) |>
    names()

  # Construct the right-hand side of the formula with s() for numeric columns
  num_rhs <-
    num_cols |>
    map_chr(\(.col) {
      # Get number of unique numeric values in .col
      num_unique <- data[[.col]] |> unique() |> length()

      # For <20 unique values, do not attempt more than half of that knots or else chances of failure are higher (e.g., if the analysis bootstraps or otherwise samples the data).
      # For ⩽ 4 unique values, then don't even attempt smoothing; just leave the values distinct.
      term <- case_when(
        num_unique <= 4 ~
          .col,
        num_unique |> between(5, 19) ~
          stringr::str_glue('{smooth_fun}({.col},k={num_unique %/% 2})'),
        num_unique >= 20 ~
          stringr::str_glue('{smooth_fun}({.col})')  # gam will automatically determine k
      )

      term
    }) |>
    paste0(collapse = ' + ')

  non_num_rhs <-
    col_names |>
    setdiff(c(y_col, num_cols)) |>
    paste0(collapse = ' + ')

  # Construct the full formula string
  formula_str <- paste0(
    y_col, " ~ ",
    num_rhs,
    # Add non-numeric rhs if there is any
    ifelse(
      nchar(non_num_rhs) > 0,
      ifelse(
        expand_parametric,
        paste0(
          ifelse(nchar(num_rhs) > 0, ' + ', ''),
          paste0(non_num_rhs, collapse = ' + ')
        ),
        ' + .'
      ),
      ''
    )
  )

  return(formula_str)
}
