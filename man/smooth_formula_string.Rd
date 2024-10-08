% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/smooth_formula_string.R
\name{smooth_formula_string}
\alias{smooth_formula_string}
\title{Create a character string for a mgcv::gam formula}
\usage{
smooth_formula_string(data, y_col, smooth_fun = "s", expand_parametric = TRUE)
}
\arguments{
\item{data}{dataframe. All the variables in \code{data} except \code{y_col} will be listed in the resulting formula string. To exclude any variables, assign as \code{data} only the subset of variables desired.}

\item{y_col}{character(1). Name of the y outcome variable.}

\item{smooth_fun}{character(1). Function to use for smooth wraps; default is 's' for the \code{s()} function.}

\item{expand_parametric}{logical(1). If \code{TRUE} (default), explicitly list each non-smooth (parametric) term. If \code{FALSE}, use \code{.} to lump together all non-smooth terms.}
}
\value{
Returns a single character string that represents a formula with \code{y_col} on the left and all other variables in \code{data} on the right, each formatted with an appropriate \code{s()} function when applicable.
}
\description{
Create a character string that wraps appropriate variables in a dataframe with \code{s()} smooth functions. Based on the datatype of each variable, it determines whether it is a numeric variable to be smoothed:
\itemize{
\item Non-numeric: no smoothing.
\item Numeric: determine knots based on the number of unique values for that variable:
\itemize{
\item \verb{<= 4}: no smoothing
\item \verb{5 to 19} (inclusive): smooth function with knots equal to the floored half of the number of unique values. E.g., 6 unique values receive 3 knots, 7 will receive 3 knots, and 8 will receive 4 knots.
\item \verb{>= 20}: smooth function with no specified number of knots, allowing the \code{gam()} function to detect the appropriate number.
}
}
}
\examples{
smooth_formula_string(mtcars, 'mpg')

}
