
# autogam() --------------

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
#' @import staccuracy
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

  # Create autogam S3 object
  ag <- list()
  class(ag) <- c('autogam')
  attr(ag, 'autogam_version') <- utils::packageVersion('autogam')

  ag$gam <- do.call(gam, args)

  # Calculate performance measures
  ag$perf <- list(
    mae = mae(data[[y_col]], ag$gam$fitted.values),
    win_mae = win_mae(data[[y_col]], ag$gam$fitted.values),
    sa_wmae_mad = sa_wmae_mad(data[[y_col]], ag$gam$fitted.values),
    rmse = rmse(data[[y_col]], ag$gam$fitted.values),
    win_rmse = win_rmse(data[[y_col]], ag$gam$fitted.values),
    sa_wrmse_sd = sa_wrmse_sd(data[[y_col]], ag$gam$fitted.values)
  )

  # Return arguments as params element
  ag$params <- args
  ag$params$data <- NULL  # too big for now
  ag$params$formula <- NULL  # already in ag$gam$formula

  ag$params$y_type <- ale:::var_type(data[[y_col]])

  return(ag)
}


# autogam methods ------------------------------

## Customized methods --------------------

#' Print Method for autogam Objects
#'
#' This function prints an `autogam` object. It calls the `mgcv::gam` object `print()` method and then adds basic performance metrics from the `autogam` object:
#' * For models that predict numeric outcomes, it prints "MAE", the mean absolute error, and "Std. accuracy", the standardized accuracy (staccuracy) of the winsorized MAE relative to the mean absolute deviation.
#' * For models that predict binary outcomes, it prints "AUC", the area under the ROC curve.
#'
#' @param x An object of class \code{autogam}.
#' @param ... Additional arguments passed to other methods.
#' @return Invisibly returns the input object \code{x}.
#' @export
#' @method print autogam
#'
print.autogam <- function(x, ...) {
  # Call mgcv:::print.gam
  mgcv:::print.gam(x$gam, ...)

  # Print basic performance measures
  if (x$params$y_type == 'numeric') {
    cat(
      '\n',
      'MAE: ', round(x$perf$mae, 3), '; ',
      'Std. accuracy: ', round(x$perf$sa_wmae_mad * 100, 1), '%',
      sep = ''
    )
  }
  else if (x$params$y_type == 'binary') {
    cat(
      '\n',
      'AUC: ', round(x$perf$auc, 3),
      sep = ''
    )
  }

  invisible(x)
}



#' Plot Method for autogam Objects
#'
#' This function plots an `autogam` object. It calls the `mgcv::gam` object [mgcv::plot.gam()] method.
#'
#' @param x An object of class \code{autogam}.
#' @param ... Additional arguments passed to other methods.
#' @return Same return object as [mgcv::print.gam()].
#' @export
#' @method plot autogam
#'
plot.autogam <- function(x, ...) {
  mgcv:::plot.gam(x$gam, ...)
}


#' Summary Method for autogam Objects
#'
#' This function returns a summary of an `autogam` object. It calls the `mgcv::gam` object [mgcv::summary.gam()] method.
#'
#' @param x An object of class \code{autogam}.
#' @param ... Additional arguments passed to other methods.
#' @return Same return object as [mgcv::summary.gam()].
#' @export
#' @method summary autogam
#'
summary.autogam <- function(x, ...) {
  mgcv:::summary.gam(x$gam, ...)
}



## Generic methods passed on to mgcv::gam methods ----------------

# methods(class = 'gam')

#' Generic autogam methods passed on to mgcv::gam methods
#'
#' An `autogam` object contains a `gam` element that is simply an `mgcv::gam` object. So, it supports all `mgcv::gam` methods by, in most cases, simply passing the `gam` element on to their corresponding `mgcv::gam` methods. Only the following methods have special specifications for autogam (see their dedicated documentation files for details): [print.autogam()].
#'
#' @name autogam generic methods
#' @rdname generic-method
#'
#' @param x An object of class \code{autogam}.
#' @param ... Additional arguments passed to other methods.
#' @return Returns the return object of the corresponding `mgcv::gam` method.
#' @export
#' @method anova autogam
#'
anova.autogam <- function(x, ...) {
  mgcv:::anova.gam(x$gam, ...)
}


#' @rdname generic-method
#' @export
#' @method cooks.distance autogam
cooks.distance.autogam <- function(x, ...) {
  mgcv:::cooks.distance.gam(x$gam, ...)
}


#' @rdname generic-method
#' @export
#' @method formula autogam
formula.autogam <- function(x, ...) {
  mgcv:::formula.gam(x$gam, ...)
}


#' @rdname generic-method
#' @export
#' @method influence autogam
influence.autogam <- function(x, ...) {
  mgcv:::influence.gam(x$gam, ...)
}


#' @rdname generic-method
#' @export
#' @method logLik autogam
logLik.autogam <- function(x, ...) {
  mgcv:::logLik.gam(x$gam, ...)
}


#' @rdname generic-method
#' @export
#' @method model.matrix autogam
model.matrix.autogam <- function(x, ...) {
  mgcv:::model.matrix.gam(x$gam, ...)
}


#' @rdname generic-method
#' @export
#' @method predict autogam
predict.autogam <- function(x, ...) {
  mgcv:::predict.gam(x$gam, ...)
}


#' @rdname generic-method
#' @export
#' @method residuals autogam
residuals.autogam <- function(x, ...) {
  mgcv:::residuals.gam(x$gam, ...)
}


#' @rdname generic-method
#' @export
#' @method vcov autogam
vcov.autogam <- function(x, ...) {
  mgcv:::vcov.gam(x$gam, ...)
}

