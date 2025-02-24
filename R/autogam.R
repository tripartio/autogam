
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
#' @param bs character(1). The default basis function for GAM smooths. See `?mgcv::smooth.terms` for details. Whereas the default `bs` in `mgcv` is 'tp', `autogam`'s default is 'cr', which is much faster and comparably accurate.
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
  ...,
  bs = 'cr'
) {
  # Preliminaries ---------------

  ## Validate arguments -----------------------
  # Only directly validate autogam() arguments; mgcv::gam will validate ... arguments

  ## Populate args for the mgcv::gam call ----------------------
  args <- list(...)
  y_vals <- data[[y_col]]
  y_type <- var_type(y_vals)

  # Explicitly assign data to the arguments list. Note that data cannot be overridden because it is a named input to autogam()
  args$data <- data

  # Use REML as the default method
  if (is.null(args$method)) {
    args$method <- 'REML'
  }


  # Construct optimal GAM settings --------------

  ## Determine y_col distribution -----------------------

  # Initialize distribution for final params
  y_dist <- NULL   # single final distribution used
  y_dists <- NULL  # possible distributions

  if (is.null(args$family)) {
    cli_inform('Detecting distribution of {.var {y_col}}...')
    y_dists <- univariateML::model_select(
      y_vals,
      models = uml_models[[y_type]],
      return = 'all'
    )
  }


  ## Choose gam or bam ----------------------

  # Default to bam
  gam_fun <- bam
  args$discrete <- TRUE
  args$method <- 'fREML'

  ## Detect interactions ---------------------------


  ## Create smooth formula ----------------------

  ## Force smooth basis s(bs='cr'), which is much faster than the default tp and seems to be equally accurate.
  ## Since gam and bam do not expose bs as a default parameter, the only acceptable way to do this is to force smooth_formula_string to add bs='cr' as a default. So, I need to set bs='cr' as a top-level option for autogam.
  ## This is probably worth a request to Simon Wood to add the parameter option.
  ## It is not acceptable for me to override mgcv::s() to establish my own default because this would render the autogam$gam object inconsistent with mgcv::gam objects.

  # Create a default smooth formula if the user doesn't specify the GAM formula (which is probably most of the time)
  if (is.null(args$formula)) {
    args$formula <- smooth_formula_string(data, y_col, bs = bs) |>
      stats::as.formula()
  }



  # Execute the GAM call --------------------------

  # Create autogam S3 object
  ag <- list()
  class(ag) <- c('autogam')
  attr(ag, 'autogam_version') <- utils::packageVersion('autogam')

  if (!is.null(args$family) || !inherits(y_dists, 'tbl_df')) {
    # if (!is.null(args$family)) {
    # The user specified the family to fit
    ag$gam <- do.call(gam_fun, args)
  }
  else {
    # Try the auto-detected best distribution fits

    # Some family links with mgcv are more unstable than others, so iterate through each from the best fit to the worst until the fit successfully runs.
    ## But family problems are not the only reasons why the gam call fails. Optimizers are also sometimes a problem. An advanced upgrade will try to detect if the problem is the optimizer and then switcht based on that.
    ## See https://stat.ethz.ch/R-manual/R-devel/library/mgcv/html/gam.convergence.html
    for (it.yd in y_dists$univariateML) {
      tryCatch(
        {
          it.yd_model <- attr(it.yd, 'model')
          args$family <- uml_to_family_links |>
            (`[[`)(y_type) |>
            (`[[`)(it.yd_model)

          cli_inform('Fitting GAM with {.var {it.yd_model}} distribution...')
          ag$gam <- do.call(gam_fun, args)

          y_dist <- it.yd_model

          break  # the GAM fit worked
        },
        error = \(e) {
          # Immediately print a warning message
          cli_alert_warning(as.character(e))
          # Register a warning in the warning() system; usually printed when everything finishes executing.
          cli_warn(as.character(e))
        }
      )
    }
  }

  if (is.null(ag$gam)) {
    cli_abort('The GAM could not be fit.')
  }


  # Calculate performance measures ---------------------
  ag$perf <- if (y_type == 'numeric') {
    list(
      mae         = mae(y_vals, ag$gam$fitted.values),
      win_mae     = win_mae(y_vals, ag$gam$fitted.values),
      sa_wmae_mad = sa_wmae_mad(y_vals, ag$gam$fitted.values),
      rmse        = rmse(y_vals, ag$gam$fitted.values),
      win_rmse    = win_rmse(y_vals, ag$gam$fitted.values),
      sa_wrmse_sd = sa_wrmse_sd(y_vals, ag$gam$fitted.values)
    )
  }
  else if (y_type == 'binary') {
    list(
      auc = aucroc(y_vals, ag$gam$fitted.values)$auc
    )
  }

  # Print success message. Important because possible warnings might obscure the main point.
  cat('\n')
  cli_alert_success(stringr::str_glue(
    'GAM successfully fit with {round(ag$perf$sa_wmae_mad * 100, 1)}% standardized accuracy.'
  ))
  cat('\n')

  # Return arguments as params element -----------------------
  ag$params <- args
  ag$params$data <- NULL  # already in ag$gam$model
  ag$params$formula <- NULL  # already in ag$gam$formula

  ag$params$y_type  <- y_type
  ag$params$y_dist  <- y_dist   # Single distribution that was used
  ag$params$y_dists <- y_dists  # Only valid if multiple distributions were evaluated

  return(ag)
}


# Distribution variables ---------------

# ml*** models to call from univariateML that correspond to mgcv::gam
uml_models <- list(
  binary = c(
    'cauchy', 'exp', 'gumbel', 'logis', 'norm'
  ),
  numeric = c(
    'norm', 'lnorm', 'exp', 'invgauss',
    'gamma', 'invgamma', 'lgamma',
    # 'std',  # buggy
    # 'llogis',
    'beta'
    # # discrete
    # 'pois'
  )
)


# mapping from univariateML distribution names to families and links compatible with mgcv::gam
uml_to_family_links <- list(
  binary = list(
    Cauchy = stats::binomial(link = 'cauchit'),  # cauchy
    Exponential = stats::binomial(link = 'log'),  # exp
    Gumbel = stats::binomial(link = 'cloglog'),  # gumbel
    Logistic = stats::binomial(link = 'logit'),
    Normal = stats::binomial(link = 'probit')
  ),
  numeric = list(
    # Gaussian links (normally distributed data)
    Normal = stats::gaussian(link = 'identity'),
    `Log-normal` = stats::gaussian(link = 'log'),
    Exponential = stats::gaussian(link = 'log'),
    `Inverse Gaussian` = stats::gaussian(link = 'inverse'),
    # # Inverse Gaussian
    # # But univariateML has nothing corresponding to the 1/mu^2, log or inverse links. (What does an inverse link of an inverse distribution even mean???)
    # `Inverse Gaussian` = stats::inverse.gaussian(link = 'identity')

    # Gamma distribution links
    Gamma = stats::Gamma(link = 'identity'),
    `Inverse gamma` = stats::Gamma(link = 'inverse'),
    `Log-gamma` = stats::Gamma(link = 'log'),

    # Advanced mgcv distributions
    # `Student-t` = mgcv::scat(),  # Use default identity link  # buggy
    Beta = mgcv::betar(),  # Use default logit link

    # Poisson links (count data)
    # But univariateML has nothing corresponding to the log or sqrt Poisson links
    Poisson = stats::poisson(link = 'identity')
    # mgcv Beta distribution
    # mgcv scaled t distribution
  )
)


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
  mgcv::print.gam(x$gam, ...)

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
  mgcv::plot.gam(x$gam, ...)
}


#' Summary Method for autogam Objects
#'
#' This function returns a summary of an `autogam` object. It calls the `mgcv::gam` object [mgcv::summary.gam()] method.
#'
#' @param object An object of class \code{autogam}.
#' @param ... Additional arguments passed to other methods.
#' @return Same return object as [mgcv::summary.gam()].
#' @export
#' @method summary autogam
#'
summary.autogam <- function(object, ...) {
  mgcv::summary.gam(object$gam, ...)
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
#' @param object,model An object of class \code{autogam}.
#' @param ... Additional arguments passed to other methods.
#' @return Returns the return object of the corresponding `mgcv::gam` method.
#' @export
#' @method anova autogam
#'
anova.autogam <- function(object, ...) {
  mgcv::anova.gam(object$gam, ...)
}


#' @rdname generic-method
#' @export
#' @method coef autogam
coef.autogam <- function(object, ...) {
  stats::coef(object$gam, ...)
}


#' @rdname generic-method
# # I don't know why CHECK bugs out if this method is exported; it works otherwise
# @export
#' @method cooks.distance autogam
cooks.distance.autogam <- function(model, ...) {
  # For some reason this only works when called from stats, not mgcv
  stats::cooks.distance(model$gam, ...)
}


#' @rdname generic-method
#'
#' @param x formula
#' @param ... other arguments
#'
#' @export
#' @method formula autogam
formula.autogam <- function(x, ...) {
  mgcv::formula.gam(x$gam, ...)
}


#' @rdname generic-method
# # I don't know why CHECK bugs out if this method is exported; it works otherwise
# @export
#' @method influence autogam
influence.autogam <- function(model, ...) {
  mgcv::influence.gam(model$gam, ...)
}


#' @rdname generic-method
#' @export
#' @method logLik autogam
logLik.autogam <- function(object, ...) {
  mgcv::logLik.gam(object$gam, ...)
}


#' @rdname generic-method
#' @export
#' @method model.matrix autogam
model.matrix.autogam <- function(object, ...) {
  mgcv::model.matrix.gam(object$gam, ...)
}


#' @rdname generic-method
#' @export
#' @method predict autogam
predict.autogam <- function(object, ...) {
  mgcv::predict.gam(object$gam, ...)
}


#' @rdname generic-method
#' @export
#' @method residuals autogam
residuals.autogam <- function(object, ...) {
  mgcv::residuals.gam(object$gam, ...)
}


#' @rdname generic-method
#' @export
#' @method vcov autogam
vcov.autogam <- function(object, ...) {
  mgcv::vcov.gam(object$gam, ...)
}

