
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
#' @param binary_true_value any single atomic value. The value of `actual` that is considered `TRUE`; any other value of `actual` is considered `FALSE`. For example, if `2` means `TRUE` and `1` means `FALSE`, then set `binary_true_value = 2`.
# @param ale,ale_options logical(1),list. If `ale` is TRUE, the returned autogam object includes accumulated local effects (ALE) results from `ale::ale()`. By default (if `ale_options = NULL`), the `x_cols` argument for `ale::ale()` will be set to all predictor terms and interactions used in the formula that `autogam()` creates. `ale_options` is a named list of arguments passed to `ale::ale()`. `x_cols` can be specified there to override the default `autogam()` setting.
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
  # ale,
  # ale_options,
  bs = 'cr',
  binary_true_value = NULL
) {
  # Preliminaries ---------------

  ## Validate arguments -----------------------
  # Only directly validate autogam() arguments; mgcv::gam will validate ... arguments

  # Validate the dataset
  validate(data |> inherits('data.frame'))
  validate(
    !any(is.na(data)),
    msg = '{.arg data} must not have any missing values.'
  )



  # Validate inputs
  validate(is.null(binary_true_value) || rlang::is_scalar_atomic(binary_true_value))


  ## Populate args for the mgcv::gam call ----------------------
  args <- list(...)
  y_vals <- data[[y_col]]
  y_type <- var_type(y_vals)

  if (y_type %notin% c('numeric', 'binary')) {
    cli_abort('For now, only numeric and binary y outcomes are supported.')
  }

  if (y_type == 'binary') {
    if (!is.null(binary_true_value)) {
      # If binary_true_value is provided, then it overrides any other values of y_vals
      y_vals <- y_vals == binary_true_value
    }
    else {
      # Coerce actual to binary using the standard as.logical() rules
      y_vals <- as.logical(y_vals)
      if (sum(is.na(y_vals)) > 0) {
        cli_abort('Some "{y_col}" values are not valid binary values.')
      }
    }

  }




  # Explicitly assign data to the arguments list. Note that data cannot be overridden because it is a named input to autogam()
  args$data <- data

  # Use REML as the default method; it's a bit slower but often more accurate
  if (is.null(args$method)) {
    args$method <- 'REML'
  }


  # Construct optimal GAM settings --------------

  ## Determine y_col distribution -----------------------

  y_dists <- NULL  # needed for final params
  if (is.null(args$family)) {
    cli_inform('Detecting distribution of {.var {y_col}}...')

    # Coerce y_vals to a numeric type to determine its distribution.
    num_y_vals <- if (y_type == 'numeric') {
      y_vals
    }
    else if (y_type == 'binary') {
      if (!(is.numeric(y_vals))) {
      # if (!(is.numeric(y_vals) || is.logical(y_vals))) {
          # Coerce y_vals to a numeric binary format
        y_vals_factor <- factor(y_vals)
        y_vals_levels <- levels(y_vals_factor)

        y_vals_factor |>
          as.integer() |>  # binary factors become 1 and 2, so...
          (`-`)(1)  # subtract 1 to convert values to 0 and 1
      }
    }


    tryCatch(
      {
        y_dists <- univariateML::model_select(
          num_y_vals,
          models = uml_models[[y_type]],
          return = 'all'
        )
      },
      error = \(e) {
        cli_alert_danger(e)
      }
    )
  }





  ## Choose gam or bam ----------------------

  # Default to bam
  gam_fun <- bam
  # args$discrete <- TRUE
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

  if (!is.null(args$family)) {
    # The user specified the family to fit
    tryCatch(
      {
        ag$gam <- do.call(gam_fun, args)
      },
      error = \(e) {
        # Immediately print a warning message
        cli_alert_warning('Warning: ')
        print(e)
        warning(e)
        # cli_warn(as.character(e))
      }
    )

    y_dist <- NULL
  }
  else {
    # Try the auto-detected best distribution fits

    # Some family links with mgcv are more unstable than others, so iterate through each from the best fit to the worst until the fit successfully runs.
    ## But family problems are not the only reasons why the gam call fails. Optimizers are also sometimes a problem. An advanced upgrade will try to detect if the problem is the optimizer and then switch based on that.
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
          cli_alert_warning('Warning: ')
          print(e)
          warning(e)
          # cli_warn(as.character(e))
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
    'beta',
    # # discrete
    'pois'
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
#' @param x,model An object of class \code{autogam}.
#' @param ... Additional arguments passed to other methods.
#' @return Returns the return object of the corresponding `mgcv::gam` method.
#' @export
#' @method anova autogam
#'
anova.autogam <- function(x, ...) {
  mgcv::anova.gam(x$gam, ...)
}


#' @rdname generic-method
#' @export
#' @method coef autogam
coef.autogam <- function(x, ...) {
  stats::coef(x$gam, ...)
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
logLik.autogam <- function(x, ...) {
  mgcv::logLik.gam(x$gam, ...)
}


#' @rdname generic-method
#' @export
#' @method model.matrix autogam
model.matrix.autogam <- function(x, ...) {
  mgcv::model.matrix.gam(x$gam, ...)
}


#' @rdname generic-method
#' @export
#' @method predict autogam
predict.autogam <- function(x, ...) {
  mgcv::predict.gam(x$gam, ...)
}


#' @rdname generic-method
#' @export
#' @method residuals autogam
residuals.autogam <- function(x, ...) {
  mgcv::residuals.gam(x$gam, ...)
}


#' @rdname generic-method
#' @export
#' @method vcov autogam
vcov.autogam <- function(x, ...) {
  mgcv::vcov.gam(x$gam, ...)
}

