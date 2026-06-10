#' Swap x and y in a formula
#'
#' By default a formula of x on y is converted into a formula of y
#' on x, while the reverse swap is done only if \code{backward = TRUE}.
#'
#' @param f formula An R model formula
#' @param backwards logical If \code{NULL} the swap is done irrespective of
#'   the variable in the lhs.
#'
#' @details If \code{backwards = TRUE}, a formula with \code{x} in the lhs is
#' always, returned. If \code{backwards = FALSE}, a formula with \code{y} in the
#' lhs is always, returned. If \code{backwards = NULL} \code{x} and \code{y}
#' are always swapped.
#'
#' This function is meant to be used only as a helper within 'ggplot2'
#' statistics. Normally together with geometries supporting orientation when we
#' want to automate the change in orientation based on a user-supplied formula.
#' Only \code{x} and \code{y} are exchanged, and in other respects the formula
#' is rebuilt copying the environment from \code{f}.
#'
#' @return A copy of \code{f} with \code{x} and \code{y} swapped by each other
#'   in the lhs and rhs.
#'
swap_xy <- function(f, backwards = FALSE) {
  f.chr <- as.character(f)
  if (is.null(backwards)) {
    backwards <- grepl("y", f.chr[2])
  }
  if (backwards) {
    # lhs
    f.chr[2] <- gsub("\\by\\b", "x", f.chr[2])
    # rhs
    f.chr[-c(1, 2)] <- gsub("\\bx\\b", "y", f.chr[-c(1, 2)])
  } else {
    # lhs
    f.chr[2] <- gsub("\\bx\\b", "y", f.chr[2])
    # rhs
    f.chr[-c(1, 2)] <- gsub("\\by\\b", "x", f.chr[-c(1, 2)])
  }
  # reassemble
  f.chr <- paste(f.chr[2], f.chr[3], sep = f.chr[1])
  # define new formula in the same environment as original
  stats::as.formula(f.chr, env = environment(f))
}

#' Guess the orientation from model formula
#'
#' @param orientation character \code{"x"} or \code{"y"}.
#' @param formula model formula based on x and y.
#' @param default.formula model formula to be used when argument passed to
#'   \code{formula} is \code{NULL} or \code{NA}.
#' @param formula.on.x logical Flip x and y in formula, used when the x
#'   and y in data are not flipped in the compute function.
#'
#' @return Named list with members formula and orientation.
#'
#' @details Set defaults for both \code{orientation} and \code{formula},
#'   ensuring consistency both when x and y and swapped in data and when they
#'   are not.
#'
#' @keywords internal
#'
guess_orientation <- function(orientation = NULL,
                              formula = NULL,
                              default.formula = y ~ x,
                              formula.on.x = FALSE) {
  if (is.null(formula) && is.null(default.formula)) {
    stop("'formula' missing with no default")
  }
  # as.character(formula)[2] is the lhs of the formula
  if (formula.on.x) {
    # if we flip x and y in data, the formula should be with x as explanatory
    if (is.null(formula)) {
      formula = default.formula
      if (is.na(orientation)) {
        orientation = "x"
      }
    } else {
       if (is.null(orientation) || is.na(orientation)) {
        # we guess orientation from formula
        if (grepl("y", as.character(formula)[2])) {
          orientation <- "x"
        } else if (grepl("x", as.character(formula)[2])) {
          orientation <- "y"
        }
      }
      formula <- swap_xy(formula, backwards = FALSE)
    }
  } else {
    # if we do not flip x and y in data, the formula should match orientation
    # for explanatory.
    # we guess formula from orientation
    if (is.null(formula)) {
      if (is.null(orientation) || is.na(orientation) || orientation == "x") {
        formula <- swap_xy(default.formula, backwards = FALSE)
      } else if (orientation == "y") {
        formula <- swap_xy(default.formula, backwards = TRUE)
      }
    }
    # we guess orientation from formula
    if (is.null(orientation) || is.na(orientation)) {
      # we look for a term CONTAINING x or y
      if (grepl("x", as.character(formula)[2])) {
        orientation <- "y"
      } else if (grepl("y", as.character(formula)[2]))
        orientation <- "x"
    } else if (!grepl("x|y", as.character(formula)[2])) {
      stop("'formula' must refer to aesthetics 'x' and 'y', not names in 'data'")
    }
  }
  list(orientation = orientation,
       formula = formula)
}

#' Check limit.to and fullrange arguments
#'
#' Implement backwards compatibility and support override of
#' \code{fullrange} by \code{limit.to}.
#'
#' @inheritParams stat_poly_eq
#'
#' @keywords internal
#'
check_limit_to  <- function(fullrange, limit.to = NULL, orientation = "x") {
  # respect fullrange for backwards compatibility and consistence with 'ggplot2'
  # but limit.to overrides it silently if set
  if (is.null(limit.to)) {
    if (is.logical(fullrange)) {
      if (fullrange) {
        limit.to <- "none"
      } else {
        limit.to <- orientation
      }
    }
  }

  if (is.character(limit.to) &&
      !limit.to %in% c("none", "x", "y", "xy", "yx")) {
    stop("'limit.to' bad argument: '", limit.to,
         "'! should be one of \"none\", \"x\", \"y\", \"xy\"")
  } else if (is.numeric(limit.to)) {
    limit.to <- sort(unique(na.omit(limit.to)))
  }
  limit.to
}

#' Safely extract the formula from an object
#'
#' @param fm Fitted model object or a call object.
#' @param method.args List of arguments to check for the formula.
#' @param verbose logical If \code{TRUE} message triggered if call to
#'   \code{formula()} fails.
#'
#' @details Method \code{\link{formula}} is not implemented for all fitted
#'   model objects, while the default method triggers an error and stops
#'   exectution. Function \code{fail_safe_formula()} wraps the call to
#'   \code{formula()} and handles the error conditions by attempting to
#'   extract the formula from a list of arguments. If this fails, it returns
#'   \code{NA}, with a message.
#'
#' @return A named list with objects of class formula or NA as member(s).
#'
#' @keywords internal
#'
fail_safe_formula <- function(fm,
                              method.args = list(),
                              verbose = TRUE) {
  withCallingHandlers({
    withRestarts({
      z <- stats::formula(fm)
      if (!is.list(z)) {
        z <- list(formula = z)
      }
    }, handleError = function(cond) {
      selector <- intersect(names(method.args),
                            c("formula", "fixed", "random", "model"))
      if (length(selector)) {
        if (verbose) message("'formula' extracted from arguments")
        z <- method.args[selector]
      } else {
        if (verbose) message("'formula' not available")
        z <- NA
      }
      if (!is.list(z)) {
        z <- list(formula = z)
      }
    })
  }, error = function(cond) {
    invokeRestart("handleError")
  })
}

# Internal function with shared code --------------------------------------

#' Apply model fit methods to data
#'
#' Fit models using different methods translating some arguments
#' to make possible use of consistent arguments across calls to
#' stats.
#'
#' @param data data.frame containing the variables in the model.
#' @param formula a formula object. Using aesthetic names \code{x} and \code{y}
#'   instead of original variable names.
#' @param method function or character If character, "lm", "rlm", "lmrob",
#'   "lts", "gls", "ma", "sma", "segreg", "rq" or the name of a model fit
#'   function are accepted, possibly followed by the fit function's
#'   \code{method} argument separated by a colon (e.g. \code{"rlm:M"}). If a
#'   function is different to \code{lm()}, \code{rlm()}, \code{ltsReg()},
#'   \code{gls()}, \code{ma}, \code{sma}, it must have formal parameters named
#'   \code{formula}, \code{data}, and \code{weights}. See Details.
#' @param method.args named list with additional arguments. Not \code{data}
#'   or \code{weights} which are always passed through aesthetic mappings.
#' @param n.min integer Minimum number of distinct values in the explanatory
#'   variable (on the rhs of formula) for fitting to the attempted.
#' @param fit.seed RNG seed argument passed to
#'   \code{\link[base:Random]{set.seed}()}. Defaults to \code{NA}, indicating
#'   that \code{set.seed()} should not be called.
#' @param orientation character Either "x" or "y" controlling the default for
#'   \code{formula}. The letter indicates the aesthetic considered the
#'   explanatory variable in the model fit.
#' @param level numeric Value in 0..1 used for SMA and MA fits.
#' @param accept.rq logical Accept quantile regression fits with 'quantreg' or
#'   warn when encountered.
#'
#' @return A list with three named members: \code{fm} the fitted model object
#'   and \code{method.args}, the arguments passed to the model fit function as a
#'   nested named list, \code{fit.seed} the seed used and \code{method.name},
#'   the name of the model fit function passed as arguemnt, which can differ
#'   from the class of \code{fm}.
#'
#' @note Called by \code{\link{stat_fit_residuals}()},
#'   \code{\link{stat_fit_deviations}()} and \code{\link{stat_fit_fitted}()}.
#'
#' @keywords internal
#'
fit_models_internal <- function(data,
                                method,
                                method.name,
                                method.args,
                                n.min,
                                formula,
                                fit.seed,
                                orientation,
                                level = 0.95,
                                accept.rq = TRUE) {

  stopifnot(!any(c("formula", "data") %in% names(method.args)))

  if (is.null(data$weight)) {
    data$weight <- 1
  }

  if (length(unique(data[[orientation]])) < n.min) {
    return(list())
  }

  # If method was specified as a character string, replace with
  # the corresponding function. Some model fit functions themselves have a
  # method parameter accepting character strings as argument. We support
  # these by splitting strings passed as argument at a colon.
  if (is.character(method)) {
    # we set default methods for fit functions
    method <- switch(method,
                     lm = "lm:qr",
                     rlm = "rlm:M",
                     lmrob = "lmrob:MM",
                     rq = ifelse(nrow(data) < 5000L, "rq:br", "rq:fn"),
                     lts = "ltsReg",
                     gls = "gls:REML",
                     sma = "sma:SMA",
                     ma = "sma:MA",
                     segreg = "segreg",
                     method)
    method.name <- method
    method <- strsplit(x = method, split = ":", fixed = TRUE)[[1]]
    if (length(method) > 1L) {
      fun.method <- method[2]
      method <- method[1]
    } else {
      fun.method <- character()
    }
    # get functions based on their name
    method <- switch(method,
                     lm = stats::lm,
                     rlm =
                       {rlang::check_installed("MASS",
                                               reason = "to use method \"rlm\"");
                         MASS::rlm},
                     rq =
                       {rlang::check_installed("quantreg",
                                               reason = "to use method \"rq\"");
                         quantreg::rq},
                     lqs =
                       {rlang::check_installed("MASS",
                                               reason = "to use method \"lqs\"");
                         MASS::lqs},
                     ltsReg =
                       {rlang::check_installed("robustbase",
                                               reason = "to use method \"ltsReg\"");
                         robustbase::ltsReg},
                     lmrob =
                       {rlang::check_installed("robustbase",
                                               reason = "to use method \"lmrob\"");
                         robustbase::lmrob},
                     gls =
                       {rlang::check_installed("nlme",
                                               reason = "to use method \"gls\"");
                         nlme::gls},
                     sma =
                       {rlang::check_installed("smatr",
                                               reason = "to use method \"sma\"");
                         smatr::sma},
                     sma =
                       {rlang::check_installed("smatr",
                                               reason = "to use method \"sma\"");
                         smatr::sma},
                     segreg =
                       {rlang::check_installed("segmented",
                                               reason = "to use method \"segreg\"");
                         segmented::segreg},
                     match.fun(method))
  } else if (is.function(method)) {
    fun.method <- character()
  }

  if (exists("weight", data) && !all(data[["weight"]] == 1)) {
    stopifnot("A mapping to 'weight' and a named argument 'weights' cannot co-exist" =
                !"weights" %in% method.args)
    fun.args <- list(formula = quote(formula),
                     data = quote(data),
                     weights = data[["weight"]])
  } else {
    fun.args <- list(formula = quote(formula),
                     data = quote(data))
  }
  fun.args <- c(fun.args, method.args)

  if (length(fun.method)) {
    fun.args[["method"]] <- fun.method
  }

  if (grepl("^ma$|^sma$", method.name) && !"alpha" %in% names(fun.args)) {
    fun.args <- c(fun.args, list(alpha = 1 - level))
  }

  # gls() parameter for formula is called model
  if (grepl("gls", method.name)) {
    names(fun.args)[1] <- "model"
  }

  if (!is.na(fit.seed)) {
    set.seed(fit.seed)
  }
  # quantreg contains code with partial matching of names!
  # so we silence selectively only these warnings
  withCallingHandlers({
    fm <- do.call(method, args = fun.args)
  }, warning = function(w) {
    if (startsWith(conditionMessage(w), "partial match of") ||
        startsWith(conditionMessage(w), "partial argument match of")) {
      invokeRestart("muffleWarning")
    }
  })

  if (!length(fm) || (is.atomic(fm) && is.na(fm))) {
    return(list())
  } else if (!(inherits(fm, "lm") || inherits(fm, "lmrob") ||
               inherits(fm, "gls") || inherits(fm, "lts") ||
               inherits(fm, "lqs") || inherits(fm, "sma") ||
               inherits(fm, "nls") || # inherits(fm, "onls") ||
               accept.rq && (inherits(fm, "rq") || inherits(fm, "rqs")))) {
    message("Method \"", method.name,
            "\" did not return a ",
            "\"lm\", \"nls\", \"lmrob\", \"lqs\", \"lts\", \"gls\", \"sma\", ",
            ifelse(accept.rq, "\"rq\", \"rqs\"", ""),
            "object, possible failure ahead.")
  }
  list(fm = fm,
       method.name = method.name,
       method.args = fun.args,
       fit.seed = fit.seed)
}

# extract_weights ---------------------------------------------------------

#' Extract prior and fitted weights
#'
#' Extract the prior and fitted weights from a fitted model object.
#'
#' @param fm a fitted model object of a supported class.
#' @param n.row interger The expected length of the weights vectors to extract.
#'
#' @return A list with two named members: \code{rob.weight.vals} the weights
#'   actually used to weight residuals, either user supplied or computed, and
#'   \code{weight.vals} the prior weights passed as argument. When not available
#'   the vectors are filled with \code{NA_real_} values.
#'
#' @note Called by \code{\link{stat_fit_residuals}()} and
#'   \code{\link{stat_fit_deviations}()}.
#'
#' @keywords internal
#'
extract_weights <- function(fm, n.row) {
  if (inherits(fm, "lmrob")) {
    rob.weight.vals <- stats::weights(fm, type = "robustness")
    weight.vals <- stats::weights(fm, type = "prior")
    if (!length(weight.vals)) {
      weight.vals <- rep_len(1, n.row)
    }
  } else if (inherits(fm, "lts")) {
    rob.weight.vals <- fm[["lts.wt"]]
    weight.vals <- rep_len(1, n.row)
  } else if (inherits(fm, "rlm")) {
    rob.weight.vals <- fm[["w"]]
    weight.vals <- stats::weights(fm)
  } else if (inherits(fm, "lqs")) {
    rob.weight.vals <- rep_len(NA_real_, n.row)
    weight.vals <- rep_len(1, n.row)
  } else if (inherits(fm, "gls") ||
             inherits(fm, "lme") || inherits(fm, "nlme")) {
    if (!is.null(fm$modelStruct$varStruct)) {
      # "weights" have to be extracted
      rob.weight.vals <- nlme::varWeights(fm$modelStruct$varStruct)
      if (!is.null(nlme::getGroups(fm))) {
        # order of weights needs to be restored
        ordering <- order(order(nlme::getGroups(fm)))
        rob.weight.vals <- rob.weight.vals[ordering]
      }
    } else {
      rob.weight.vals <- rep(1, n.row)
    }
    # weights' argument is a "variance model", not prior weight values
    weight.vals <- rep_len(1, n.row)
  } else if (inherits(fm, "lm")) { # order matters as e.g. "rlm" inherits "lm"
    weight.vals <- stats::weights(fm)
    if (!length(weight.vals)) {
      weight.vals <- rep_len(1, n.row)
    }
    rob.weight.vals <- weight.vals # actually used are those input
  } else {
    rob.weight.vals <- rep(NA_real_, n.row)
    try(weight.vals <- stats::weights(fm))
    if (inherits(weight.vals, "try-error") ||
        length(weight.vals) != n.row) {
      if (exists("weights", fm) &&  # defensive
          length(fm[["weights"]]) == n.row) {
        weight.vals <- fm[["weights"]]
      } else {
        weight.vals <- rep_len(NA_real_, n.row)
      }
    }
  }
  list(rob.weight.vals = rob.weight.vals,
       weight.vals = weight.vals)
}


# extract_fitted ----------------------------------------------------------

extract_fitted <- function(fm, n.row) {
  # As users may use model fit functions that we have not tested
  # we try hard to extract the components from the model fit object
  if (inherits(fm, "sma")) {
    #    fitted.vals <- stats::fitted(fm, type = "fitted", centred = FALSE)
    message("Fitted values could not be retrieved for \"sma\" object!")
    fitted.vals <- rep(NA_real_, n.row)
  } else {
    try(fitted.vals <- stats::fitted(fm))
    if (inherits(fitted.vals, "try-error") ||
        length(fitted.vals) != n.row) {
      if (exists("fitted.values", fm) &&  # defensive
          length(fm[["fitted.values"]]) == n.row) {
        fitted.vals <- fm[["fitted.values"]]
      } else {
        message("Fitted values could not be retrieved for \"",
                class(fm)[1], "\" object!")
        fitted.vals <- rep(NA_real_, n.row)
      }
    }
  }
  fitted.vals
}


# extract_residuals -------------------------------------------------------

extract_residuals <- function(fm, resid.type, weighted) {
  if (inherits(fm, "sma")) {
    fit.residuals <- stats::fitted(fm, type = "residuals")
  } else {
    if (!is.null(resid.type)) {
      if (weighted) {
        if (resid.type != "deviance") {
          warning("Ignoring supplied 'resid.type' as 'weighted = TRUE'")
        }
        resid.args <- list(obj = fm, drop0 = TRUE)
      } else {
        resid.args <- list(object = fm, type = resid.type)
      }
    } else {
      if (weighted) {
        resid.args <- list(obj = fm, drop0 = TRUE)
      } else {
        resid.args <- list(object = fm)
      }
    }
    if (weighted) {
      fit.residuals <-
        do.call(stats::weighted.residuals, args = resid.args)
    } else {
      fit.residuals <-
        do.call(stats::residuals, args = resid.args)
    }
  }

  fit.residuals
}
