#' Helper function for fitting qunatile regression
#'
#' Factored out code used in both stat_quant_line(), stat_quant_band()
#' and stat_quant_eq().
#'
#' @inheritParams stat_quant_line
#'
#' @keywords internal
#'
#' @details
#' This function does the model fitting and returns a fitted model object. It
#' decodes the method, sorts the quantiles and does the fit.
#'
quant_helper_fun <- function(data,
                             formula = y ~ x,
                             quantiles = c(0.25, 0.5, 0.75),
                             method,
                             method.name,
                             method.args = list(),
                             n.min = 3L,
                             fit.seed = NA,
                             weight = 1,
                             na.rm = FALSE,
                             orientation = "x") {

  if (length(unique(data[[orientation]])) < n.min) {
    return(data.frame())
  }

  # If method was specified as a character string, replace with
  # the corresponding function. Some model fit functions themselves have a
  # method parameter accepting character strings as argument. We support
  # these by splitting strings passed as argument at a colon.
  if (is.character(method)) {
    if (method %in% c("br", "fn", "pfn", "sfn", "fnc", "conquer",
                      "pfnb", "qfnb", "ppro", "lasso")) {
      method <- paste("rq", method, sep = ":")
      message("Using method: ", method)
    }
    method.name <- method
    method <- strsplit(x = method, split = ":", fixed = TRUE)[[1]]
    if (length(method) > 1L) {
      fun.method <- method[2]
      method <- method[1]
    } else {
      fun.method <- character()
    }
    method <- switch(method,
                     rq = quantreg::rq,
                     rqss = quantreg::rqss,
                     match.fun(method))
  } else if (is.function(method)) {
    fun.method <- method.args[["method"]]
    if (length(fun.method)) {
      method.name <- paste(method.name, fun.method, sep = ":")
    }
  }

  fun.args <- list(quote(formula),
                   tau = quantiles,
                   data = quote(data),
                   weights = data[["weight"]])
  fun.args <- c(fun.args, method.args)
  if (length(fun.method)) {
    fun.args[["method"]] <- fun.method
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
  list(fm = fm, fun.args = fun.args)
}
