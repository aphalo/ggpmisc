#' Helper function for fitting quantile regression
#'
#' Factored out code used in both stat_quant_line(), stat_quant_band()
#' and stat_quant_eq().
#'
#' @inheritParams stat_quant_line
#' @param fit.by.quantile logical If TRUE return a separate fitted model
#'   object for each quantile as needed for predictions with confidence bands.
#'
#' @return A list, with members fm1, fm2, ... one for each fitted model and
#'   fun.args1, fun.args2, ... with arguments passed in each call to the
#'   model fit function.
#'
#' @keywords internal
#'
#' @details
#' This function does the model fitting and returns a fitted model object. It
#' decodes the method, sorts the quantiles and does the fit.
#'
#' @examples
#'
#' ggpmisc:::quant_helper_fun(data.frame(x = mpg$displ, y = mpg$hwy), method = "br")
#'
quant_helper_fun <- function(data,
                             formula = y ~ x,
                             quantiles = c(0.25, 0.5, 0.75),
                             fit.by.quantile = FALSE,
                             method,
                             method.name,
                             method.args = list(),
                             n.min = 3L,
                             fit.seed = NA,
                             weight = 1,
                             na.rm = FALSE,
                             orientation = "x") {

  if (length(unique(data[[orientation]])) >= n.min) {
    if (is.null(weight)) {
      weight <- 1
    }
    if (!exists("weight", data)) {
      data[["weight"]] <- rep_len(weight, length.out = nrow(data))
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
                     data = quote(data),
                     weights = data[["weight"]])
    fun.args <- c(fun.args, method.args)
    if (length(fun.method)) {
      fun.args[["method"]] <- fun.method
    }

    if (fit.by.quantile) {
      qs <- seq_along(quantiles)
    } else {
      qs <- 1L
    }
    z <- list()
    if (!is.na(fit.seed)) {
      set.seed(fit.seed)
    }
    for (i in qs) {
      if (length(qs) == 1L) {
        fun.args[["tau"]] <- quantiles
      } else {
        fun.args[["tau"]] <- quantiles[i]
      }
      # quantreg contains code with partial matching of names!
      # so we silence selectively only these warnings
      withCallingHandlers({
        fm <- do.call(method, args = fun.args)
        z[[paste("fm", i, sep ="")]] <- if (!is.null(fm)) fm else NA
      }, warning = function(w) {
        if (startsWith(conditionMessage(w), "partial match of") ||
            startsWith(conditionMessage(w), "partial argument match of")) {
          invokeRestart("muffleWarning")
        }
      })
      z[[paste("fun.args", i, sep ="")]] <- fun.args
    }
  } else {
    for (i in qs) {
      z[[paste("fm", i, sep ="")]] <- NA
      z[[paste("fun.args", i, sep ="")]] <- fun.args
    }
  }
  z
}
