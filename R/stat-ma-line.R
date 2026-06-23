#' @rdname stat_ma_eq
#'
#' @export
#'
stat_ma_line <- function(mapping = NULL,
                         data = NULL,
                         geom = "smooth",
                         position = "identity",
                         ...,
                         orientation = NA,
                         method = "lmodel2:MA",
                         method.args = list(),
                         n.min = 2L,
                         formula = NULL,
                         range.y = NULL,
                         range.x = NULL,
                         se = TRUE,
                         fit.seed = NA,
                         fm.values = FALSE,
                         n = 80,
                         nperm = 99,
                         fullrange = FALSE,
                         limit.to = NULL,
                         level = 0.95,
                         na.rm = FALSE,
                         show.legend = NA,
                         inherit.aes = TRUE) {

  stopifnot("Args 'formula' and/or 'data' in 'method.args'" =
              !any(c("formula", "data") %in% names(method.args)))

  # we make a character string name for the method
  if (is.character(method)) {
    method <- trimws(method, which = "both")
    method.name <- method
  } else if (is.function(method)) {
    method.name <- deparse(substitute(method))
    if (grepl("^function[ ]*[(]", method.name[1])) {
      method.name <- "function"
    }
  } else {
    method.name <- "missing"
  }

  if (grepl("^lm$|^lm[:]|^rlm$|^rlm[:]|^gls$|^gls[:]", method.name)) {
    stop("Methods 'lm', 'rlm' and 'gls' not supported, please use 'stat_poly_line()'.")
  } else if (grepl("^rq$|^rq[:]", method.name)) {
    stop("Method 'rq' not supported, please use 'stat_quant_line()'.")
  }

  temp <- guess_orientation(orientation = orientation,
                            formula = formula,
                            default.formula = y ~ x,
                            formula.on.x = TRUE)
  orientation <- temp[["orientation"]]
  formula <-  temp[["formula"]]

  limit.to <- check_limit_to(fullrange = fullrange,
                           limit.to = limit.to,
                           orientation = orientation)

  if (is.character(method)) {
    if (grepl("^rq", method)) {
      stop("Method 'rq' not supported, please use 'stat_quant_eq()'.")
    } else if (grepl("^lm$|^lm[:]|^rlm$|^rlm[:]", method)) {
      stop("Methods 'lm' and 'rlm' not supported, please use 'stat_poly_eq()'.")
    }
  }

  if (grepl("RMA$", method.name) && (is.null(range.y) || is.null(range.x))) {
    stop("Method \"RMA\" is computed only if both 'range.x' and 'range.y' are set.")
  }

  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = StatMaLine,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = rlang::list2(
      method = method,
      method.args = method.args,
      n.min = n.min,
      formula = formula,
      range.y = range.y,
      range.x = range.x,
      se = se,
      fit.seed = fit.seed,
      fm.values = fm.values,
      n = n,
      nperm = nperm,
      limit.to = limit.to,
      level = level,
      na.rm = na.rm,
      orientation = orientation,
      ...
    )
  )
}

# Defined here to avoid a note in check --as-cran as the import from 'polynom'
# is not seen when the function is defined in-line in the ggproto object.
#' @rdname ggpmisc-ggproto
#'
#' @format NULL
#' @usage NULL
#'
ma_line_compute_group_fun <-
  function(data,
           scales,
           method,
           method.args = list(),
           n.min = 2L,
           formula = NULL,
           range.y = NULL, range.x = NULL,
           se = TRUE,
           fit.seed = NA,
           fm.values = FALSE,
           n = 80,
           nperm = 99,
           limit.to = "x",
           xseq = NULL,
           level = 0.95,
           na.rm = FALSE,
           flipped_aes = NA,
           orientation = "x") {

    rlang::check_installed("lmodel2", reason = "to use stat_ma_line()")

    data <- ggplot2::flip_data(data, flipped_aes)
    if (length(unique(data$x)) < n.min) {
      # Not enough data to perform fit
      return(data.frame())
    }

    if (is.numeric(limit.to)) {
      xseq <- limit.to
      limit.to <- "none"
    }

    if (is.null(xseq)) {
      if (grepl("x", limit.to)) {
        xrange <- range(data[[orientation]], na.rm = TRUE)
      } else {
        xrange <- scales[[orientation]]$dimension()
      }
      if (grepl("y", limit.to)) {
        yrange <- range(data[[c(x = "y", y = "x")[orientation]]], na.rm = TRUE)
      } else {
        yrange <- scales[[c(x = "y", y = "x")[orientation]]]$dimension()
      }
      xseq <- seq(from = xrange[1], to = xrange[2], length.out = n)
    }

    temp.ls <- fit_lmodel2_internal(data = data,
                                    method = method,
                                    method.args = method.args,
                                    n.min = n.min,
                                    formula = formula,
                                    range.y = range.y,
                                    range.x = range.x,
                                    fit.seed = fit.seed,
                                    orientation = orientation,
                                    nperm = nperm)

    if (!length(temp.ls) || !length(temp.ls[["fm"]])) {
      # An empty data.frame results in no plot layer when passed to geoms
      return(data.frame())
    }
    fm <- temp.ls[["fm"]]
    method.name <- temp.ls[["method.name"]] # argument or default which varies
    fun.method <- temp.ls[["fun.method"]]
    method.args <- temp.ls[["method.args"]] # argument or default which varies

    newdata <- data.frame(x = xseq)

    prediction <- stats::predict(fm,
                                 method = fun.method,
                                 newdata = newdata,
                                 interval = "confidence"
    )

    names(prediction) <- c("y", "ymin", "ymax")
    if (any(is.na(prediction$ymin)) || any(is.na(prediction$ymax))) {
      warning("Confidence band not available; see 'lmoldel2::lmodel2()'")
      prediction$ymin <- NULL
      prediction$ymax <- NULL
    }
    prediction <- cbind(newdata, prediction)

    if (grepl("y", limit.to)) {
      # with method "sma" or "ma", trimming only on x is illogical
      selector <-
        which(prediction[[c(x = "y", y = "x")[orientation]]] >= yrange[1] &
                prediction[[c(x = "y", y = "x")[orientation]]] <= yrange[2])
      prediction <- prediction[selector, ]
    }

    if (fm.values) {
      idx <- which(fm[["regression.results"]][["Method"]] == fun.method)
      prediction[["p.value"]] <- fm[["regression.results"]][["P-perm (1-tailed)"]][idx]
      prediction[["r.squared"]] <- fm[["rsquare"]]
      prediction[["n"]] <- fm[["n"]]
      prediction[["fm.class"]] <- class(fm)[1]
      prediction[["fm.method"]] <- method.name
      prediction[["fm.formula"]] <- fail_safe_formula(fm, method.args)
      prediction[["fm.formula.chr"]] <- format(prediction[["fm.formula"]])
    }
    prediction$flipped_aes <- flipped_aes
    z <- ggplot2::flip_data(prediction, flipped_aes)

    show_colnames(z, stat.name = "stat_ma_line")

    z
  }

#' @rdname ggpmisc-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatMaLine <-
  ggplot2::ggproto("StatMaLine", Stat,
                   setup_params = function(data, params) {
                     params$flipped_aes <- has_flipped_aes(data, params, ambiguous = TRUE)
#                     message("`geom_ma_line()` using method ", params$method)
                     params
                   },

                   extra_params = c("na.rm", "orientation"),

                   compute_group = ma_line_compute_group_fun,

                   required_aes = c("x", "y")
  )
