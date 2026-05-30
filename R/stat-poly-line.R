#' @rdname stat_poly_eq
#'
#' @export
#'
stat_poly_line <- function(mapping = NULL,
                           data = NULL,
                           geom = "smooth",
                           position = "identity",
                           ...,
                           orientation = NA,
                           method = "lm",
                           formula = NULL,
                           se = NULL,
                           fit.seed = NA,
                           fm.values = FALSE,
                           n = 80,
                           fullrange = FALSE,
                           limit.to = NULL,
                           level = 0.95,
                           method.args = list(),
                           n.min = 2L,
                           na.rm = FALSE,
                           show.legend = NA,
                           inherit.aes = TRUE) {

  stopifnot("Args 'formula' and/or 'data' in 'method.args'" =
              !any(c("formula", "data") %in% names(method.args)))

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

  if (method.name == "auto") {
    message("Method 'auto' is equivalent to 'lm', splines are not supported.")
    method <- method.name <- "lm"
  } else if (grepl("^rq$|^rq[:]|^rqss$|^rqss[:]", method.name)) {
    stop("Methods 'rq' and 'rqss' not supported, please use 'stat_quant_line()'.")
  } else if (grepl("^lmodel2$|^lmodel2[:]", method.name)) {
    stop("Method 'lmodel2' not supported, please use 'stat_ma_line()'.")
  }

  if (is.null(se)) {
    se <- ifelse(grepl("gls|lqs", method.name), FALSE, TRUE)
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

  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = StatPolyLine,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = rlang::list2(
      method = method,
      method.name = method.name,
      formula = formula,
      se = se,
      fit.seed = fit.seed,
      fm.values = fm.values,
      n = n,
      limit.to = limit.to,
      level = level,
      na.rm = na.rm,
      orientation = orientation,
      method.args = method.args,
      n.min = n.min,
      ...
    )
  )
}

poly_line_compute_group_fun <-
  function(data,
           scales,
           method,
           method.name,
           formula = NULL,
           se,
           fit.seed = NA,
           fm.values = FALSE,
           n = 80,
           limit.to = "x",
           xseq = NULL,
           level = 0.95,
           method.args = list(),
           n.min = 2L,
           na.rm = FALSE,
           flipped_aes = NA,
           orientation = "x") {

    data <- ggplot2::flip_data(data, flipped_aes)

    temp.ls <- fit_models_internal(data = data,
                                   method = method,
                                   method.name = method.name,
                                   method.args = method.args,
                                   n.min = n.min,
                                   formula = formula,
                                   fit.seed = fit.seed,
                                   orientation = "x") # data already flipped
    if (!length(temp.ls) || !length(temp.ls[["fm"]])) {
      # An empty data.frame results in no plot layer when passed to geoms
      return(data.frame())
    }
    fm <- temp.ls[["fm"]]
    method.name <- temp.ls[["method.name"]] # argument or default which varies
    method.args <- temp.ls[["method.args"]] # argument or default which varies

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

    has.predict.method <- FALSE
    for (cl in class(fm)) {
      if (cl == "sma") break() # has dummy predict() method
      if (any(grepl("^predict", utils::methods(class = cl)))) {
        has.predict.method <- TRUE
        break()
      }
    }
    if (has.predict.method) {
      # We try hard to extract predicted values
      newdata <- data.frame(x = xseq)
      try(
        prediction <- stats::predict(fm,
                                     newdata = newdata,
                                     se.fit = se,
                                     level = level,
                                     interval = if (se) "confidence" else "none"
        )
      )
      if (inherits(prediction, "try-error")) {
        if (se) {
          message("Confidence band not supported: overridding 'se = TRUE'")
          se <- FALSE
        }
        try(
          prediction <- stats::predict(fm, newdata = newdata)
        )
        if (inherits(prediction, "try-error")) {
          warning("Prediction failed!")
          prediction <- rep_len(NA_real_, nrow(data))
        }
      }
      if (se) {
        if (exists("fit", prediction)) {
          prediction <- as.data.frame(prediction[["fit"]])
        }
        names(prediction) <- c("y", "ymin", "ymax")
      } else {
        prediction <- data.frame(y = as.vector(prediction))
      }
      prediction <- cbind(newdata, prediction)
    } else { # does not have predict method
      if (class(fm)[1] == "sma") {
        if (se) {
          # fm$coef[[1]] is a data.frame
          #
          #           coef(SMA) lower limit upper limit
          # elevation 39.441685   38.011038    40.87233
          # slope     -4.609003   -5.008148    -4.24167
          ### parameter estimates assumed independent!!
          ## bootstraping would be more appropriate
          coef.sma <- fm$coef[[1]]
          b0 <- unlist(coef.sma["elevation", ])
          b0 <- ifelse(is.na(b0), 0, b0)
          b1 <- unlist(coef.sma["slope", ])
          # centering is needed
          b0delta <- b0 - b0[1]
          if (all(b0 == 0)) {
            # center on zero
            center.y <- 0
            center.x <- 0
          } else {
            message("SMA/MA, band is currently for slope only!")
            # data centroid
            center.y <- mean(data[["y"]])
            center.x <- mean(data[["x"]])
          }
          rightside <- xseq > center.x
          leftside <- !rightside
          prediction <- data.frame(x = xseq,
                                   y = center.y + b0delta[1] +
                                     b1[1] * (xseq - center.x),
                                   ymin = center.y +
                                     (rightside * b1[2] + leftside * b1[3]) *
                                     (xseq - center.x),
                                   ymax = center.y +
                                     (rightside * b1[3] + leftside * b1[2]) *
                                     (xseq - center.x))
        } else { # se is FALSE
          coefs <- stats::coefficients(fm)
          # named vector
          #
          # elevation     slope
          # 39.441685 -4.609003
          prediction <-
            data.frame(x = xseq, # data[["x"]]
                       y = coefs["elevation"] + coefs["slope"] * xseq)
        }
      } else {
        message("Fitted line plotted")
        prediction <-
          data.frame(x = data[["x"]], y = fitted(fm))
      }
    }

    if (grepl("y", limit.to)) {
      # with method "sma" or "ma", trimming only on x is illogical
      selector <-
        which(prediction[[c(x = "y", y = "x")[orientation]]] >= yrange[1] &
                prediction[[c(x = "y", y = "x")[orientation]]] <= yrange[2])
      prediction <- prediction[selector, ]
    }

    if (fm.values) {
      fm.summary <- summary(fm)
      # summary methods for different fit methods may no return some slots
      if (exists("fstatistic", fm.summary)) {
        prediction[["p.value"]] <-
          stats::pf(q = fm.summary[["fstatistic"]][["value"]],
                    df1 = fm.summary[["fstatistic"]][["numdf"]],
                    df2 = fm.summary[["fstatistic"]][["dendf"]],
                    lower.tail = FALSE)
      } else {
        prediction[["p.value"]] <- NA_real_
      }
      if (exists("r.squared", fm.summary)) {
        prediction[["r.squared"]] <- fm.summary[["r.squared"]]
      } else {
        prediction[["r.squared"]] <- NA_real_
      }
      if (exists("adj.r.squared", fm.summary)) {
        prediction[["adj.r.squared"]] <- fm.summary[["adj.r.squared"]]
      } else {
        prediction[["adj.r.squared"]] <- NA_real_
      }
      if (exists("residuals", fm)) {
        prediction[["n"]] <- length(fm[["residuals"]])
      } else {
        prediction[["n"]] <- NA_real_
      }
      prediction[["fm.class"]] <- class(fm)[1]
      prediction[["fm.method"]] <- method.name
      prediction[["fm.formula"]] <- fail_safe_formula(fm, method.args)
      prediction[["fm.formula.chr"]] <- format(prediction[["fm.formula"]])
    }

     prediction$flipped_aes <- flipped_aes
     ggplot2::flip_data(prediction, flipped_aes)
  }

#' @rdname ggpmisc-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatPolyLine <-
  ggplot2::ggproto("StatPolyLine", Stat,
                   setup_params = function(data, params) {
                     params[["flipped_aes"]] <-
                       ggplot2::has_flipped_aes(data, params, ambiguous = TRUE)
                     params
                   },

                   extra_params = c("na.rm", "orientation"),

                   compute_group = poly_line_compute_group_fun,

                   dropped_aes = c("weight"),
                   required_aes = c("x", "y")
  )
