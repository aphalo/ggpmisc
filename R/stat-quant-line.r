#' @rdname stat_quant_eq
#'
#' @export
#'
stat_quant_line <- function(mapping = NULL,
                            data = NULL,
                            geom = "smooth",
                            position = "identity",
                            ...,
                            orientation = NA,
                            quantiles = c(0.25, 0.5, 0.75),
                            formula = NULL,
                            se = length(quantiles) == 1L,
                            fit.seed = NA,
                            fm.values = FALSE,
                            n = 80,
                            fullrange = FALSE,
                            limit.to = NULL,
                            method = "rq",
                            method.args = list(),
                            n.min = 3L,
                            level = 0.95,
                            type = "direct",
                            interval = "confidence",
                            na.rm = FALSE,
                            show.legend = NA,
                            inherit.aes = TRUE) {

  stopifnot("Args 'formula' and/or 'data' in 'method.args'" =
              !any(c("formula", "data") %in% names(method.args)))

  if (is.null(se) || is.na(se) || !se) {
    # change defaults because computing confidence band is time consuming
    interval <- "none"
    type <- "none"
  }

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
  } else if (grepl("^lmodel2$|^lmodel2[:]", method.name)) {
    stop("Method 'lmodel2' not supported, please use 'stat_ma_line()'.")
  }

  if (method.name == "rqss") {
    default.formula <- y ~ qss(x)
  } else {
    default.formula <- y ~ x
  }
  temp <- guess_orientation(orientation = orientation,
                            formula = formula,
                            default.formula = default.formula,
                            formula.on.x = TRUE)
  orientation <- temp[["orientation"]]
  formula <-  temp[["formula"]]

  limit.to <- check_limit_to(fullrange = fullrange,
                             limit.to = limit.to,
                             orientation = orientation)

  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = StatQuantLine,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params =
      rlang::list2(
        quantiles = quantiles,
        formula = formula,
        se = se,
        fit.seed = fit.seed,
        fm.values = fm.values,
        n = n,
        limit.to = limit.to,
        method = method,
        method.name = method.name,
        method.args = method.args,
        n.min = n.min,
        na.rm = na.rm,
        orientation = orientation,
        level = level,
        type = type,
        interval = interval,
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
quant_line_compute_group_fun <- function(data,
                                         scales,
                                         quantiles = c(0.25, 0.5, 0.75),
                                         formula = NULL,
                                         n = 80,
                                         limit.to = "x",
                                         xseq = NULL,
                                         method,
                                         method.name,
                                         method.args = list(),
                                         n.min = 3L,
                                         lambda = 1,
                                         level = 0.95,
                                         type = "none",
                                         interval = "none",
                                         se = TRUE,
                                         fit.seed = NA,
                                         fm.values = FALSE,
                                         na.rm = FALSE,
                                         flipped_aes = NA,
                                         orientation = "x",
                                         make.groups = TRUE) {

  rlang::check_installed("quantreg", reason = "to use stat_quant_line()")

  data <- ggplot2::flip_data(data, flipped_aes)
  if (length(unique(data$x)) < n.min) {
    # Not enough data to perform fit
    return(data.frame())
  }

  if (is.null(data[["weight"]])) {
    data[["weight"]] <- 1
  }

  quantiles <- sort(unique(quantiles))

  fms.ls <-  quant_helper_fun(data = data,
                              formula = formula,
                              quantiles = quantiles,
                              fit.by.quantile = TRUE, # one fm per quantile
                              method = method,
                              method.name = method.name,
                              method.args = method.args,
                              n.min = n.min,
                              fit.seed = fit.seed,
                              weight = data[["weight"]],
                              na.rm = na.rm,
                              orientation = "x")

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

  grid <- data.frame(x = xseq)

  preds.ls <- list()
  fms.idxs <- grep("^fm", names(fms.ls))
  for (i in seq_along(fms.idxs)) {
    temp.grid <- grid

    fm <- fms.ls[[fms.idxs[i]]]
    if (!length(fm) || (is.atomic(fm) && is.na(fm))) {
      next()
    }
    pred <- stats::predict(fm, newdata = grid, level = level,
                           type = type, interval = interval)

    if (is.matrix(pred)) {
      temp.grid[["y"]] <- pred[ , 1L]
      if (ncol(pred) >= 3L) {
        temp.grid[["ymin"]] <- pred[ , 2L]
        temp.grid[["ymax"]] <- pred[ , 3L]
      }
    } else {
      temp.grid[["y"]] <- pred
    }
    # if ymin and ymax exist and are not NA they affect scale limits
    if (!exists("ymin", temp.grid)) {
      temp.grid[["ymin"]] <- NA_real_
    }
    if (!exists("ymax", temp.grid)) {
      temp.grid[["ymax"]] <- NA_real_
    }

    temp.grid[["quantile"]] <- fm[["tau"]]
    temp.grid[["group"]] <- paste(data[["group"]][1], fm[["tau"]], sep = "-")

    if (fm.values) {
      temp.grid[["n"]] <- length(resid(fm)) / length(fm[["tau"]])
      temp.grid[["fm.class"]] <- class(fm)
      temp.grid[["fm.method"]] <- method.name
      temp.grid[["fm.formula.chr"]] <- format(formula(fm))
    }

    preds.ls[[i]] <- temp.grid
  }

  z <- dplyr::bind_rows(preds.ls)

  if (nrow(z) >= 1L) {
    # a factor with nicely formatted labels for levels is helpful
    quant.digits <- ifelse(min(z[["quantile"]]) < 0.01 || max(z[["quantile"]]) > 0.99,
                           3, 2)
    quant.levels <- sort(unique(z[["quantile"]]), decreasing = TRUE)
    quant.labels <- sprintf("%#.*f", quant.digits, quant.levels)
    z[["quantile.f"]] <-
      factor(z[["quantile"]], levels = quant.levels, labels = quant.labels)
  }

  if (grepl("y", limit.to)) {
    # with method "sma" or "ma", trimming only on x is illogical
    selector <-
      which(z[[c(x = "y", y = "x")[orientation]]] >= yrange[1] &
              z[[c(x = "y", y = "x")[orientation]]] <= yrange[2])
    z <- z[selector, ]
  }

  z[["flipped_aes"]] <- flipped_aes
  z <- ggplot2::flip_data(z, flipped_aes)

  if (interactive()) {
    show_colnames(z)
  }

  z
}

#' @rdname ggpmisc-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatQuantLine <-
  ggplot2::ggproto("StatQuantLine", ggplot2::Stat,
                   setup_params = function(data, params) {
                     params[["flipped_aes"]] <-
                       ggplot2::has_flipped_aes(data, params, ambiguous = TRUE)
                     params
                   },
                   extra_params = c("na.rm", "orientation"),
                   compute_group = quant_line_compute_group_fun,
                   dropped_aes = "weight",
                   default_aes = ggplot2::aes(group = after_stat(group),
                                              weight = 1),
                   required_aes = c("x", "y")
  )
