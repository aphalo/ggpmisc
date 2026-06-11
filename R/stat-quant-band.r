#' @rdname stat_quant_eq
#'
#' @export
#'
stat_quant_band <- function(mapping = NULL,
                            data = NULL,
                            geom = "smooth",
                            position = "identity",
                            ...,
                            orientation = NA,
                            quantiles = c(0.25, 0.5, 0.75),
                            formula = NULL,
                            fit.seed = NA,
                            fm.values = FALSE,
                            n = 80,
                            fullrange = FALSE,
                            limit.to = NULL,
                            method = "rq",
                            method.args = list(),
                            n.min = 3L,
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

  quantiles <- unique(quantiles)
  if (length(quantiles) != 3) {
    stop("'quantiles' should be a vector of 3 unique quantiles, not ",
         length(quantiles), " quantiles. See 'stat_quant_line()'")
  }

  temp.pars <- rlang::list2(
    quantiles = quantiles,
    formula = formula,
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
    ...
  )

  # avoid warning from other geoms such as "pointrange"
  if (geom == "smooth") {
    temp.pars$se <- TRUE
  }

  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = StatQuantBand,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = temp.pars
  )
}

# Defined here to avoid a note in check --as-cran as the import from 'polynom'
# is not seen when the function is defined in-line in the ggproto object.
#' @rdname ggpmisc-ggproto
#'
#' @format NULL
#' @usage NULL
#'
quant_band_compute_group_fun <- function(data,
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
                                         fit.seed = NA,
                                         fm.values = FALSE,
                                         na.rm = FALSE,
                                         flipped_aes = NA,
                                         orientation = "x") {

  rlang::check_installed("quantreg", reason = "to use stat_quant_band()")

  data <- ggplot2::flip_data(data, flipped_aes)
  if (length(unique(data$x)) < n.min) {
    # Not enough data to perform fit
    return(data.frame())
  }

  if (is.null(data[["weight"]])) {
    data[["weight"]] <- 1
  }

  quantiles <- sort(quantiles)

  fms.ls <-  quant_helper_fun(data = data,
                              formula = formula,
                              quantiles = quantiles,
                              fit.by.quantile = TRUE,
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
  newdata <- data.frame(x = xseq)

  preds.ls <- list()
  preds.names <- c("ymin", "y", "ymax")
  fms.idxs <- grep("^fm", names(fms.ls))
  for (i in seq_along(fms.idxs)) {

    fm <- fms.ls[[fms.idxs[i]]]
    if (!length(fm) || (is.atomic(fm) && is.na(fm))) {
      return(data.frame())
    }
    pred <- stats::predict(fm, newdata = newdata, level = .95,
                           type = "none", interval = "none")

    if (is.matrix(pred)) {
      preds.ls[[preds.names[i]]] <- pred[ , 1L]
    } else {
      preds.ls[[preds.names[i]]] <- pred
    }

    if (fm.values) {
      preds.ls[[paste(names(fms.ls)[[fms.idxs[i]]],
                      "class", sep = ".")]] <- class(fm)
      preds.ls[[paste(names(fms.ls)[[fms.idxs[i]]],
                      "formula.chr", sep = ".")]] <- format(formula(fm))
     }
  }

  newdata <- dplyr::bind_cols(newdata, preds.ls)
  if (!"y" %in% colnames(newdata)) {
    # y in required_aes
    newdata[["y"]] <- NA_real_
  } else {
    if (grepl("y", limit.to)) {
      # with steep slopes trimming on the response range can be needed
      selector <-
        which(newdata[[c(x = "y", y = "x")[orientation]]] >= yrange[1] &
                newdata[[c(x = "y", y = "x")[orientation]]] <= yrange[2])
      newdata <- newdata[selector, ]
    }
  }

  if (fm.values) {
    newdata[["n"]] <- length(resid(fm)) / length(fm[["tau"]])
    newdata[["fm.method"]] <- method.name
  }

  newdata[["flipped_aes"]] <- flipped_aes
  z <- ggplot2::flip_data(newdata, flipped_aes)

  show_colnames(z, stat.name = "stat_quant_band")

  z
}

#' @rdname ggpmisc-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatQuantBand <-
  ggplot2::ggproto("StatQuantBand", ggplot2::Stat,
                   setup_params = function(data, params) {
                     params$flipped_aes <-
                       ggplot2::has_flipped_aes(data, params, ambiguous = TRUE)
                     params
                   },
                   extra_params = c("na.rm", "orientation"),
                   compute_group = quant_band_compute_group_fun,
                   dropped_aes = "weight",
                   required_aes = c("x", "y")
  )

