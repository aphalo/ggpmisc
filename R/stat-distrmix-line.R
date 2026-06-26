#' @rdname stat_distrmix_eq
#'
#' @export
#'
stat_distrmix_line <- function(mapping = NULL,
                               data = NULL,
                               geom = "line",
                               position = "identity",
                               ...,
                               orientation = NA,
                               method = "normalmixEM",
                               se = NULL,
                               quantiles = NA,
                               fit.seed = NA,
                               fm.values = FALSE,
                               n = min(100 + 50 * k, 300),
                               fullrange = TRUE,
                               level = 0.95,
                               method.args = list(),
                               k = 2,
                               free.mean = TRUE,
                               free.sd = TRUE,
                               components = "all",
                               n.min = 10L * k,
                               na.rm = FALSE,
                               show.legend = NA,
                               inherit.aes = TRUE) {

  stopifnot("Arg 'x' should not be in 'method.args'!" =
              !any("x" %in% names(method.args)))

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

  if (is.null(se)) {
    se <- FALSE
  }

  if (is.null(k)) {
    k <- 2
  } else if (k < 1) {
    stop("Expected k >= 1, but k = ", k)
  }

  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = StatDistrmixLine,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = rlang::list2(
      method = method,
      method.name = method.name,
      se = se,
      quantiles = quantiles,
      fit.seed = fit.seed,
      fm.values = fm.values,
      n = n,
      fullrange = fullrange,
      level = level,
      na.rm = na.rm,
      orientation = orientation,
      method.args = method.args,
      k = k,
      free.mean = free.mean,
      free.sd = free.sd,
      components = components,
      n.min = n.min,
      ...
    )
  )
}

distrmix_compute_group_fun <-
  function(data,
           scales,
           method,
           method.name,
           se = FALSE,
           quantiles = NA,
           fit.seed = NA,
           fm.values = FALSE,
           n = 80,
           fullrange = TRUE,
           xseq = NULL,
           level = 0.95,
           method.args = list(),
           k = 2,
           free.mean = TRUE,
           free.sd = TRUE,
           components = "all",
           n.min = 10L * k,
           na.rm = FALSE,
           flipped_aes,
           orientation = NA) {

    rlang::check_installed("mixtools", reason = "to use stat_distrmix_line()")

    data <- ggplot2::flip_data(data, flipped_aes)
    if (is.na(orientation)) {
      if (flipped_aes) {
        orientation <- "y"
      } else {
        orientation <- "x"
      }
    }

    if (length(unique(data$x)) < n.min) {
      message("Skipping! Fewer than 'n.min = ", n.min,
              "' unique observations found 'n = ", length(unique(data$x)), "'")
      # Not enough data to perform fit
      return(data.frame())
    }

    fm_params.tb <-
      distrmix_helper_fun(data = data,
                           method = method,
                           method.name = method.name,
                           se = se,
                           method.args = method.args,
                           k = k,
                           free.mean = free.mean,
                           free.sd = free.sd,
                           n.min = n.min,
                           fit.seed = fit.seed,
                           fm.values = TRUE)

    if (length(fm_params.tb) == 1L && is.na(fm_params.tb)) {
      # model fitting was skipped or failed
      return(data.frame())
    }

    fm_params.tb <- fm_params.tb[-nrow(fm_params.tb), ]

    # x range used for prediction
    # ensure that all the component Normals are fully predicted
    # by passing vectors of fitted parameters to qnorm()
    pred.range <- range(qnorm(p = 0.000125 * min(k, 4),
                              mean = fm_params.tb[["mu"]],
                              sd = fm_params.tb[["sigma"]],
                              lower.tail = TRUE),
                        qnorm(p = 0.000125 * min(k, 4),
                              mean = fm_params.tb[["mu"]],
                              sd = fm_params.tb[["sigma"]],
                              lower.tail = FALSE),
                        scales$ggplot2::flipped_names(flipped_aes)$x$dimension())

    k <- length(fm_params.tb[["lambda"]])

    prediction <- list()
    prediction[["x"]] <-
      seq(from = pred.range[1], to = pred.range[2], length.out = n)
    prediction[["comp.sum"]] <- rep(0, n)
    for (i in 1:k) {
      comp.name <- paste("comp", i, sep = ".")
      prediction[[comp.name]] <-
        dnorm(prediction[["x"]],
              mean = fm_params.tb[["mu"]][i],
              sd = fm_params.tb[["sigma"]][i]) * fm_params.tb[["lambda"]][i]
      prediction[["comp.sum"]] <-
        prediction[["comp.sum"]] + prediction[[comp.name]]
    }

    prediction <- as.data.frame(prediction)

    prediction <-
      tidyr::pivot_longer(prediction,
                          cols = tidyr::starts_with("comp."),
                          names_to = "component",
                          values_to = "density") |> as.data.frame()

    comp.names <- unique(prediction[["component"]])
    lambdas <- c(1, fm_params.tb[["lambda"]])
    names(lambdas) <- comp.names

    if (components == "sum") {
      selector <- which(prediction[["component"]] == "comp.sum")
      prediction <- prediction[selector, ]
      comp.names <- "comp.sum"
    } else if (components == "members") {
      selector <- which(prediction[["component"]] != "comp.sum")
      prediction <- prediction[selector, ]
      comp.names <- setdiff(comp.names, "comp.sum")
    } else if (components != "all") {
      warning("Ignoring bad 'components' argument: \"", components, "\"")
    }

    if (length(unique(na.omit(quantiles))) >= 2) {
      quantiles <- range(quantiles, na.rm = TRUE)
    } else {
      quantiles <- c(0, 1)
    }
    prediction[["cum.density"]] <- numeric(nrow(prediction))
    prediction[["is.tail"]] <- logical(nrow(prediction))
    for (comp in comp.names) {
      selector <- which(prediction[["component"]] == comp)
      temp <- cumsum(prediction[selector, "density", drop = TRUE])
      prediction[selector, "cum.density"] <- temp / max(temp) * lambdas[comp]
      prediction[selector, "is.tail"] <-
        prediction[selector, "cum.density", drop = TRUE] < (quantiles[1] * lambdas[comp]) |
        prediction[selector, "cum.density", drop = TRUE] > (quantiles[2] * lambdas[comp])
    }

    # to be able to obtain a valid cdf we constrain the range late
    if (!fullrange) {
      xrange <- range(data[["x"]])
      selector <-
        which(prediction[["x"]] >= xrange[1] & prediction[["x"]] <= xrange[2])
      prediction <- prediction[selector, ]
    }

    if (fm.values) {
      prediction[["converged"]] <- fm_params.tb[["converged"]][1]
      prediction[["n"]] <- nrow(data)
      prediction[[".size"]] = nrow(prediction)
      prediction[["fm.class"]] <- fm_params.tb[["fm.class"]][1]
      prediction[["fm.method"]] <- fm_params.tb[["fm.method"]][1]
    }

    prediction[["flipped_aes"]] <- flipped_aes
    z <- ggplot2::flip_data(prediction, flipped_aes)
    show_colnames(z, stat.name = "stat_distrmix_line")
    z
  }

#' @rdname ggpmisc-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatDistrmixLine <-
  ggplot2::ggproto("StatDistrmixLine", ggplot2::Stat,
                   setup_params = function(self, data, params) {
                     # temporary kludge as I cannot get has_flipped_aes() to work
                     # unless 'orientation' is set
                     if (is.null(params$orientation) || is.na(params$orientation)) {
                       if ("x" %in% colnames(data)) {
                         params$orientation <- "x"
                       } else if ("y" %in% colnames(data)) {
                         params$orientation <- "y"
                       }
                     }
                     if (!params$orientation %in% colnames(data)) {
                       stop("'orientation' does not match a mapped aesthetic")
                     }

                     params$flipped_aes <-
                       has_flipped_aes(data, params,
                                       main_is_orthogonal = FALSE,
                                       main_is_continuous = TRUE)

                     params
                   },

                   extra_params = c("na.rm", "orientation"),

                   compute_group = distrmix_compute_group_fun,

                   default_aes =
                     ggplot2::aes(x = after_stat(density),
                                  y = after_stat(density),
                                  group = after_stat(component),
                                  weight = NULL),
                   dropped_aes = "weight",
                   required_aes = "x|y"
  )

#' @rdname stat_distrmix_eq
#'
#' @export
#'
stat_distrmix_area <- function(mapping = NULL,
                               data = NULL,
                               geom = "area",
                               position = "identity",
                               ...,
                               orientation = NA,
                               method = "normalmixEM",
                               se = NULL,
                               quantiles = NA,
                               fit.seed = NA,
                               fm.values = FALSE,
                               n = min(100 + 50 * k, 300),
                               fullrange = TRUE,
                               level = 0.95,
                               method.args = list(),
                               k = 2,
                               free.mean = TRUE,
                               free.sd = TRUE,
                               components = "sum",
                               n.min = 10L * k,
                               na.rm = FALSE,
                               show.legend = NA,
                               inherit.aes = TRUE) {

  stat_distrmix_line(mapping = mapping,
                     data = data,
                     geom = geom,
                     position = position,
                     ... = ...,
                     orientation = orientation,
                     method = method,
                     se = se,
                     quantiles = quantiles,
                     fit.seed = fit.seed,
                     fm.values = fm.values,
                     n = n,
                     fullrange = fullrange,
                     level = level,
                     method.args = method.args,
                     k = k,
                     free.mean = free.mean,
                     free.sd = free.sd,
                     components = components,
                     n.min = n.min,
                     na.rm = na.rm,
                     show.legend = show.legend,
                     inherit.aes = inherit.aes)
}
