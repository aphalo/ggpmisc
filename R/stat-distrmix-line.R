#' @rdname stat_distrmix_eq
#'
#' @export
#'
stat_distrmix_line <- function(mapping = NULL,
                                data = NULL,
                                geom = "line",
                                position = "identity",
                                ...,
                                orientation = "x",
                                method = "normalmixEM",
                                se = NULL,
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
    k <- k
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
           fit.seed = NA,
           fm.values = FALSE,
           n = 80,
           fullrange = FALSE,
           xseq = NULL,
           level = 0.95,
           method.args = list(),
           k = 2,
           free.mean = TRUE,
           free.sd = TRUE,
           components = "all",
           n.min = 10L * k,
           na.rm = FALSE,
           flipped_aes = NA,
           orientation = "x") {

    rlang::check_installed("mixtools", reason = "to use stat_distrmix_line()")

    data <- ggplot2::flip_data(data, flipped_aes)

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
    if (fullrange) {
      # ensure that the component Normals are fully predicted
      x.range <- range(qnorm(p = 0.0005,
                             mean = fm_params.tb[["mu"]],
                             sd = fm_params.tb[["sigma"]],
                             lower.tail = TRUE),
                       qnorm(p = 0.0005,
                             mean = fm_params.tb[["mu"]],
                             sd = fm_params.tb[["sigma"]],
                             lower.tail = FALSE),
                       scales[[orientation]]$dimension())
    } else {
      # predict the component normals in the data range
      x.range <- range(data[["x"]])
    }

    k <- length(fm_params.tb[["lambda"]])
    prediction <- list()
    prediction[["x"]] <-
      seq(from = x.range[1], to = x.range[2], length.out = n)
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
                          values_to = "density")

    if (components == "sum") {
      selector <- which(prediction[["component"]] == "comp.sum")
      prediction <- prediction[selector, ]
    } else if (components == "members") {
      selector <- which(prediction[["component"]] != "comp.sum")
      prediction <- prediction[selector, ]
    } else if (components != "all") {
      warning("Ignoring bad 'components' argument: \"", components, "\"")
    }

    if (fm.values) {
      prediction[["converged"]] <- fm_params.tb[["converged"]][1]
      prediction[["n"]] <- nrow(data)
      prediction[[".size"]] = nrow(prediction)
      prediction[["fm.class"]] <- fm_params.tb[["fm.class"]][1]
      prediction[["fm.method"]] <- fm_params.tb[["fm.method"]][1]
    }

    prediction[["flipped_aes"]] <- flipped_aes
    ggplot2::flip_data(prediction, flipped_aes)
  }

#' @rdname ggpmisc-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatDistrmixLine <-
  ggplot2::ggproto("StatDistrmixLine", ggplot2::Stat,
                   setup_params = function(data, params) {
                     params[["flipped_aes"]] <-
                       ggplot2::has_flipped_aes(data, params, ambiguous = TRUE)
                     params
                   },

                   extra_params = c("na.rm", "orientation"),

                   compute_group = distrmix_compute_group_fun,

                   default_aes =
                     ggplot2::aes(y = after_stat(density),
                                  group = after_stat(component)),
                   dropped_aes = c("weight"),
                   required_aes = "x|y"
  )
