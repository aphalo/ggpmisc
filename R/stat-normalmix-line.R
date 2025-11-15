#' Predicted line from Normal mixture model fit
#'
#' \code{stat_normalmix_line()} fits a Normal mixture model, by default with
#' \code{\link[mixtools]{normalmixEM}()}. Predicted values are
#' computed and, by default, plotted.
#'
#' @details
#' This statistic is similar to \code{\link[ggplot2]{stat_density}} with a
#' Guassian kernel but instead of fitting a sinple Normal distribution it fits
#' a mixture of two or more Normal distributions.
#' Other defaults are consistent with those in \code{stat_normalmix_eq()}.
#'
#' @param mapping The aesthetic mapping, usually constructed with
#'   \code{\link[ggplot2]{aes}}. Only needs to be set at the layer level if you
#'   are overriding the plot defaults.
#' @param data A layer specific dataset, only needed if you want to override the
#'   plot defaults.
#' @param geom The geometric object to use display the data
#' @param position The position adjustment to use for overlapping points on this
#'   layer.
#' @param show.legend logical. Should this layer be included in the legends?
#'   \code{NA}, the default, includes if any aesthetics are mapped. \code{FALSE}
#'   never includes, and \code{TRUE} always includes.
#' @param inherit.aes If \code{FALSE}, overrides the default aesthetics, rather
#'   than combining with them. This is most useful for helper functions that
#'   define both data and aesthetics and shouldn't inherit behaviour from the
#'   default plot specification, e.g. \code{\link[ggplot2]{borders}}.
#' @param ... other arguments passed on to \code{\link[ggplot2]{layer}}. This
#'   can include aesthetics whose values you want to set, not map. See
#'   \code{\link[ggplot2]{layer}} for more details.
#' @param na.rm	a logical indicating whether NA values should be stripped before
#'   the computation proceeds.
#' @param method function or character If character, "normalmixEM"
#'   or the name of a model fit function are accepted, possibly followed by the
#'   fit function's \code{method} argument separated by a colon. The function
#'   must return a model fit object of class \code{mixEM}.
#' @param method.args named list with additional arguments.
#' @param k integer Number of mixture components to fit.
#' @param free.mean,free.sd logical If TRUE, allow the fitted \code{mean} and/or
#'   fitted \code{sd} to vary among the component Normal distributions.
#' @param components character One of \code{"all"}, \code{"sum"}, or
#'   \code{members} select which densities are returned.
#' @param n.min integer Minimum number of distinct values in the mapped
#'   variable for fitting to the attempted.
#' @param se Currently ignored.
#' @param fm.values logical Add parameter estimates and their standard errors
#'   to the returned values (`FALSE` by default.)
#' @param fullrange Should the prediction span the full range of the fitted
#'   distributions, or just the range of the data?
#' @param level Level of confidence interval to use (0.95 by default).
#' @param n Number of points at which to evaluate smoother.
#' @param orientation character Either "x" or "y" controlling the default for
#'   \code{formula}.
#'
#' @return The value returned by the statistic is a data frame, with \code{n}
#'   rows of predicted density for each component of the mixture plus their
#'   sum and the corresponding vector of \code{x} values. Optionally it will
#'   also include additional values related to the model fit.
#'
#' @section Computed variables: \code{stat_normalmix_line()} provides the following
#'   variables, some of which depend on the orientation:
#'   \describe{\item{density}{predicted density values}
#'   \item{x}{the \code{n} values for the quantiles}
#'   \item{component}{A factor indexing the components and their sum}}
#'
#'   If \code{fm.values = TRUE} is passed then columns with parameters estimates
#'   are added, with the same value in each row within a group.
#'   This is wasteful and disabled by default, but provides a simple and robust
#'   approach to achieve effects like colouring or hiding of the model fit line
#'   by group.
#'
#' @section Aesthetics: \code{stat_normalmix_line} expects observations mapped
#'   to \code{x} and \code{weight} passed as argument
#'   to parameter \code{weights}. Both must be mapped to \code{numeric}
#'   variables. In addition, the aesthetics understood by the geom
#'   (\code{"geom_line"} is the default) are understood and grouping
#'   respected.
#'
#' @family ggplot statistics for mixture model fits.
#'
#' @examples
#' ggplot(faithful, aes(x = waiting)) +
#'   stat_normalmix_line()
#'
#' ggplot(faithful, aes(x = waiting)) +
#'   stat_normalmix_line(components = "sum")
#'
#' ggplot(faithful, aes(x = waiting)) +
#'   stat_normalmix_line(components = "members")
#'
#' ggplot(faithful, aes(x = waiting)) +
#'  geom_histogram(aes(y = after_stat(density)), bins = 20) +
#'  stat_normalmix_line(aes(colour = after_stat(component),
#'                          fill = after_stat(component)),
#'                      geom = "area", linewidth = 1, alpha = 0.25, se = FALSE)
#'
#' ggplot(faithful, aes(x = waiting)) +
#'  stat_normalmix_line(aes(colour = after_stat(component),
#'                          fill = after_stat(component)),
#'                      geom = "area", linewidth = 1, alpha = 0.25,
#'                      components = "members", se = FALSE)
#'
#' @export
#'
stat_normalmix_line <- function(mapping = NULL,
                                data = NULL,
                                geom = "line",
                                position = "identity",
                                ...,
                                method = "normalmixEM",
                                se = NULL,
                                fm.values = FALSE,
                                n = 250,
                                fullrange = TRUE,
                                level = 0.95,
                                method.args = list(),
                                k = 2,
                                free.mean = TRUE,
                                free.sd = TRUE,
                                components = "all",
                                n.min = 10L * k,
                                na.rm = FALSE,
                                orientation = NA,
                                show.legend = NA,
                                inherit.aes = TRUE) {
  stopifnot("Arg 'x' in 'method.args'" = !any("x" %in% names(method.args)))

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

  if (method.name != "normalmixEM") {
    stop("Only method currently supported is \"normalmixEM\"")
  }

  if (is.null(se)) {
    se <- FALSE
  }

  if (is.null(k)) {
    k <- k
  } else if (k < 2) {
    stop("Expected k >= 2, but k = ", k)
  }

  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = StatNormalmixLine,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = rlang::list2(
      method = method,
      method.name = method.name,
      se = se,
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

poly_normalmix_compute_group_fun <-
  function(data,
           scales,
           method,
           method.name,
           se,
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
    data <- ggplot2::flip_data(data, flipped_aes)
    if (length(unique(data$x)) < n.min) {
      # Not enough data to perform fit
      return(data.frame())
    }

#    if (is.null(data$weight)) data$weight <- 1

    # If method was specified as a character string, replace with
    # the corresponding function. Some model fit functions themselves have a
    # method parameter accepting character strings as argument. We support
    # these by splitting strings passed as argument at a colon.
    if (is.character(method)) {
      method <- switch(method,
                       normalmix = ,
                       normalmixEM = "normalmixEM",
                       method)
      method.name <- method
      method <- strsplit(x = method, split = ":", fixed = TRUE)[[1]]
      if (length(method) > 1L) {
        fun.method <- method[2]
        method <- method[1]
      } else {
        fun.method <- character()
      }
      method <- switch(method,
                       normalmixEM = mixtools::normalmixEM,
                       match.fun(method))
    } else if (is.function(method)) {
      fun.method <- character()
    }

    if (exists("maxit", method.args)) {
      maxit <- method.args[["maxit"]]
    } else {
      maxit <- 1e3
    }
    fun.args <- list(x = data[["x"]],
                     k = k,
                     arbmean = free.mean,
                     arbvar = free.sd,
                     maxit = maxit)

    if (length(intersect(fun.args, method.args))) {
      warning("Skipped named arguments in 'method.args': ",
              paste(intersect(fun.args, method.args), collapse = ", "))
      method.args <- method.args[setdiff(names(method.args), names(fun.args))]
    }
    fun.args <- c(fun.args, method.args)

    fm <- do.call(method, args = fun.args)

    converged <- length(fm[["all.loglik"]]) < maxit

    if (!length(fm) || (is.atomic(fm) && is.na(fm))) {
      return(data.frame())
    } else if (!inherits(fm, "mixEM")) {
      warning("Method \"", method.name,
              "\" did not return a ",
              "\"mixEM\" object, skipping.")
      return(data.frame())
    }

    # extract fitted parameter estimates
    if (se) {
      # using bootstrap, standard errors are computed for the parameter estimates
      # B is the number of "trials" used to estimate the se
      Nmix.param.se <- mixtools::boot.se(Nmix.ls, B = 100)
      Nmix.param.se[grepv(".se$", names(Nmix.param.se))]

      params.tb <- c(Nmix.ls[c("lambda", "mu", "sigma")],
                     list(mu.se = as.vector(Nmix.param.se[["mu.se"]])),
                     Nmix.param.se[c("lambda.se", "sigma.se")]) |>
        as.data.frame()
    } else {
      params.tb <- c(Nmix.ls[c("lambda", "mu", "sigma")]) |>
        as.data.frame()
    }

    # x range usded for prediction
    if (fullrange) {
      # ensure that the component Normals are fully predicted
      x.range <- range(qnorm(p = 0.0005,
                             mean = params.tb[["mu"]],
                             sd = params.tb[["sigma"]],
                             lower.tail = TRUE),
                       qnorm(p = 0.0005,
                             mean = params.tb[["mu"]],
                             sd = params.tb[["sigma"]],
                             lower.tail = FALSE))
    } else {
      # predict the component normals in the data range
      x.range <- range(data[["x"]])
    }

    k <- length(params.tb[["lambda"]])
    prediction <- list()
    prediction[["x"]] <-
      seq(from = x.range[1], to = x.range[2], length.out = n)
    prediction[["comp.sum"]] <- rep(0, n)
    for (i in 1:k) {
      comp.name <- paste("comp", i, sep = ".")
      prediction[[comp.name]] <-
        dnorm(prediction[["x"]],
              mean = params.tb[["mu"]][i],
              sd = params.tb[["sigma"]][i]) * params.tb[["lambda"]][i]
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
      prediction <- subset(prediction, component == "comp.sum")
    } else if (components == "members") {
      prediction <- subset(prediction, component != "comp.sum")
    } else if (components != "all") {
      warning("Ignoring bad 'components' = ", components)
      components <- "all"
    }

    if (fm.values) {
      prediction[["converged"]] <- converged
      prediction[["n.x"]] <- nrow(data)
      prediction[["fm.class"]] <- class(fm)[1]
      prediction[["fm.method"]] <- fm[["ft"]]
    }

    prediction[["flipped_aes"]] <- flipped_aes
    ggplot2::flip_data(prediction, flipped_aes)
  }

#' @rdname ggpmisc-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatNormalmixLine <-
  ggplot2::ggproto("StatNormalmixLine", Stat,
                   setup_params = function(data, params) {
                     params[["flipped_aes"]] <-
                       ggplot2::has_flipped_aes(data, params, ambiguous = TRUE)
                     params
                   },

                   extra_params = c("na.rm", "orientation"),

                   compute_group = poly_normalmix_compute_group_fun,

                   default_aes =
                     ggplot2::aes(y = after_stat(density),
                                  group = after_stat(component)),
                   dropped_aes = c("weight"),
                   required_aes = "x|y"
  )

