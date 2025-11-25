#' Predicted line from Normal mixture model fit
#'
#' \code{stat_distrmix_line()} fits a Normal mixture model, by default with
#' \code{\link[mixtools]{normalmixEM}()}. Predicted values are
#' computed and, by default, plotted.
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
#'   \code{"members"} select which densities are returned.
#' @param n.min integer Minimum number of distinct values in the mapped
#'   variable for fitting to the attempted.
#' @param se Currently ignored.
#' @param fit.seed RNG seed argument passed to \code{\link[base:Random]{set.seed}()}.
#'   Defaults to \code{NA}, which means that \code{set.seed()} will not be
#'   called.
#' @param fm.values logical Add parameter estimates and their standard errors
#'   to the returned values (`FALSE` by default.)
#' @param fullrange Should the prediction span the combined range of the scale
#'   and of the fitted distributions, or just span the range of the data?
#' @param level Level of confidence interval to use (0.95 by default).
#' @param n Number of points at which to evaluate the model prediction.
#' @param orientation character Either "x" or "y", the mapping of the values
#'   to which the mixture model is to be fitetd. NOT YET IMPLEMENTED!
#'
#' @details This statistic is similar to \code{\link[ggplot2]{stat_density}} but
#'   instead of fitting a single distribution it can fit a mixture of two or
#'   more Normal distributions, using an approach related to clustering.
#'   Defaults are consistent between \code{stat_distrmix_line()} and
#'   \code{stat_distrmix_eq()}. Parameter \code{fit.seed} if not \code{NA} is used
#'   in a call to \code{set.seed()} immediately before calling the model fit
#'   function. As the fitting procedure makes use of the (pseudo-)random number
#'   generator (RNG), convergence can depend on it, and in such cases setting
#'   \code{fit.seed} to the same value in \code{\link{stat_distrmix_line}()} and in
#'   \code{\link{stat_distrmix_eq}()} can ensure consistency, and more
#'   generally, reproducibility.
#'
#'   A mixture model as described above, is fitted for \code{k >= 2}, while
#'   \code{k == 1} is treated as a special case and a Normal distribution fitted
#'   with function \code{\link[MASS]{fitdistr}()}. In this case the SE values
#'   are exact estimates.
#'
#' @return The value returned by the statistic is a data frame, with \code{n}
#'   rows of predicted density for each component of the mixture plus their
#'   sum and the corresponding vector of \code{x} values. Optionally it will
#'   also include additional values related to the model fit.
#'
#' @section Computed variables: \code{stat_distrmix_line()} provides the following
#'   variables, some of which depend on the orientation:
#'   \describe{\item{density}{predicted density values}
#'   \item{x}{the \code{n} values for the quantiles}
#'   \item{component}{A factor indexing the components and/or their sum}}
#'
#'   If \code{fm.values = TRUE} is passed then columns with diagnosis and
#'   parameters estimates are added, with the same value in each row within a
#'   group:
#'   \describe{\item{converged}{\code{logical} indicating if convergence was achieved}
#'   \item{n}{\code{numeric} the number of \code{x} values}
#'   \item{.size}{\code{numeric} the number of \code{density} values}
#'   \item{fm.class}{\code{character} the most derived class of the fitted model object}
#'   \item{fm.method}{\code{character} the method, as given by the \code{ft}
#'   field of the fitted model objects}}
#'   This is wasteful and disabled by default, but provides a simple and robust
#'   approach to achieve effects like colouring or hiding of the model fit line
#'   by group depending on the outcome of model fitting.
#'
#' @section Aesthetics: \code{stat_distrmix_eq} expects observations mapped to
#'   \code{x} from a \code{numeric} variable. A new grouping is added by mapping
#'   \code{component} to the \code{group} aesthetic. Additional aesthetics as
#'   understood by the geom (\code{"geom_line"} by default) can be set.
#'
#' @family ggplot statistics for mixture model fits.
#'
#' @examples
#' ggplot(faithful, aes(x = waiting)) +
#'   stat_distrmix_line()
#'
#' # ggplot(faithful, aes(y = waiting)) +
#' #  stat_distrmix_line(orientation = "y")
#'
#' ggplot(faithful, aes(x = waiting)) +
#'   stat_distrmix_line(components = "sum")
#'
#' ggplot(faithful, aes(x = waiting)) +
#'   stat_distrmix_line(components = "members")
#'
#' ggplot(faithful, aes(x = waiting)) +
#'  geom_histogram(aes(y = after_stat(density)), bins = 20) +
#'  stat_distrmix_line(aes(colour = after_stat(component),
#'                          fill = after_stat(component)),
#'                      geom = "area", linewidth = 1, alpha = 0.25, se = FALSE)
#'
#' ggplot(faithful, aes(x = waiting)) +
#'  stat_distrmix_line(aes(colour = after_stat(component),
#'                          fill = after_stat(component)),
#'                      geom = "area", linewidth = 1, alpha = 0.25,
#'                      components = "members", se = FALSE)
#'
#' ggplot(faithful, aes(x = waiting)) +
#'  stat_distrmix_line(geom = "area", linewidth = 1, alpha = 0.25,
#'                      colour = "black", outline.type = "upper",
#'                      components = "sum", se = FALSE)
#'
#' # special case of no mixture
#' ggplot(subset(faithful, waiting > 66), aes(x = waiting)) +
#'   stat_distrmix_line(k = 1)
#'
#' # Inspecting the returned data using geom_debug()
#' gginnards.installed <- requireNamespace("gginnards", quietly = TRUE)
#'
#' if (gginnards.installed)
#'   library(gginnards)
#'
#' if (gginnards.installed)
#'   ggplot(faithful, aes(x = waiting)) +
#'     stat_distrmix_line(geom = "debug", components = "all")
#'
#' if (gginnards.installed)
#'   ggplot(faithful, aes(x = waiting)) +
#'     stat_distrmix_line(geom = "debug", components = "sum")
#'
#' if (gginnards.installed)
#'   ggplot(faithful, aes(x = waiting)) +
#'     stat_distrmix_line(geom = "debug", components = "members")
#'
#' if (gginnards.installed)
#'   ggplot(faithful, aes(x = waiting)) +
#'     stat_distrmix_line(geom = "debug", fm.values = TRUE)
#'
#' @export
#'
stat_distrmix_line <- function(mapping = NULL,
                                data = NULL,
                                geom = "line",
                                position = "identity",
                                ...,
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
                                orientation = "x",
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
    stat = StatNormalmixLine,
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

normalmix_compute_group_fun <-
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

    data <- ggplot2::flip_data(data, flipped_aes)

    if (length(unique(data$x)) < n.min) {
      message("Skipping! Fewer than 'n.min = ", n.min,
              "' unique observations found 'n = ", length(unique(data$x)), "'")
      # Not enough data to perform fit
      return(data.frame())
    }

    fm_params.tb <-
      normalmix_helper_fun(data = data,
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
StatNormalmixLine <-
  ggplot2::ggproto("StatNormalmixLine", ggplot2::Stat,
                   setup_params = function(data, params) {
                     params[["flipped_aes"]] <-
                       ggplot2::has_flipped_aes(data, params, ambiguous = TRUE)
                     params
                   },

                   extra_params = c("na.rm", "orientation"),

                   compute_group = normalmix_compute_group_fun,

                   default_aes =
                     ggplot2::aes(y = after_stat(density),
                                  group = after_stat(component)),
                   dropped_aes = c("weight"),
                   required_aes = "x|y"
  )

#' Helper function for fitting Normal mixture model
#'
#' Factored out code used in both stat_distrmix_line() and stat_distrmix_eq().
#'
#' @inheritParams stat_distrmix_line
#'
#' @keywords internal
#'
#' @details
#' This function does the model fitting and returns a data frame with the
#' estimates for the parameters. It is a wrapper on functions from package
#' 'mixtools'.
#'
normalmix_helper_fun <-
  function(data,
           aes.name = "x",
           method,
           method.name,
           se,
           method.args = list(),
           k = 2,
           free.mean = TRUE,
           free.sd = TRUE,
           n.min = 10L * k,
           fit.seed = NA,
           fm.values = TRUE) {
    if (k == 1) {
      message("With k = 1 one Normal distribution is fitted. Irrelevant parameters ignored!")
      fm <- MASS::fitdistr(data[[aes.name]], "normal")
      # extract fitted parameter estimates
      if (se) {
        fm_params.tb <- data.frame(lambda = 1,
                                   mu = fm[["estimate"]]["mean"],
                                   sigma = fm[["estimate"]]["sd"],
                                   lambda.se = 0,
                                   mu.se = fm[["sd"]]["mean"],
                                   sigma.se = fm[["sd"]]["sd"],
                                   k = k,
                                   row.names = 1L)
      } else {
        fm_params.tb <- data.frame(lambda = 1,
                                   mu = fm[["estimate"]]["mean"],
                                   sigma = fm[["estimate"]]["sd"],
                                   k = k,
                                   row.names = 1L)
      }
      fm_params.tb <- rbind(fm_params.tb, c(1, rep(NA_real_, ncol(fm_params.tb) - 1)))
      if (fm.values) {
        fm_params.tb[["converged"]] <- TRUE
        fm_params.tb[["n"]] <- fm[["n"]]
        fm_params.tb[["fm.class"]] <- class(fm)[1]
        fm_params.tb[["fm.method"]] <- "exact"
      }
    } else {
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
      fun.args <- list(x = data[[aes.name]],
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

      if (!is.na(fit.seed)) {
        set.seed(fit.seed)
      }
      fm <- do.call(method, args = fun.args)

      if (!length(fm) || (is.atomic(fm) && is.na(fm))) {
        return(NA)
      } else if (!inherits(fm, "mixEM")) {
        warning("Skipping! Method \"", method.name,
                "\" did not return a ",
                "\"mixEM\" object, but a \"", class(fm)[1], "\" object")
        return(NA)
      }

      converged <- length(fm[["all.loglik"]]) < maxit

      # extract fitted parameter estimates
      if (se) {
        # using bootstrap, standard errors are computed for the parameter estimates
        # B is the number of "trials" used to estimate the se
        fm.param.se <- mixtools::boot.se(fm, B = 100)
        fm.param.se[grepv(".se$", names(fm.param.se))]

        fm_params.tb <- c(fm[c("lambda", "mu", "sigma")],
                          list(mu.se = as.vector(fm.param.se[["mu.se"]])),
                          fm.param.se[c("lambda.se", "sigma.se")])
      } else {
        fm_params.tb <- c(fm[c("lambda", "mu", "sigma")])
      }
      fm_params.tb <- as.data.frame(fm_params.tb)
      fm_params.tb[["k"]] <- k

      # add row for sum
      fm_params.tb <- rbind(fm_params.tb,
                            c(1,  rep(NA_real_, ncol(fm_params.tb) - 1)))

      if (fm.values) {
        fm_params.tb[["converged"]] <- converged
        fm_params.tb[["n"]] <- nrow(data)
        fm_params.tb[["fm.class"]] <- class(fm)[1]
        fm_params.tb[["fm.method"]] <- fm[["ft"]]
      }
    } # end k > = 2

    # add id column
    fm_params.tb[["component"]] <-
      paste("comp", c(as.character(1:k), "sum"), sep = ".")

    fm_params.tb

  }
