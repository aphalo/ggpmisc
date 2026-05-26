#' Mixture model prediction and annotations
#'
#' Statistics \code{stat_distrmix_line()} and \code{stat_distrmix_eq()} fit a
#' Normal mixture model. While \code{stat_distrmix_line()} adds prediction
#' lines, \code{stat_distrmix_eq()} adds textual labels to a plot.
#'
#' @inheritParams stat_poly_eq
#' @param k integer Number of mixture components to fit.
#' @param free.mean,free.sd logical If TRUE, allow the fitted \code{mean} and/or
#'   fitted \code{sd} to vary among the component Normal distributions.
#' @param components character One of \code{"all"}, \code{"sum"}, or
#'   \code{"members"} select which densities are returned.
#' @param se Currently ignored.
#' @param fm.values logical Add parameter estimates and their standard errors
#'   to the returned values (`FALSE` by default.)
#' @param eq.digits integer Number of digits after the decimal point to
#'   use for parameters in labels. If \code{Inf}, use exponential
#'   notation with three decimal places.
#'
#' @aesthetics StatDistrmixEq
#' @aesthetics StatDistrmixLine
#'
#' @return The value returned by the statistic is a data frame, with \code{n}
#'   rows of predicted density for each component of the mixture plus their
#'   sum and the corresponding vector of \code{x} values. Optionally it will
#'   also include additional values related to the model fit.
#'
#' @details \code{stat_distrmix_line()} is similar to
#'   \code{\link[ggplot2]{stat_density}} but in addition to fitting a single
#'   distribution it can fit a mixture of two or more Normal distributions,
#'   using an approach related to clustering. Defaults are consistent between
#'   \code{stat_distrmix_line()} and \code{stat_distrmix_eq()}.
#'   \code{stat_distrmix_eq()} can be used to add matched textual annotations.
#'
#'   If \code{k >= 2} a mixture of Normals model is fitted with
#'   \code{\link[mixtools]{normalmixEM}()}, while if \code{k == 1} a single
#'   Normal distribution is fitted with function \code{\link[MASS]{fitdistr}()}.
#'   Only for \code{k == 1} the SE values are exact estimates.
#'
#'   Parameter \code{fit.seed} if not \code{NA} is used in a call to
#'   \code{set.seed()} immediately before calling the model fit function. As the
#'   fitting procedure makes use of the (pseudo-)random number generator (RNG),
#'   convergence can depend on it, and in such cases setting \code{fit.seed} to
#'   the same value in \code{stat_distrmix_line()} and in
#'   \code{stat_distrmix_eq()} can ensure consistency, and more generally,
#'   reproducibility.
#'
#'   The minimum number of observations with distinct values in the explanatory
#'   variable can be set through parameter \code{n.min}. The default depends on
#'   \code{k}, the number of components in the mix. Model fits with too few
#'   observations are unreliable, thus, using larger values of \code{n.min} than
#'   the default is wise.
#'
#' @return The value returned by \code{stat_distrmix_line()} is a data frame, with \code{n}
#'   rows of predicted density for each component of the mixture plus their
#'   sum and the corresponding vector of \code{x} values.
#'
#'   The value returned by \code{stat_distrmix_eq()} is a data frame, with one
#'   row of estimates for each group of data in the plot.
#'
#'   Both statistics optionally also return additional values related to the
#'   model fit.
#'
#' @inheritSection check_output_type Output types
#'
#' @inheritSection stat_poly_eq Position of labels
#'
#' @section Variables computed by \code{stat_distrmix_line()}:
#'   Some of the returned variables depend on the orientation.
#'
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
#'   This provides a simple and robust
#'   approach to achieve effects like colouring or hiding annotations
#'   by group depending on the outcome of model fitting.
#'
#' @section Variables computed by \code{stat_distrmix_eq()}:
#'   Some of the variables depend on the orientation:
#'   \describe{\item{x}{the location of text labels}
#'   \item{y}{the location of text labels}
#'   \item{eq.label}{\code{character} string for equations}
#'   \item{n.label}{\code{character} string for number of observations}
#'   \item{method.label}{\code{character} string for model fit method}
#'   \item{lambda}{\code{numeric} the estimate of the contribution of the
#'  component of the mixture towards the joint density}
#'   \item{mu}{\code{numeric} the estimate of the mean}
#'   \item{sigma}{\code{numeric} the estimate of the standard deviation}
#'   \item{component}{A factor indexing the components of the mixture and/or
#'   their sum}}
#'
#'   If \code{SE = TRUE} is passed then columns with standard errors for the
#'   parameter estimates:
#'   \describe{\item{lambda.se}{\code{numeric} the estimate of the contribution
#'   of the component of the mixture towards the joint density}
#'   \item{mu.se}{\code{numeric} the estimate of the mean}
#'   \item{sigma.se}{\code{numeric} the estimate of the standard deviation}}
#'
#'   If \code{fm.values = TRUE} the same additional columns are returned as by
#'   \code{stat_distrmix_eq()}. This is wasteful of storage space as values are
#'   stored in multiple copies and, thus, disabled by default. However, it
#'   provides a simple and robust approach to achieve effects like colouring or
#'   hiding of the model fit line by group depending on the outcome of model
#'   fitting.
#'
#' @inherit stat_poly_eq seealso
#'
#' @family 'ggpmisc' statistics for model fits
#'
#' @examples
#' ggplot(faithful, aes(x = waiting)) +
#'   stat_distrmix_line(components = "sum") +
#'   stat_distrmix_eq()
#'
#' ggplot(faithful, aes(x = waiting)) +
#'   stat_distrmix_line(components = "sum") +
#'   stat_distrmix_eq(use_label("eq", "n", "method"))
#'
#' ggplot(faithful, aes(x = waiting)) +
#'   stat_distrmix_line(components = "sum") +
#'   stat_distrmix_eq(geom = "label_npc")
#'
#' ggplot(faithful, aes(x = waiting)) +
#'   stat_distrmix_line(components = "sum") +
#'   stat_distrmix_eq(geom = "text", label.x = "center", label.y = "bottom")
#'
#' ggplot(faithful, aes(x = waiting)) +
#'   stat_distrmix_line(components = "sum") +
#'   stat_distrmix_eq(geom = "text", hjust = "inward")
#'
#' ggplot(faithful, aes(x = waiting)) +
#'   stat_distrmix_line(components = "members") +
#'   stat_distrmix_eq(components = "members")
#'
#' ggplot(faithful, aes(x = waiting)) +
#'   stat_distrmix_line(components = "members") +
#'   stat_distrmix_eq(components = "members", se = TRUE)
#'
#' # ggplot(faithful, aes(y = waiting)) +
#' #  stat_distrmix_eq(orientation = "y")
#'
#' ggplot(faithful, aes(x = waiting)) +
#'  geom_histogram(aes(y = after_stat(density)), bins = 20) +
#'  stat_distrmix_line(aes(colour = after_stat(component),
#'                          fill = after_stat(component)),
#'                      geom = "area", linewidth = 1, alpha = 0.25) +
#'  stat_distrmix_eq(aes(colour = after_stat(component)))
#'
#' ggplot(faithful, aes(x = waiting)) +
#'  stat_distrmix_line(aes(colour = after_stat(component),
#'                          fill = after_stat(component)),
#'                      geom = "area", linewidth = 1, alpha = 0.25,
#'                      components = "members") +
#'  stat_distrmix_eq(aes(colour = after_stat(component)),
#'                      components = "members")
#'
#' ggplot(faithful, aes(x = waiting)) +
#'  stat_distrmix_line(geom = "area", linewidth = 1, alpha = 0.25,
#'                      colour = "black", outline.type = "upper",
#'                      components = "sum", se = FALSE) +
#'  stat_distrmix_eq(components = "sum")
#'
#' # special case of no mixture
#' ggplot(subset(faithful, waiting > 66), aes(x = waiting)) +
#'   stat_distrmix_line(k = 1) +
#'   stat_distrmix_eq(k = 1)
#'
#' ggplot(subset(faithful, waiting > 66), aes(x = waiting)) +
#'   stat_distrmix_line(k = 1) +
#'   stat_distrmix_eq(k = 1, se = TRUE)
#'
#' # Inspecting the returned data using geom_debug_group()
#' gginnards.installed <- requireNamespace("gginnards", quietly = TRUE)
#'
#' if (gginnards.installed)
#'   library(gginnards)
#'
#' if (gginnards.installed)
#'   ggplot(faithful, aes(x = waiting)) +
#'     stat_distrmix_line(geom = "debug_group", components = "all")
#'     stat_distrmix_eq(geom = "debug_group", components = "all")
#'
#' if (gginnards.installed)
#'   ggplot(faithful, aes(x = waiting)) +
#'     stat_distrmix_eq(geom = "debug_group", components = "sum")
#'
#' if (gginnards.installed)
#'   ggplot(faithful, aes(x = waiting)) +
#'     stat_distrmix_eq(geom = "debug_group", components = "members")
#'
#' if (gginnards.installed)
#'   ggplot(faithful, aes(x = waiting)) +
#'     stat_distrmix_eq(geom = "debug_group",
#'                       components = "members",
#'                       fm.values = TRUE)
#'
#' @export
#'
stat_distrmix_eq <- function(mapping = NULL,
                             data = NULL,
                             geom = "text_npc",
                             position = "identity",
                             ...,
                             orientation = "x",
                             method = "normalmixEM",
                             method.args = list(),
                             n.min = 10L * k,
                             level = 0.95,
                             k = 2,
                             free.mean = TRUE,
                             free.sd = TRUE,
                             se = FALSE,
                             fit.seed = NA,
                             fm.values = TRUE,
                             components = NULL,
                             eq.with.lhs = TRUE,
                             eq.digits = 2,
                             label.x = "left",
                             label.y = "top",
                             hstep = 0,
                             vstep = NULL,
                             output.type = NULL,
                             na.rm = FALSE,
                             parse = NULL,
                             show.legend = NA,
                             inherit.aes = TRUE) {
  stopifnot("Arg 'x' should not be passed in 'method.args'!" =
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

  if (is.null(eq.with.lhs) || anyNA(eq.with.lhs)) {
    eq.with.lhs = FALSE
  } else if (!(is.logical(eq.with.lhs) || is.character(eq.with.lhs))) {
    stop("'eq.with.lhs' must be 'logical' or 'character', not '", mode(eq.with.lhs), "'")
  }

  if (is.null(se)) {
    se <- FALSE
  }

  if (is.null(k)) {
    k <- 2
  } else if (k < 1) {
    stop("Expected k >= 1, but k = ", k)
  }

  output.type <-
    check_output_type(output.type = output.type, geom = geom)

  if (is.null(components)) {
    components <- ifelse(output.type == "numeric", "members", "sum")
  }

  if (is.null(parse)) {
    parse <- output.type == "expression"
  }

  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = StatDistrmixEq,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = rlang::list2(
      method = method,
      method.name = method.name,
      se = se,
      fit.seed = fit.seed,
      level = level,
      method.args = method.args,
      k = k,
      free.mean = free.mean,
      free.sd = free.sd,
      components = components,
      n.min = n.min,
      eq.with.lhs = eq.with.lhs,
      eq.digits = eq.digits,
      label.x = label.x,
      label.y = label.y,
      hstep = hstep,
      vstep = ifelse(is.null(vstep),
                     ifelse(grepl("label", geom),
                            0.10,
                            0.05),
                     vstep),
      npc.used = grepl("_npc", geom),
      output.type = output.type,
      na.rm = na.rm,
      orientation = orientation,
      parse = parse,
      ...
    )
  )
}

distrmix_eq_compute_group_fun <-
  function(data,
           scales,
           method,
           method.name,
           se = FALSE,
           fit.seed = NA,
           eq.with.lhs = TRUE,
           xseq = NA,
           level = 0.95,
           method.args = list(),
           k = 2,
           free.mean = TRUE,
           free.sd = TRUE,
           components = NA,
           n.min = 10L * k,
           flipped_aes = NA,
           eq.digits = 2,
           eq.format = NULL, # not yet a user visible parameter
           label.x = "left",
           label.y = "top",
           hstep = 0,
           vstep = 0.1,
           npc.used = TRUE,
           output.type = "expression",
           parse = FALSE,
           na.rm = FALSE,
           orientation = "x") {

    rlang::check_installed("mixtools", reason = "to use stat_distrmix_eq()")

    if (length(unique(data[[orientation]])) < n.min) {
      message("Skipping! Fewer than 'n.min = ", n.min,
              "' unique observations found 'n = ",
              length(unique(data[[orientation]])), "'")
      # Not enough data to perform fit
      return(data.frame())
    }

    output.type <- if (!length(output.type)) {
      "expression"
    } else {
      tolower(output.type)
    }
    stopifnot(output.type %in%
                c("expression", "text", "markdown", "numeric", "latex", "tex", "tikz"))

    fm_params.tb <-
      distrmix_helper_fun(data = data,
                           aes.name = orientation,
                           method = method,
                           method.name = method.name,
                           se = se,
                           method.args = method.args,
                           k = k,
                           free.mean = free.mean,
                           free.sd = free.sd,
                           n.min = n.min,
                           fit.seed = fit.seed,
                           fm.values = TRUE) # values are used in labels!

    if (length(fm_params.tb) == 1L && is.na(fm_params.tb)) {
      # model fitting was skipped or failed
      return(data.frame())
    }

    # update k in case it was modified during fitting
    k <- fm_params.tb[["k"]][1]

    if (output.type != "numeric") {
      # generate labels
      eq.label <- character(k + 1)
      if (se) {
        eq.format <-
          "%.*f*(%.*g) %%*%% italic(N)(mu*`=`*%.*g*(%.*g), sigma*`=`*%.*g*(%.*g))"
        for (i in 1:k) {
          eq.label[i] <- sprintf(eq.format,
                                 eq.digits, fm_params.tb[["lambda"]][i],
                                 eq.digits, fm_params.tb[["lambda.se"]][i],
                                 eq.digits, fm_params.tb[["mu"]][i],
                                 eq.digits, fm_params.tb[["mu.se"]][i],
                                 eq.digits, fm_params.tb[["sigma"]][i],
                                 eq.digits, fm_params.tb[["sigma.se"]][i])
        }
      } else {
        eq.format <- "%.*g %%*%% italic(N)(mu*`=`*%.*g, sigma*`=`*%.*g)"
        for (i in 1:k) {
          eq.label[i] <- sprintf(eq.format,
                                 eq.digits, fm_params.tb[["lambda"]][i],
                                 eq.digits, fm_params.tb[["mu"]][i],
                                 eq.digits, fm_params.tb[["sigma"]][i])
        }
      }
      if (is.logical(eq.with.lhs)) {
        if (eq.with.lhs) {
          lhs <- "DF~`=`~"
        } else {
          lhs = ""
        }
      } else if (is.character(eq.with.lhs)) {
        lhs <- eq.with.lhs
      }

      eq.label[k + 1] <- paste(eq.label[-(k + 1)], collapse = " + ", sep = "")
      fm_params.tb[["eq.label"]] <- paste(lhs, eq.label, sep = "")
      fm_params.tb[["n.label"]] <- paste("n~`=`~", fm_params.tb[["n"]], sep = "")
      fm_params.tb[["method.label"]] <-
        paste("\"method: ", fm_params.tb[["fm.method"]], "\"", sep = "")
    }

    if (components == "sum") {
      selector <- which(fm_params.tb[["component"]] == "comp.sum")
      fm_params.tb <- fm_params.tb[selector, ]
    } else if (components == "members") {
      selector <- which(fm_params.tb[["component"]] != "comp.sum")
      fm_params.tb <- fm_params.tb[selector, ]
    } else if (components != "all") {
      warning("Ignoring bad 'components' argument: \"", components, "\"")
    }

    # Compute label positions
    if (is.character(label.x)) {
      margin.npc <- 0.05
      label.x <- ggpp::compute_npcx(x = label.x,
                                    group = seq_along(fm_params.tb[["component"]]),
                                    h.step = hstep,
                                    margin.npc = margin.npc)
    }
    if (is.character(label.y)) {
      margin.npc <- 0.05
      label.y <- ggpp::compute_npcy(y = label.y,
                                    group = seq_along(fm_params.tb[["component"]]),
                                    v.step = vstep,
                                    margin.npc = margin.npc)
    }

    if (npc.used) {
      fm_params.tb$npcx <- label.x
      fm_params.tb$x <- NA_real_
      fm_params.tb$npcy <- label.y
      fm_params.tb$y <- NA_real_
    } else {
      fm_params.tb$npcx <- NA_real_
      fm_params.tb$x <- I(label.x)
      fm_params.tb$npcy <- NA_real_
      fm_params.tb$y <- I(label.y)
    }

    fm_params.tb
  }

#' @rdname ggpmisc-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatDistrmixEq <-
  ggplot2::ggproto("StatDistrmixEq", ggplot2::Stat,

                   extra_params = c("na.rm", "parse", "orientation"),

                   setup_params = function(data, params) {
                     params[["flipped_aes"]] <-
                       ggplot2::has_flipped_aes(data, params, ambiguous = TRUE)
                     params
                   },

                   compute_group = distrmix_eq_compute_group_fun,

                   default_aes =
                     ggplot2::aes(npcx = after_stat(npcx),
                                  npcy = after_stat(npcy),
                                  label = after_stat(eq.label),
                                  hjust = "inward",
                                  vjust = "inward",
                                  group = after_stat(component)),
                   required_aes = "x|y"
  )
