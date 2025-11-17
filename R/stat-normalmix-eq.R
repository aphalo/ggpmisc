#' Predicted equation from Normal mixture model fit
#'
#' \code{stat_normalmix_eq()} fits a Normal mixture model, by default with
#' \code{\link[mixtools]{normalmixEM}()}. Predicted values are
#' computed and, by default, plotted.
#'
#' @inheritParams stat_normalmix_line
#'
#' @param eq.with.lhs If \code{character} the string is pasted to the front of
#'   the equation label before parsing or a \code{logical} (see note).
#' @param se logical, if \code{TRUE} standard errors for parameter estimates
#'   are obtained by bootstrapping.
#' @param level Level of confidence interval to use (0.95 by default).
#' @param eq.digits integer Number of digits after the decimal point to
#'   use for parameters in labels. If \code{Inf}, use exponential
#'   notation with three decimal places.
#' @param label.x,label.y \code{numeric} with range 0..1 "normalized parent
#'   coordinates" (npc units) or character if using \code{geom_text_npc()} or
#'   \code{geom_label_npc()}. If using \code{geom_text()} or \code{geom_label()}
#'   numeric in native data units. If too short they will be recycled.
#' @param hstep,vstep numeric in npc units, the horizontal and vertical step
#'   used between labels for different mixture model components.
#' @param output.type character One of "expression", "LaTeX", "text",
#'   "markdown" or "numeric".
#' @param parse logical Passed to the geom. If \code{TRUE}, the labels will be
#'   parsed into expressions and displayed as described in \code{?plotmath}.
#'   Default is \code{TRUE} if \code{output.type = "expression"} and
#'   \code{FALSE} otherwise.
#'
#' @return The value returned by the statistic is a data frame, with \code{n}
#'   rows of predicted density for each component of the mixture plus their
#'   sum and the corresponding vector of \code{x} values. Optionally it will
#'   also include additional values related to the model fit.
#'
#' @inherit stat_normalmix_line details
#'
#' @section Computed variables: \code{stat_normalmix_eq()} provides the
#'   following
#'   variables, some of which depend on the orientation:
#'   \describe{\item{x}{the location of text labels}
#'   \item{y}{the location of text labels}
#'   \item{eq.label}{\code{character} string for equations}
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
#'   If \code{fm.values = TRUE} is passed then columns with diagnosis and
#'   parameters estimates are added, with the same value in each row within a
#'   group:
#'   \describe{\item{converged}{\code{logical} indicating if convergence was
#'   achieved}
#'   \item{n}{\code{numeric} the number of \code{x} values}
#'   \item{.size}{\code{numeric} the number of \code{density} values}
#'   \item{fm.class}{\code{character} the most derived class of the fitted model
#'    object}
#'   \item{fm.method}{\code{character} the method, as given by the \code{ft}
#'   field of the fitted model objects}}
#'   This is wasteful and disabled by default, but provides a simple and robust
#'   approach to achieve effects like colouring or hiding of the model fit line
#'   by group depending on the outcome of model fitting.
#'
#' @section Aesthetics: \code{stat_normalmix_eq} expects observations mapped to
#'   \code{x} from a \code{numeric} variable. A new grouping is added by mapping
#'   as default \code{component} to the \code{group} aesthetic and
#'   \code{eq.label} to the label aesthetic. Additional aesthetics as
#'   understood by the geom (\code{"text_npc"} by default) can be set.
#'
#' @family ggplot statistics for mixture model fits.
#'
#' @examples
#' ggplot(faithful, aes(x = waiting)) +
#'   stat_normalmix_line(components = "sum") +
#'   stat_normalmix_eq()
#'
#' ggplot(faithful, aes(x = waiting)) +
#'   stat_normalmix_line(components = "sum") +
#'   stat_normalmix_eq(geom = "label_npc")
#'
#' ggplot(faithful, aes(x = waiting)) +
#'   stat_normalmix_line(components = "sum") +
#'   stat_normalmix_eq(geom = "text", label.x = "center", label.y = "bottom")
#'
#' ggplot(faithful, aes(x = waiting)) +
#'   stat_normalmix_line(components = "sum") +
#'   stat_normalmix_eq(geom = "text", hjust = "inward")
#'
#' ggplot(faithful, aes(x = waiting)) +
#'   stat_normalmix_line(components = "members") +
#'   stat_normalmix_eq(components = "members")
#'
#' ggplot(faithful, aes(x = waiting)) +
#'   stat_normalmix_line(components = "members") +
#'   stat_normalmix_eq(components = "members", se = TRUE)
#'
#' # ggplot(faithful, aes(y = waiting)) +
#' #  stat_normalmix_eq(orientation = "y")
#'
#' ggplot(faithful, aes(x = waiting)) +
#'  geom_histogram(aes(y = after_stat(density)), bins = 20) +
#'  stat_normalmix_line(aes(colour = after_stat(component),
#'                          fill = after_stat(component)),
#'                      geom = "area", linewidth = 1, alpha = 0.25) +
#'  stat_normalmix_eq(aes(colour = after_stat(component)))
#'
#' ggplot(faithful, aes(x = waiting)) +
#'  stat_normalmix_line(aes(colour = after_stat(component),
#'                          fill = after_stat(component)),
#'                      geom = "area", linewidth = 1, alpha = 0.25,
#'                      components = "members") +
#'  stat_normalmix_eq(aes(colour = after_stat(component)),
#'                      components = "members")
#'
#' ggplot(faithful, aes(x = waiting)) +
#'  stat_normalmix_line(geom = "area", linewidth = 1, alpha = 0.25,
#'                      colour = "black", outline.type = "upper",
#'                      components = "sum", se = FALSE) +
#'  stat_normalmix_eq(components = "sum")
#'
#' # Inspecting the returned data using geom_debug()
#' gginnards.installed <- requireNamespace("gginnards", quietly = TRUE)
#'
#' if (gginnards.installed)
#'   library(gginnards)
#'
#' if (gginnards.installed)
#'   ggplot(faithful, aes(x = waiting)) +
#'     stat_normalmix_line(geom = "debug", components = "all")
#'     stat_normalmix_eq(geom = "debug", components = "all")
#'
#' if (gginnards.installed)
#'   ggplot(faithful, aes(x = waiting)) +
#'     stat_normalmix_eq(geom = "debug", components = "sum")
#'
#' if (gginnards.installed)
#'   ggplot(faithful, aes(x = waiting)) +
#'     stat_normalmix_eq(geom = "debug", components = "members")
#'
#' if (gginnards.installed)
#'   ggplot(faithful, aes(x = waiting)) +
#'     stat_normalmix_eq(geom = "debug",
#'                       components = "members",
#'                       fm.values = TRUE)
#'
#' @export
#'
stat_normalmix_eq <- function(mapping = NULL,
                              data = NULL,
                              geom = "text_npc",
                              position = "identity",
                              ...,
                              method = "normalmixEM",
                              method.args = list(),
                              n.min = 10L * k,
                              level = 0.95,
                              k = 2,
                              free.mean = TRUE,
                              free.sd = TRUE,
                              se = FALSE,
                              seed = NA,
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
                              orientation = "x",
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

  if (method.name != "normalmixEM") {
    stop("Only method currently supported is \"normalmixEM\"")
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
  } else if (k < 2) {
    stop("Expected k >= 2, but k = ", k)
  }

  if (is.null(output.type)) {
    if (geom %in% c("richtext", "textbox", "marquee")) {
      output.type <- "markdown"
    } else {
      output.type <- "expression"
    }
  }

  if (is.null(components)) {
    components <- ifelse(output.type == "numeric", "members", "sum")
  }

  if (is.null(parse)) {
    parse <- output.type == "expression"
  }

  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = StatNormalmixEq,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = rlang::list2(
      method = method,
      method.name = method.name,
      se = se,
      seed = seed,
      level = level,
      na.rm = na.rm,
      orientation = orientation,
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

normalmix_eq_compute_group_fun <-
  function(data,
           scales,
           method,
           method.name,
           se = FALSE,
           seed = NA,
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
           label.x = "left",
           label.y = "top",
           hstep = 0,
           vstep = 0.1,
           npc.used = TRUE,
           output.type = "expression",
           parse = FALSE,
           na.rm = FALSE,
           orientation = "x") {

    force(data)

    if (orientation == "x") {
      if (length(unique(data$x)) < n.min) {
        return(data.frame())
      }
    } else if (orientation == "y") {
      if (length(unique(data$y)) < n.min) {
        return(data.frame())
      }
    }

    output.type <- if (!length(output.type)) {
      "expression"
    } else {
      tolower(output.type)
    }
    stopifnot(output.type %in%
                c("expression", "text", "markdown", "numeric", "latex", "tex", "tikz"))

    params.tb <-
      normalmix_helper_fun(data = data,
                           aes.name = orientation,
                           method = method,
                           method.name = method.name,
                           se = se,
                           method.args = method.args,
                           k = k,
                           free.mean = free.mean,
                           free.sd = free.sd,
                           n.min = n.min,
                           seed = seed,
                           fm.values = TRUE)

    # update k in case it was modified during fitting
    k <- params.tb[["k"]][1]
    # add id column
    params.tb["component"] <- paste("comp", c(as.character(1:k), "sum"), sep = ".")

    if (output.type != "numeric") {
      # generate labels
      eq.format <- NULL
      eq.label <- character(k + 1L)
      if (se) {
        if (is.null(eq.format)) {
          eq.format <-
            "%.*f*(%.*#g) %%*%% italic(N)(mu*`=`*%.*#g*(%.*#g), sigma*`=`*%.*#g*(%.*#g))"
        }
        for (i in 1:k) {
          eq.label[i] <- sprintf(eq.format,
                                 eq.digits, params.tb[["lambda"]][i],
                                 eq.digits, params.tb[["lambda.se"]][i],
                                 eq.digits, params.tb[["mu"]][i],
                                 eq.digits, params.tb[["mu.se"]][i],
                                 eq.digits, params.tb[["sigma"]][i],
                                 eq.digits, params.tb[["sigma.se"]][i])
        }
      } else {
        if (is.null(eq.format)) {
          eq.format <- "%.*#g %%*%% italic(N)(mu*`=`*%.*#g, sigma*`=`*%.*#g)"
        }
        for (i in 1:k) {
          eq.label[i] <- sprintf(eq.format,
                                 eq.digits, params.tb[["lambda"]][i],
                                 eq.digits, params.tb[["mu"]][i],
                                 eq.digits, params.tb[["sigma"]][i])
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
      params.tb[["eq.label"]] <- paste(lhs, eq.label, sep = "")
    }

    if (components == "sum") {
      selector <- which(params.tb[["component"]] == "comp.sum")
      params.tb <- params.tb[selector, ]
    } else if (components == "members") {
      selector <- which(params.tb[["component"]] != "comp.sum")
      params.tb <- params.tb[selector, ]
    } else if (components != "all") {
      warning("Ignoring bad 'components' argument: \"", components, "\"")
    }

    # Compute label positions
    if (is.character(label.x)) {
      margin.npc <- 0.05
      label.x <- ggpp::compute_npcx(x = label.x,
                                    group = seq_along(params.tb[["component"]]),
                                    h.step = hstep,
                                    margin.npc = margin.npc)
    }
    if (is.character(label.y)) {
      margin.npc <- 0.05
      label.y <- ggpp::compute_npcy(y = label.y,
                                    group = seq_along(params.tb[["component"]]),
                                    v.step = vstep,
                                    margin.npc = margin.npc)
    }

    if (npc.used) {
      params.tb$npcx <- label.x
      params.tb$x <- NA_real_
      params.tb$npcy <- label.y
      params.tb$y <- NA_real_
    } else {
      params.tb$npcx <- NA_real_
      params.tb$x <- I(label.x)
      params.tb$npcy <- NA_real_
      params.tb$y <- I(label.y)
    }

    params.tb
  }

#' @rdname ggpmisc-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatNormalmixEq <-
  ggplot2::ggproto("StatNormalmixEq", Stat,
                   extra_params = c("na.rm", "orientation"),

                   setup_params = function(data, params) {
                     params[["flipped_aes"]] <-
                       ggplot2::has_flipped_aes(data, params, ambiguous = TRUE)
                     params
                   },

                   compute_group = normalmix_eq_compute_group_fun,

                   default_aes =
                     ggplot2::aes(npcx = after_stat(npcx),
                                  npcy = after_stat(npcy),
                                  label = after_stat(eq.label),
                                  hjust = "inward",
                                  vjust = "inward",
                                  group = after_stat(component)),
                   required_aes = "x|y"
  )

