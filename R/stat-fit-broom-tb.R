# generics::tidy as tibble -----------------------------------------------------

#' @title Model-fit summary or ANOVA
#'
#' @description \code{stat_fit_tb} fits a model and returns a "tidy" version of
#'   the model's summary or ANOVA table, using '\code{tidy()} methods from
#'   packages 'broom', 'broom.mixed', or other 'broom' extensions. The
#'   annotation is added to the plots in tabular form.
#'
#' @param mapping The aesthetic mapping, usually constructed with
#'   \code{\link[ggplot2]{aes}}. Only needs to be
#'   set at the layer level if you are overriding the plot defaults.
#' @param data A layer specific dataset, only needed if you want to override the
#'   plot defaults.
#' @param geom The geometric object to use display the data
#' @param position The position adjustment to use for overlapping points on this
#'   layer
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
#' @param method character.
#' @param method.args,tidy.args lists of arguments to pass to \code{method} and
#'   to \code{tidy()}.
#' @param n.min integer Minimum number of distinct values in the explanatory
#'   variable (on the rhs of formula) for fitting to the attempted.
#' @param tb.type character One of "fit.summary", "fit.anova" or "fit.coefs".
#' @param digits integer indicating the number of significant digits to be used
#'   for all numeric values in the table.
#' @param p.digits integer indicating the number of decimal places to round
#'   p-values to, with those rounded to zero displayed as the next larger
#'   possible value preceded by "<". If \code{p.digits} is outside the range
#'   1..22 no rounding takes place.
#' @param tb.vars,tb.params character or numeric vectors, optionally named, used
#'   to select and/or rename the columns or the parameters in the table
#'   returned.
#' @param label.x,label.y \code{numeric} Coordinates in data units or with range
#'   0..1, expressed in "normalized parent coordinates" or as character strings
#'   depending on the geometry used. If too short they will be recycled. They set
#'   the \code{x} and \code{y} coordinates at the \code{after_stat} stage.
#' @param table.theme NULL, list or function A 'gridExtra' \code{ttheme}
#'   definition, or a constructor for a \code{ttheme} or NULL for default.
#' @param table.rownames,table.colnames logical flag to enable or disabling
#'   printing of row names and column names.
#' @param table.hjust numeric Horizontal justification for the core and column
#'   headings of the table.
#' @param parse If TRUE, the labels will be parsed into expressions and
#'   displayed as described in \code{?plotmath}.
#'
#' @details \code{stat_fit_tb()} Applies a model fitting function per panel,
#'   using the grouping factors from aesthetic mappings in the fitted model.
#'   This is suitable, for example for analysis of variance used to test for
#'   differences among groups.
#'
#'   The argument to \code{method} can be any fit method for which a suitable
#'   \code{tidy()} method is available, including non-linear regression. Fit
#'   methods retain their default arguments unless overridden.
#'
#'   A ggplot statistic receives as data a data frame that is not the one passed
#'   as argument by the user, but instead a data frame with the variables mapped
#'   to aesthetics. In other words, it respects the grammar of graphics and
#'   consequently within arguments passed through \code{method.args} names of
#'   aesthetics like \eqn{x} and \eqn{y} should be used instead of the original
#'   variable names. The plot's default \code{data} is used by default, which
#'   helps ensure that the model is fitted to the same data as plotted in other
#'   layers.
#'
#' @section Computed variables: The output of \code{tidy()} is returned as a
#'   single "cell" in a tibble (i.e., a tibble nested within a tibble). The
#'   returned \code{data} object contains a single tibble, containing the result
#'   from a single model fit to all data in a panel. If grouping is present, it
#'   is ignored in the sense of returning a single table, but the grouping
#'   aesthetic can be a term in the fitted model.
#'
#' @return A tibble with columns named \code{fm.tb} (a tibble returned by
#'   \code{tidy()} with possibly renamed and subset columns and rows, within a
#'   list), \code{fm.tb.type} (copy of argument passed to \code{tb.type}),
#'   \code{fm.class} (the class of the fitted model object), \code{fm.method}
#'   (the fit function's name), \code{fm.call} (the call if available), \code{x}
#'   and \code{y}.
#'
#'   To explore the values returned by this statistic, which vary depending on
#'   the model fitting function and model formula we suggest the use of
#'   \code{\link[gginnards]{geom_debug}}.
#'
#' @seealso \code{\link[broom]{broom}}, \code{broom.mixed}, and
#'   \code{\link[broom]{tidy}} for details on how the tidying of the result of
#'   model fits is done. See \code{\link[ggpp]{geom_table}} for details on how
#'   inset tables respond to mapped aesthetics and table themes. For details on
#'   predefined table themes see \code{\link[ggpp]{ttheme_gtdefault}}.
#'
#' @family ggplot statistics for model fits
#'
#' @export
#'
#' @examples
#' # Package 'broom' needs to be installed to run these examples.
#' # We check availability before running them to avoid errors.
#' broom.installed <- requireNamespace("broom", quietly = TRUE)
#'
#' if (broom.installed)
#'   library(broom)
#'
#' # data for examples
#'   x <- c(44.4, 45.9, 41.9, 53.3, 44.7, 44.1, 50.7, 45.2, 60.1)
#'   covariate <- sqrt(x) + rnorm(9)
#'   group <- factor(c(rep("A", 4), rep("B", 5)))
#'   my.df <- data.frame(x, group, covariate)
#'
#' gginnards.installed  <- requireNamespace("gginnards", quietly = TRUE)
#'
#' if (gginnards.installed)
#'   library(gginnards)
#'
#' ## covariate is a numeric or continuous variable
#' # Linear regression fit summary, all defaults
#' if (broom.installed)
#'   ggplot(my.df, aes(covariate, x)) +
#'     geom_point() +
#'     stat_fit_tb() +
#'     expand_limits(y = 70)
#'
#' # we can use geom_debug() and str() to inspect the returned value
#' # and discover the variables that can be mapped to aesthetics with
#' # after_stat()
#' if (broom.installed && gginnards.installed)
#'   ggplot(my.df, aes(covariate, x)) +
#'     geom_point() +
#'     stat_fit_tb(geom = "debug", summary.fun = str) +
#'     expand_limits(y = 70)
#'
#' # Linear regression fit summary, with default formatting
#' if (broom.installed)
#'   ggplot(my.df, aes(covariate, x)) +
#'     geom_point() +
#'     stat_fit_tb(tb.type = "fit.summary") +
#'     expand_limits(y = 70)
#'
#' # Linear regression fit summary, with manual table formatting
#' if (broom.installed)
#'   ggplot(my.df, aes(covariate, x)) +
#'     geom_point() +
#'     stat_fit_tb(digits = 2,
#'                 p.digits = 4,
#'                 tb.params = c("intercept" = 1, "covariate" = 2),
#'                 tb.vars = c(Term = 1, Estimate = 2,
#'                             "italic(s)" = 3, "italic(t)" = 4,
#'                             "italic(P)" = 5),
#'                 parse = TRUE) +
#'     expand_limits(y = 70)
#'
#' # Linear regression ANOVA table, with default formatting
#' if (broom.installed)
#'   ggplot(my.df, aes(covariate, x)) +
#'     geom_point() +
#'     stat_fit_tb(tb.type = "fit.anova") +
#'     expand_limits(y = 70)
#'
#' # Linear regression ANOVA table, with manual table formatting
#' if (broom.installed)
#'   ggplot(my.df, aes(covariate, x)) +
#'     geom_point() +
#'     stat_fit_tb(tb.type = "fit.anova",
#'                 tb.params = c("Covariate" = 1, 2),
#'                 tb.vars = c(Effect = 1, d.f. = 2,
#'                             "M.S." = 4, "italic(F)" = 5,
#'                             "italic(P)" = 6),
#'                 parse = TRUE) +
#'     expand_limits(y = 67)
#'
#' # Linear regression fit coeficients, with default formatting
#' if (broom.installed)
#'   ggplot(my.df, aes(covariate, x)) +
#'     geom_point() +
#'     stat_fit_tb(tb.type = "fit.coefs") +
#'     expand_limits(y = 67)
#'
#' # Linear regression fit coeficients, with manual table formatting
#' if (broom.installed)
#'   ggplot(my.df, aes(covariate, x)) +
#'     geom_point() +
#'     stat_fit_tb(tb.type = "fit.coefs",
#'                 tb.params = c(a = 1, b = 2),
#'                 tb.vars = c(Term = 1, Estimate = 2)) +
#'     expand_limits(y = 67)
#'
#' ## x is also a numeric or continuous variable
#' # Polynomial regression, with default formatting
#' if (broom.installed)
#'   ggplot(my.df, aes(covariate, x)) +
#'     geom_point() +
#'     stat_fit_tb(method.args = list(formula = y ~ poly(x, 2))) +
#'     expand_limits(y = 70)
#'
#' # Polynomial regression, with manual table formatting
#' if (broom.installed)
#'   ggplot(my.df, aes(covariate, x)) +
#'     geom_point() +
#'     stat_fit_tb(method.args = list(formula = y ~ poly(x, 2)),
#'                 tb.params = c("x^0" = 1, "x^1" = 2, "x^2" = 3),
#'                 tb.vars = c("Term" = 1, "Estimate" = 2, "S.E." = 3,
#'                             "italic(t)" = 4, "italic(P)" = 5),
#'                 parse = TRUE) +
#'     expand_limits(y = 70)
#'
#' ## group is a factor or discrete variable
#' # ANOVA summary, with default formatting
#' if (broom.installed)
#'   ggplot(my.df, aes(group, x)) +
#'     geom_point() +
#'     stat_fit_tb() +
#'     expand_limits(y = 70)
#'
#' # ANOVA table, with default formatting
#' if (broom.installed)
#'   ggplot(my.df, aes(group, x)) +
#'     geom_point() +
#'     stat_fit_tb(tb.type = "fit.anova") +
#'     expand_limits(y = 70)
#'
#' # ANOVA table, with manual table formatting
#' if (broom.installed)
#'   ggplot(my.df, aes(group, x)) +
#'     geom_point() +
#'     stat_fit_tb(tb.type = "fit.anova",
#'                 tb.vars = c(Effect = "term", "df", "italic(F)" = "statistic",
#'                             "italic(P)" = "p.value"),
#'                 tb.params = c(Group = 1, Error = 2),
#'                 parse = TRUE)
#'
#' # ANOVA table, with manual table formatting
#' # using column names with partial matching
#' if (broom.installed)
#'   ggplot(my.df, aes(group, x)) +
#'     geom_point() +
#'     stat_fit_tb(tb.type = "fit.anova",
#'                 tb.vars = c(Effect = "term", "df", "italic(F)" = "stat",
#'                             "italic(P)" = "p"),
#'                 tb.params = c(Group = "x", Error = "Resid"),
#'                 parse = TRUE)
#'
#' # ANOVA summary, with default formatting
#' if (broom.installed)
#'   ggplot(my.df, aes(group, x)) +
#'     geom_point() +
#'     stat_fit_tb() +
#'     expand_limits(y = 70)
#'
#' ## covariate is a numeric variable and group is a factor
#' # ANCOVA (covariate not plotted) ANOVA table, with default formatting
#' if (broom.installed)
#'   ggplot(my.df, aes(group, x, z = covariate)) +
#'     geom_point() +
#'     stat_fit_tb(tb.type = "fit.anova",
#'                 method.args = list(formula = y ~ x + z))
#'
#' # ANCOVA (covariate not plotted) ANOVA table, with manual table formatting
#' if (broom.installed)
#'   ggplot(my.df, aes(group, x, z = covariate)) +
#'     geom_point() +
#'     stat_fit_tb(tb.type = "fit.anova",
#'                 method.args = list(formula = y ~ x + z),
#'                 tb.vars = c(Effect = 1, d.f. = 2,
#'                             "M.S." = 4, "italic(F)" = 5,
#'                             "italic(P)" = 6),
#'                 tb.params = c(Group = 1,
#'                               Covariate = 2,
#'                               Error = 3),
#'                 parse = TRUE)
#'
#' ## group is a factor or discrete variable
#' # t-test, minimal output, with manual table formatting
#' if (broom.installed)
#'   ggplot(my.df, aes(group, x)) +
#'     geom_point() +
#'     stat_fit_tb(method = "t.test",
#'               tb.vars = c("italic(t)" = "statistic", "italic(P)" = "p.value"),
#'               parse = TRUE)
#'
#' # t-test, more detailed output, with manual table formatting
#' if (broom.installed)
#'   ggplot(my.df, aes(group, x)) +
#'     geom_point() +
#'     stat_fit_tb(method = "t.test",
#'               tb.vars = c("\"Delta \"*italic(x)" = "estimate",
#'                           "CI low" = "conf.low", "CI high" = "conf.high",
#'                           "italic(t)" = "statistic", "italic(P)" = "p.value"),
#'               parse = TRUE) +
#'     expand_limits(y = 67)
#'
#' # t-test (equal variances assumed), minimal output, with manual table formatting
#' if (broom.installed)
#'   ggplot(my.df, aes(group, x)) +
#'     geom_point() +
#'     stat_fit_tb(method = "t.test",
#'                 method.args = list(formula = y ~ x, var.equal = TRUE),
#'                 tb.vars = c("italic(t)" = "statistic", "italic(P)" = "p.value"),
#'                 parse = TRUE)
#'
#' ## covariate is a numeric or continuous variable
#' # Linear regression using a table theme and non-default position
#' if (broom.installed)
#'   ggplot(my.df, aes(covariate, x)) +
#'     geom_point() +
#'     stat_fit_tb(table.theme = ttheme_gtlight,
#'                 npcx = "left", npcy = "bottom") +
#'     expand_limits(y = 35)
#'
stat_fit_tb <- function(mapping = NULL,
                        data = NULL,
                        geom = "table_npc",
                        position = "identity",
                        ...,
                        method = "lm",
                        method.args = list(formula = y ~ x),
                        n.min = 2L,
                        tidy.args = list(),
                        tb.type = "fit.summary",
                        tb.vars = NULL,
                        tb.params = NULL,
                        digits = 3,
                        p.digits = digits,
                        label.x = "center",
                        label.y = "top",
                        table.theme = NULL,
                        table.rownames = FALSE,
                        table.colnames = TRUE,
                        table.hjust = 1,
                        parse = FALSE,
                        na.rm = FALSE,
                        show.legend = FALSE,
                        inherit.aes = TRUE) {
  ggplot2::layer(
    stat = StatFitTb, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params =
      rlang::list2(method = method,
                   method.args = method.args,
                   n.min = n.min,
                   tidy.args = tidy.args,
                   tb.type = tb.type,
                   tb.vars = tb.vars,
                   tb.params = tb.params,
                   digits = digits,
                   p.digits = p.digits,
                   label.x = label.x,
                   label.y = label.y,
                   npc.used = grepl("_npc", geom),
                   table.theme = table.theme,
                   table.rownames = table.rownames,
                   table.colnames = table.colnames,
                   table.hjust = table.hjust,
                   parse = parse,
                   na.rm = na.rm,
                   ...)
  )
}

# Defined here to avoid a note in check --as-cran as the imports from 'broom'
# are not seen when the function is defined in-line in the ggproto object.
#' @rdname ggpmisc-ggproto
#'
#' @format NULL
#' @usage NULL
#'
fit_tb_compute_panel_fun <- function(data,
                                     scales,
                                     method = "lm",
                                     method.args = list(formula = y ~ x),
                                     n.min = 2L,
                                     tidy.args = list(),
                                     tb.type = "fit.summary",
                                     tb.vars = NULL,
                                     tb.params = NULL,
                                     digits = 3,
                                     p.digits = digits,
                                     npc.used = TRUE,
                                     label.x = "center",
                                     label.y = "top") {

  rlang::check_installed("broom", reason = "to use `stat_fit_tb()`")

  force(data)
  if (length(unique(data$x)) < n.min) {
    # Not enough data to perform fit
    return(data.frame())
  }

  # support setting of table position per panel
  panel.idx <- as.integer(as.character(data$PANEL[1]))
  if (length(label.x) >= panel.idx) {
    label.x <- label.x[panel.idx]
  } else if (length(label.x) > 0) {
    label.x <- label.x[1]
  }
  if (length(label.y) >= panel.idx) {
    label.y <- label.y[panel.idx]
  } else if (length(label.y) > 0) {
    label.y <- label.y[1]
  }

  if (is.character(method)) {
    method.name <- method
    method <- match.fun(method)
  } else {
    method.name <- deparse(substitute(method))
  }

  if ("data" %in% names(method.args)) {
    message("External 'data' passed can be inconsistent with plot!\n",
            "These data must be available at the time of printing!!!")
  } else if (any(grepl("formula|fixed|random|model", names(method.args)))) {
    #    method.args <- c(method.args, list(data = quote(data)))  works in most cases and avoids copying data
    method.args <- c(method.args, list(data = data)) # cor.test() needs the actual data
  } else {
    if (method.name == "cor.test" ) {
      warning("Only the 'formula' interface of methods is supported. No formula found, using '~ x + y'")
      selector <- setdiff(names(method.args), c("x", "y"))
      method.args <- c(method.args[selector], list(formula = ~ x + y, data = data)) # cor.test() needs the actual data
    } else {
      warning("Only the 'formula' interface of methods is supported. No formula found, using 'y ~ x' default")
      method.args <- c(method.args, list(formula = y ~ x, data = data)) # cor.test() needs the actual data
    }
  }
  fm <- do.call(method, method.args)
  fm.class <- class(fm) # keep track of fitted model class

  if (tolower(tb.type) %in% c("fit.anova", "anova")) {
    tidy.args <- c(x = quote(stats::anova(fm)), tidy.args)
    fm.tb <- do.call(generics::tidy, tidy.args)
  } else if (tolower(tb.type) %in% c("fit.summary", "summary")) {
    tidy.args <- c(x = quote(fm), tidy.args)
    fm.tb <- do.call(generics::tidy, tidy.args)
  } else if (tolower(tb.type) %in% c("fit.coefs", "coefs")) {
    tidy.args <- c(x = quote(fm), tidy.args)
    fm.tb <- do.call(generics::tidy, tidy.args)[c("term", "estimate")]
  }

  # reduce number of significant digits of all numeric columns
  num.cols <- sapply(fm.tb, is.numeric)
  fm.tb[num.cols] <- signif(fm.tb[num.cols], digits = digits)
  # treat p.value as a special case
  if ("p.value" %in% colnames(fm.tb) && p.digits > 0 && p.digits <= 22) {
    fm.tb[["p.value"]] <- round(fm.tb[["p.value"]], digits = p.digits)
    limit.text <- paste("<", format(1 * 10^-p.digits, nsmall = p.digits))
    fm.tb[["p.value"]] <- ifelse(fm.tb[["p.value"]] > 0,
                                 format(fm.tb[["p.value"]], nsmall = p.digits),
                                 limit.text)
  }

  if (!is.null(tb.params) && !is.null(fm.tb)) {
    if (is.character(tb.params)) {
      idxs <- pmatch(tb.params, fm.tb[[1]])
      if (length(idxs) < length(tb.params) || anyNA(idxs)) {
        warning("Attempt to select nonexistent params")
        idxs <- stats::na.omit(idxs)
        # no renaming possible, as we do not know which name was not matched
        tb.params <- unname(tb.params)
      }
    } else {
      idxs <- unname(tb.params)
      if (any(idxs > nrow(fm.tb))) {
        warning("Attempt to select nonexistent params")
        idxs <- idxs[idxs <= nrow(fm.tb)]
        tb.params <- tb.params[idxs]
      }
    }
    if (length(idxs) < nrow(fm.tb)) {
      message("Dropping params/terms (rows) from table!")
    }
    if (is.character(tb.params)) {
      idxs <- pmatch(tb.params, fm.tb[[1]])
    } else {
      idxs <- unname(tb.params)
    }
    if (length(idxs) < 1L) {
      warning("No matching parameters(s).")
      fm.tb <- NULL
    } else {
      fm.tb <- fm.tb[idxs, ]
      if (!is.null(names(tb.params))) {
        # support renaming of only some selected columns
        selector <- names(tb.params) != ""
        fm.tb[[1]][selector] <- names(tb.params)[selector]
      }
    }
  }

  if (!is.null(tb.vars)) {
    if (is.character(tb.vars)) {
      idxs <- pmatch(tb.vars, colnames(fm.tb))
       if (length(idxs) < length(tb.vars) || anyNA(idxs)) {
        warning("Attempt to select nonexistent columns by name")
        idxs <- stats::na.omit(idxs)
        # no renaming possible, as we do not know which name was not matched
        tb.vars <- unname(tb.vars)
       }
     } else {
      idxs <- unname(tb.vars)
      if (any(idxs > ncol(fm.tb))) {
        warning("Attempt to select nonexistent columns")
        idxs <- idxs[idxs <= ncol(fm.tb)]
        tb.vars <- tb.vars[idxs]
      }
    }
    if (!(1L %in% idxs) && nrow(fm.tb > 1L)) {
      # we warn only if params are two or more
      message("Dropping param names from table!")
    }
    if (length(idxs) < 1L) {
      warning("No matching column(s).")
      fm.tb <- NULL
    } else {
      fm.tb <- fm.tb[ , idxs]
      if (!is.null(names(tb.vars))) {
        # support renaming of only some selected columns
        selector <- names(tb.vars) != ""
        colnames(fm.tb)[selector] <- names(tb.vars)[selector]
      }
    }
  }

  # enclose the tibble and the call in lists to make them acceptable as columns
  formula.ls <- fail_safe_formula(fm, method.args)
  z <- tibble::tibble(fm.tb = list(fm.tb),
                      fm.tb.type = tb.type,
                      fm.class = fm.class[1],
                      fm.method = method.name,
                      fm.formula = formula.ls,
                      fm.formula.chr = format(formula.ls))

  if (npc.used) {
    margin.npc <- 0.05
  } else {
    margin.npc <- 0
  }

  if (is.character(label.x)) {
    label.x <- switch(label.x,
                      right = (1 - margin.npc),
                      center = 0.5,
                      centre = 0.5,
                      middle = 0.5,
                      left = (0 + margin.npc)
    )
    if (!npc.used) {
      # we need to use scale limits as observations are not necessarily plotted
      x.range <- scales$x$range$range
      label.x <- label.x * diff(x.range) + x.range[1]
    }
  }
  if (is.character(label.y)) {
    label.y <- switch(label.y,
                      top = (1 - margin.npc),
                      center = 0.5,
                      centre = 0.5,
                      middle = 0.5,
                      bottom = (0 + margin.npc)
    )
    if (!npc.used) {
      # we need to use scale limits as observations are not necessarily plotted
      y.range <- scales$y$range$range
      label.y <- label.y * diff(y.range) + y.range[1]
    }
  }

  if (npc.used) {
    z$npcx <- label.x
    z$x <- NA_real_
    z$npcy <- label.y
    z$y <- NA_real_
  } else {
    z$x <- label.x
    z$npcx <- NA_real_
    z$y <- label.y
    z$npcy <- NA_real_
  }

  z
}

#' @rdname ggpmisc-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatFitTb <-
  ggplot2::ggproto("StatFitTb", ggplot2::Stat,
                   compute_panel = fit_tb_compute_panel_fun,
                   dropped_aes = "weight",
                   default_aes =
                     ggplot2::aes(hjust = "inward",
                                  vjust = "inward",
                                  label = after_stat(fm.tb)),
                   required_aes = c("x", "y")
  )
