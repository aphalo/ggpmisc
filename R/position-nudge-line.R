#' Nudge labels a variable distance from points
#'
#' `position_nudge_line` is generally useful for adjusting the starting
#' position of labels or text to be repelled while preserving the original
#' position as the start of the segments. The difference compared to
#' [position_nudge_center()] is that the nudging is away from from a line or
#' curve fitted to the data points or supplied as coefficients. While
#' [position_nudge_center()] is most useful for "round-shaped", vertically- or
#' horizontally elongated clouds of points, [position_nudge_line()] is most
#' suitable when observations follow a linear or curvilinear relationship
#' between _x_ and _y_ values.
#'
#' @family position adjustments
#' @param x,y Amount of vertical and horizontal distance to move. A numeric
#'   vector of length 1, or of the same length as rows there are in `data`.
#' @param rel_distance Nudge relative to _x_ and _y_ data expanse, ignored
#'   unless `x` and `y` are both `NA`s.
#' @param abline a vector of length two giving the intercept and slope.
#' @param method One of `"spline"`, `"lm"` or `"auto"`.
#' @param formula A model formula for [lm()] when `method = "lm"`. Ignored
#'   otherwise.
#' @param direction One of "none", or "split".
#' @param line_nudge A positive multiplier >= 1, increasing nudging
#'   away from the curve or line compared to nudging from points.
#' @details When `direction = "split"` nudging is away from an implicit line or
#'   curve on either side. The line of curve can be smooth spline or linear
#'   regression fitted on-the-fly to the data points, or a straight line defined
#'   by its coefficients passed to `abline`. The fitting is well defined only if
#'   the observations fall roughly on a curve or straight line that is monotonic
#'   in `y`. By means of `line_nudge` one can increment nudging away from the
#'   line or curve compared to away from the points, which is useful for example
#'   to keep labels outside of a confidence band. Direction defaults to
#'   `"split"` when `line_nudge > 1`, and otherwise to `"none"`.
#'
#' @note Only model formulas corresponding to polynomials with no missing
#'   terms are supported. Use of [poly()] is recommended.
#'
#' @export
#'
#' @examples
#'
#' set.seed(16532)
#' df <- data.frame(
#'   x = -10:10,
#'   y = (-10:10)^2,
#'   yy = (-10:10)^2 + rnorm(21, 0, 4),
#'   yyy = (-10:10) + rnorm(21, 0, 4),
#'   l = letters[1:21]
#' )
#'
#' # Point on a line or curve
#'
#' ggplot(df, aes(x, y, label = l)) +
#'   geom_point() +
#'   geom_line(linetype = "dotted") +
#'   geom_text(position = position_nudge_line())
#'
#' ggplot(df, aes(x, y, label = l)) +
#'   geom_point() +
#'   geom_line(linetype = "dotted") +
#'   geom_text(position = position_nudge_line(rel_distance = -0.04))
#'
#' ggplot(df, aes(x, y, label = l)) +
#'   geom_point() +
#'   geom_line(linetype = "dotted") +
#'   geom_text(position = position_nudge_line(x = 0.6, y = 4))
#'
#' ggplot(df, aes(x, y, label = l)) +
#'   geom_point() +
#'   geom_line(linetype = "dotted") +
#'   geom_text(position = position_nudge_line(x = -0.6, y = -4))
#'
#' ggplot(df, aes(x, -y, label = l)) +
#'   geom_point() +
#'   geom_line(linetype = "dotted") +
#'   geom_text(position = position_nudge_line(x = 0.6, y = 4))
#'
#' ggplot(df, aes(x, y - 40, label = l)) +
#'   geom_point() +
#'   geom_line(linetype = "dotted") +
#'   geom_text(position = position_nudge_line(x = 0.6, y = 4))
#'
#' ggplot(subset(df, x >= 0), aes(y, sqrt(y), label = l)) +
#'   geom_point() +
#'   geom_line(linetype = "dotted") +
#'   geom_text(position = position_nudge_line(x = 3, y = 0.3))
#'
#' ggplot(subset(df, x >= 0), aes(y, sqrt(y), label = l)) +
#'   geom_point() +
#'   geom_line(linetype = "dotted") +
#'   geom_text(position = position_nudge_line(x = -3, y = -0.5))
#'
#' ggplot(df, aes(x, x * 2 + 5, label = l)) +
#'   geom_point() +
#'   geom_abline(intercept = 5, slope = 2, linetype = "dotted") +
#'   geom_text(position = position_nudge_line(x = 0.5, y = 1,
#'                                            abline = c(5, 2)))
#'
#' # Points scattered near a curve or line, we use 'direction = "split"'
#' ggplot(df, aes(x)) +
#'   geom_point(aes(y = yy)) +
#'   geom_line(aes(y = y), linetype = "dotted") +
#'   geom_text(aes(y = yy, label = l),
#'             position = position_nudge_line(x = -0.5, y = -4,
#'                                            direction = "split"))
#'
#' ggplot(subset(df, x >= 0), aes(y, yy)) +
#'   geom_point() +
#'   stat_smooth(method = "lm", formula = y ~ x) +
#'   geom_text(aes(label = l),
#'             position = position_nudge_line(x = -3, y = -3,
#'                                            direction = "split"))
#'
#' ggplot(subset(df, x >= 0), aes(y, yy)) +
#'   geom_point() +
#'   stat_smooth(method = "lm", formula = y ~ x) +
#'   geom_text(aes(label = l),
#'             position = position_nudge_line(x = -3, y = -3,
#'                                            line_nudge = 2,
#'                                            direction = "split"))
#'
#' ggplot(subset(df, x >= 0), aes(x, yyy)) +
#'   geom_point() +
#'   stat_smooth(method = "lm", formula = y ~ x) +
#'   geom_text(aes(label = l),
#'             position = position_nudge_line(x = -0.4, y = -0.6,
#'                                            direction = "split"))
#'
#' ggplot(subset(df, x >= 0), aes(y, yy)) +
#'   geom_point() +
#'   stat_smooth(method = "lm", formula = y ~ x) +
#'   geom_text(aes(label = l),
#'             position = position_nudge_line(x = -3, y = -3,
#'                                            method = "lm",
#'                                            direction = "split"))
#'
#' ggplot(subset(df, x >= 0), aes(y, yy)) +
#'   geom_point() +
#'   stat_smooth(method = "lm", formula = y ~ x) +
#'   geom_text(aes(label = l),
#'             position = position_nudge_line(x = -3, y = -3,
#'                                            abline = c(0, 1),
#'                                            direction = "split"))
#'
#' ggplot(subset(df, x >= 0), aes(y, -yy)) +
#'   geom_point() +
#'   stat_smooth(method = "lm", formula = y ~ x) +
#'   geom_text(aes(label = l),
#'             position = position_nudge_line(x = -3, y = -3,
#'                                            method = "lm",
#'                                            direction = "split"))
#'
position_nudge_line <- function(x = NA_real_,
                                y = NA_real_,
                                rel_distance = 0.03,
                                abline = NULL,
                                method = NULL,
                                formula = y ~ x,
                                direction = NULL,
                                line_nudge = 1) {
  # set defaults
  if (!is.null(abline)) {
    method <- "abline"
  } else {
    abline <- rep(NA_real_, 2) # to ensure that a list member is created
  }

  if (is.null(method)) {
    method <- "auto" # decided later based on nrow(data)
  }

  if (method == "linear") {
    method <- "lm"
  }

  if (is.null(direction)) {
    if (line_nudge > 1) {
      direction <- "split"
    } else {
      direction <- "none"
    }
  }

  ggproto(NULL, PositionNudgeLine,
    x = x,
    y = y,
    rel_distance = rel_distance,
    abline = abline,
    method = method,
    formula = formula,
    direction = direction,
    line_nudge = line_nudge
  )
}

#' @rdname ggpmisc-ggproto
#' @format NULL
#' @usage NULL
#' @export
PositionNudgeLine <- ggproto("PositionNudgeLine", Position,
  x = 0,
  y = 0,
  rel_distance = 0.03,
  abline = rep(NA_real_, 2),
  method = "spline",
  formula = y ~ x,
  direction = "none",
  line_nudge = 1,

  setup_params = function(self, data) {
    list(x = self$x,
         y = self$y,
         rel_distance = self$rel_distance,
         abline = self$abline,
         method = self$method,
         formula = self$formula,
         direction = self$direction,
         line_nudge = self$line_nudge
         )
  },

  compute_layer = function(self, data, params, layout) {

    # set parameter defaults that depend on data values
    xy.range.ratio <- (max(data$x) - min(data$x)) / (max(data$y) - min(data$y))

    if (all(is.na(params$x)) & all(is.na(params$y))) {
      params$x <- params$rel_distance * (max(data$x) - min(data$x))
      params$y <- params$rel_distance * (max(data$y) - min(data$y))
    } else if (xor(all(is.na(params$x)), all(is.na(params$y)))) {
      if (is.na(params$x)) {
        params$x <- params$y * xy.range.ratio
      } else {
        params$y <- params$x / xy.range.ratio
      }
    }

    if (params$method == "auto") {
      if (nrow(data) < 5) {
        params$method <- "lm"
      } else {
        params$method <- "spline"
      }
    }

    # compute lines or curves and their derivatives
    if (params$method == "abline") {
      if (is.numeric(params$abline) && length(params$abline) == 2) {
        curve <- params$abline[1] + params$abline[2] * data$x
        # ensure same length in all cases
        sm.deriv <- rep(params$abline[2], nrow(data))
      } else {
        stop("'abline' should be a numeric vector of length 2")
      }
    } else if (nrow(data) < 4 || params$method == "lm") {
      mf <- lm(formula = params$formula, data = data)
      curve <- predict(mf)
      deriv.poly <- deriv(polynom::polynomial(coef(mf)))
      sm.deriv <- predict(deriv.poly, data$x)
      if (params$method != "lm") {
        message("Fitting a linear regression as n < 4")
      }
    } else if (params$method == "spline") {
      sm.spline <- smooth.spline(data$x, data$y)
      curve <- predict(sm.spline, x = data$x, deriv = 0)$y
      sm.deriv <- predict(sm.spline, x = data$x, deriv = 1)$y
    } else {
      stop("Method \"", params$method, "\"not recognized")
    }

    # compute x and y nudge for each point
    # By changing the sign we ensure consistent positions
    angle.rotation <- ifelse(sm.deriv > 0, -0.5 * pi, +0.5 * pi)
    # scaling is needed to conpute the angle on the plot
    angle <- atan2(sm.deriv * xy.range.ratio, 1) + angle.rotation
    x_nudge <- params$x * cos(angle) * ifelse(sm.deriv < 0, -1, +1)
    y_nudge <- params$y * sin(angle) * ifelse(sm.deriv < 0, -1, +1)

    if (params$direction == "split") {
      # sign depends on position relative to the line or curve
      x_nudge <- ifelse(data$y >= curve, x_nudge, -x_nudge)
      y_nudge <- ifelse(data$y >= curve, y_nudge, -y_nudge)
    } else if (params$direction != "none") {
      warning("Ignoring unrecognized direction \"", params$direction, "\".")
    }

    if (params$line_nudge > 1) {
      # nudging further away from line or curve than from points
      adj_y_nudge <- y_nudge * params$line_nudge - (data$y - curve)
      adj_x_nudge <- x_nudge * adj_y_nudge / y_nudge
      y_nudge <- ifelse(sign(y_nudge) == sign(adj_y_nudge) &
                          abs(y_nudge) < abs(adj_y_nudge),
                        adj_y_nudge,
                        y_nudge)
      x_nudge <- ifelse(sign(y_nudge) == sign(adj_y_nudge) &
                          abs(x_nudge) >= abs(adj_x_nudge),
                        adj_x_nudge,
                        x_nudge)
    }
    # transform both dimensions
    ggplot2::transform_position(data,
                                function(x) x + x_nudge,
                                function(y) y + y_nudge)
  }
)
