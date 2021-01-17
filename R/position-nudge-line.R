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
#' @param abline a vector of length two giving the intercept and slope.
#' @param method One of `"spline"` or `"linear"`.
#' @param formula A model formula for [lm()] when `method = "linear"`. Ignored
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
#' @note Only model formulas that corresponding polynomials with no missing
#'   terms are supported. Use of [poly()] is recomended.
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
#' ggplot(df, aes(x, y)) +
#'   geom_point() +
#'   geom_line() +
#'   geom_text(aes(label = l),
#'             position = position_nudge_line(x = 0.6, y = 4))
#'
#' ggplot(df, aes(x, y)) +
#'   geom_point() +
#'   geom_line() +
#'   geom_text(aes(label = l),
#'             position = position_nudge_line(x = -0.6, y = -4))
#'
#' ggplot(df, aes(x, -y)) +
#'   geom_point() +
#'   geom_line() +
#'   geom_text(aes(label = l),
#'             position = position_nudge_line(x = 0.6, y = 4))
#'
#' ggplot(df, aes(x, y - 40)) +
#'   geom_point() +
#'   geom_line() +
#'   geom_text(aes(label = l),
#'             position = position_nudge_line(x = 0.6, y = 4))
#'
#' ggplot(subset(df, x >= 0), aes(y, sqrt(y))) +
#'   geom_point() +
#'   geom_line() +
#'   geom_text(aes(label = l),
#'             position = position_nudge_line(x = 6, y = 0.4))
#'
#' ggplot(subset(df, x >= 0), aes(y, sqrt(y))) +
#'   geom_point() +
#'   geom_line() +
#'   geom_text(aes(label = l),
#'                   position = position_nudge_line(x = -8, y = -0.2))
#'
#' ggplot(df, aes(x, x * 2 + 5)) +
#'   geom_point() +
#'   geom_line() +
#'   geom_text(aes(label = l),
#'             position = position_nudge_line(x = 0.5, y = 1,
#'                                            abline = c(5, 2)))
#'
#' # Points scattered near a curve or line, we use 'direction = "split"'
#' ggplot(df, aes(x)) +
#'   geom_point(aes(y = yy)) +
#'   geom_line(aes(y = y)) +
#'   geom_text(aes(y = yy, label = l),
#'             position = position_nudge_line(x = 0.5, y = 4,
#'                                            direction = "split"))
#'
#' ggplot(subset(df, x >= 0), aes(y, yy)) +
#'   geom_point() +
#'   stat_smooth(method = "lm", formula = y ~ x) +
#'   geom_text(aes(label = l),
#'             position = position_nudge_line(x = 2, y = 3,
#'                                            direction = "split"))
#'
#' ggplot(subset(df, x >= 0), aes(x, yyy)) +
#'   geom_point() +
#'   stat_smooth(method = "lm", formula = y ~ x) +
#'   geom_text(aes(label = l),
#'             position = position_nudge_line(x = 0.3, y = 0.6,
#'                                            line_nudge = 1.1,
#'                                            direction = "split"))
#'
#' ggplot(subset(df, x >= 0), aes(y, yy)) +
#'   geom_point() +
#'   stat_smooth(method = "lm", formula = y ~ x) +
#'   geom_text(aes(label = l),
#'             position = position_nudge_line(x = 3, y = 3,
#'                                            method = "linear",
#'                                            direction = "split"))
#'
#' ggplot(subset(df, x >= 0), aes(y, yy)) +
#'   geom_point() +
#'   stat_smooth(method = "lm", formula = y ~ x) +
#'   geom_text(aes(label = l),
#'             position = position_nudge_line(x = 3, y = 3,
#'                                            abline = c(0, 1),
#'                                            direction = "split"))
#'
#' ggplot(subset(df, x >= 0), aes(y, -yy)) +
#'   geom_point() +
#'   stat_smooth(method = "lm", formula = y ~ x) +
#'   geom_text(aes(label = l),
#'             position = position_nudge_line(x = 3, y = 3,
#'                                            method = "linear",
#'                                            direction = "split"))
#'
position_nudge_line <- function(x = 0,
                                y = 0,
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
    method <- "automatic" # decided later based on nrow(data)
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
  abline = rep(NA_real_, 2),
  method = "spline",
  formula = y ~ x,
  direction = "none",
  line_nudge = 1,

  setup_params = function(self, data) {
    list(x = self$x,
         y = self$y,
         abline = self$abline,
         method = self$method,
         formula = self$formula,
         direction = self$direction,
         line_nudge = self$line_nudge
         )
  },

  compute_layer = function(self, data, params, layout) {
    if (params$method == "automatic") {
      if (nrow(data) < 5) {
        params$method <- "linear"
      } else {
        params$method <- "spline"
      }
    }

    if (params$method == "abline") {
      if (is.numeric(params$abline) && length(params$abline) == 2) {
        curve <- params$abline[1] + params$abline[2] * data$x
        sm.deriv <- params$abline[2]
      } else {
        stop("'abline' should be a numeric vector of length 2")
      }
    } else if (nrow(data) < 4 || params$method == "linear") {
      mf <- lm(formula = params$formula, data = data)
      curve <- predict(mf)
      deriv.poly <- deriv(polynom::polynomial(coef(mf)))
      sm.deriv <- predict(deriv.poly, data$x)
      if (params$method != "linear") {
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
    # this code sort of gets the job sort of done, but feels too complex!!
    angle <- atan2(sm.deriv, 1) + 0.5 * pi * sign(sm.deriv)
    x_nudge <- params$x * cos(angle) * sign(angle)
    y_nudge <- params$y * sin(angle) * sign(angle)

    if (params$direction == "split") {
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
