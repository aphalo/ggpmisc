#' Helper function for fitting Normal mixture model
#'
#' Factored out code used in both stat_distrmix_line() and stat_distrmix_eq().
#'
#' @inheritParams stat_distrmix_line
#' @param verbose If FALSE, capture screen output from method and display it
#'   as a \code{message()} only in case of conversion failure.
#'
#' @keywords internal
#'
#' @details
#' This function does the model fitting and returns a data frame with the
#' estimates for the parameters. It is a wrapper on functions from package
#' 'mixtools'.
#'
distrmix_helper_fun <-
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
           fm.values = TRUE,
           verbose = getOption("verbose", default = FALSE)) {
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
      if (verbose) {
        fm <- do.call(method, args = fun.args)
      } else {
        printed.text <-
          utils::capture.output(fm <- do.call(method, args = fun.args))
      }

      if (!length(fm) || (is.atomic(fm) && is.na(fm))) {
        return(NA)
      } else if (!inherits(fm, "mixEM")) {
        warning("Skipping! Method \"", method.name,
                "\" did not return a ",
                "\"mixEM\" object, but a \"", class(fm)[1], "\" object")
        return(NA)
      }

      converged <- length(fm[["all.loglik"]]) < maxit
      if (!converged) {
        warning("Model fitting failure: no convergence in ", maxit, " iterations!")
        if (!verbose) {
          message("Printout from 'method':\n", printed.text)
        }
      }

      # extract fitted parameter estimates
      if (se && converged) {
        # using bootstrap, standard errors are computed for the parameter estimates
        # B is the number of "trials" used to estimate the se
        if (verbose) {
          fm.param.se <- mixtools::boot.se(fm, B = 100)
        } else {
          printed.text <-
            utils::capture.output(fm.param.se <- mixtools::boot.se(fm, B = 100))
        }
        fm.param.se[grep(".se$", names(fm.param.se), value = TRUE)]

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
