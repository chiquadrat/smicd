#' @title Plot Diagnostics for sem Objects
#'
#' @description Available are convergence plots for the estimated
#' fixed effects model parameters and the residual variance of
#' the linear or linear mixed regression model. If the Box-Cox transformation
#' is used
#' for the transformation of the dependent variable, a convergence plot
#' of the transformation parameter lambda is also available.
#' In each of the convergence plots, the estimated parameter is plotted for
#' each iteration step of the SEM-algorithm.
#' Furthermore, the average up to iteration step M is plotted (without the burn-in
#' iterations).
#' A vertical line indicates the end of the burn-in period.
#' A horizontal line marks the value of the estimated statistical indicator
#' Furthermore, the estimated density of the simulated dependent variable from
#' the last iteration step is plotted with a histogram of the interval-censored
#' true dependent variable in the back.
#' @param x an object of type "sem", typical result of \code{\link{semLm}} or
#' \code{\link{semLme}}.
#' @param ... optional arguments passed to generic function.
#' @return Convergence and density plots.
#' @references
#' Walter, P. (2019). A Selection of Statistical Methods for Interval-Censored
#' Data with Applications to the German Microcensus, PhD thesis,
#' Freie Universitaet Berlin
#' @seealso \code{\link{semObject}}, \code{\link{semLm}}, \code{\link{semLme}}
#' @export

plot.sem <- function(x, ...) {
  graphics::par(mfrow = c(2, 1))
  plot(x$conv.sigmae,
    main = expression(paste("Convergence ", sigma[e])),
    xlab = "Iteration step", ylab = "Point estimate for each iteration"
  )
  abline(v = x$burnin)
  means <- NULL
  for (j in 1:x$samples) {
    means <- c(means, mean(x$conv.sigmae[x$burnin:j]))
  }
  point <- c(rep(means[1] - .Machine$double.xmin, x$burnin), means)
  plot(point,
    xlim = c(0, (x$burnin + x$samples)),
    col = ifelse(point == means[1] - .Machine$double.xmin, "white", "black"),
    xlab = "Iteration step", ylab = "Average up to iteration step M"
  )
  abline(h = x$sigmae)
  abline(v = x$burnin)


  cat("Press [enter] to continue")
  line <- readline()

  for (i in 1:(dim(x$conv.coef)[1])) {
    graphics::par(mfrow = c(2, 1))
    lbs <- parse(text = (paste0("beta[", i - 1, "]")))
    indx <- i - 1
    plot(x$conv.coef[i, ],
      main = bquote("Convergence" ~ beta[.(indx)]),
      xlab = "Iteration step",
      ylab = "Point estimate for each iteration"
    )
    abline(v = x$burnin)

    means <- NULL
    for (j in 1:x$samples) {
      means <- c(means, mean(x$conv.coef[i, ][x$burnin:j]))
    }
    point <- c(rep(means[1] - .Machine$double.xmin, x$burnin), means)
    plot(point,
      xlim = c(0, (x$burnin + x$samples)),
      col = ifelse(point == means[1] - .Machine$double.xmin, "white", "black"),
      xlab = "Iteration step", ylab = "Average up to iteration step M"
    )
    abline(h = x$coef[i])
    abline(v = x$burnin)


    cat("Press [enter] to continue")
    line <- readline()
  }



  if (!is.null(x$conv.lambda)) {
    graphics::par(mfrow = c(2, 1))
    plot(x$conv.lambda,
      main = expression(paste("Convergence ", lambda)),
      xlab = "Iteration step",
      ylab = "Point estimate for each iteration"
    )
    abline(v = x$b.lambda)

    means <- NULL
    for (j in 1:x$samples) {
      means <- c(means, mean(x$conv.lambda[x$burnin:j]))
    }
    point <- c(rep(means[1] - .Machine$double.xmin, x$burnin), means)
    plot(point,
      xlim = c(0, (x$burnin + x$samples)),
      col = ifelse(point == means[1] - .Machine$double.xmin, "white", "black"),
      xlab = "Iteration step", ylab = "Average up to iteration step M"
    )
    abline(h = x$lambda)
    abline(v = x$burnin)


    cat("Press [enter] to continue")
    line <- readline()
  }

  if (x$transformation == "None") {
    graphics::par(mfrow = c(1, 1))
    if (x$classes[1] == -Inf) {
      x$classes[1] <- min(x$pseudo.y[, ncol(x$pseudo.y)]) - 1
    }
    if (x$classes[length(x$classes)] == Inf) {
      x$classes[length(x$classes)] <- max(x$pseudo.y[, ncol(x$pseudo.y)]) + 1
    }

    maxhist <- max(hist(x$pseudo.y[, ncol(x$pseudo.y)],
      breaks = x$classes,
      main = "Density from last iteration step", xlab = "Pseudo Y"
    )$density)
    maxden <- max(density(x$pseudo.y[, ncol(x$pseudo.y)])$y)
    maxylim <- max(maxhist, maxden)

    hist(x$pseudo.y[, ncol(x$pseudo.y)],
      breaks = x$classes, main = "Density from last iteration step",
      xlab = "Pseudo Y", ylim = c(0, maxylim)
    )
    lines(density(x$pseudo.y[, ncol(x$pseudo.y)]), col = "purple", lwd = 2)
  }

  if (x$transformation == "log") {
    graphics::par(mfrow = c(1, 1))
    pseudo.y <- exp(x$pseudo.y[, ncol(x$pseudo.y)])
    if (x$classes[1] == -Inf) {
      x$classes[1] <- min(pseudo.y) - 1
    }
    if (x$classes[length(x$classes)] == Inf) {
      x$classes[length(x$classes)] <- max(pseudo.y) + 1
    }
    classes <- x$classes

    maxhist <- max(hist(pseudo.y,
      breaks = classes, main = "Density from last iteration step",
      xlab = "Pseudo Y on original scale"
    )$density)
    maxden <- max(density(pseudo.y)$y)
    maxylim <- max(maxhist, maxden)

    hist(pseudo.y,
      breaks = classes, main = "Density from last iteration step",
      xlab = "Pseudo Y on original scale", ylim = c(0, maxylim)
    )
    lines(density(pseudo.y), col = "purple", lwd = 2)
  }

  if (x$transformation == "bc") {
    graphics::par(mfrow = c(1, 1))
    pseudo.y <- boxcox.lme.est(
      dat = x$pseudo.y[, ncol(x$pseudo.y)],
      lambda = x$lambda, inverse = T
    )[[1]]
    if (x$classes[1] == -Inf) {
      x$classes[1] <- min(pseudo.y) - 1
    }
    if (x$classes[length(x$classes)] == Inf) {
      x$classes[length(x$classes)] <- max(pseudo.y) + 1
    }
    classes <- x$classes

    maxhist <- max(hist(pseudo.y,
      breaks = classes, main = "Density from last iteration step",
      xlab = "Pseudo Y on original scale"
    )$density)
    maxden <- max(density(pseudo.y)$y)
    maxylim <- max(maxhist, maxden)

    hist(pseudo.y,
      breaks = classes, main = "Density from last iteration step",
      xlab = "Pseudo Y on original scale", ylim = c(0, maxylim)
    )
    lines(density(pseudo.y), col = "purple", lwd = 2)
  }
}
