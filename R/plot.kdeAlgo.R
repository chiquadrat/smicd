#' @title Plot Diagnostics for a kdeAlgo Object
#'
#' @description Plots the estimated density of the interval-censored variable.
#' Also, convergence plots are given for all estimated statistical indicators.
#' The estimated indicator is plotted for each iteration step of the KDE-algorithm.
#' Furthermore, the average up to iteration step M is plotted (without the burn-in
#' iterations). A vertical line indicates the end of the burn-in period.
#' A horizontal line marks the value of the estimated statistical indicator
#' @param x an object of type "kdeAlgo", typical result of \code{\link{kdeAlgo}}
#' @param indicator a vector of indicator names specifying for which indicators
#' convergence plots are plotted, e.g. \code{c("mean", "gini")}
#' @param ... optional arguments passed to generic function.
#' @return Convergence and density plots.
#' @seealso \code{\link{kdeAlgoObject}}, \code{\link{kdeAlgo}}
#' @export
#' @importFrom graphics abline hist lines plot
#' @importFrom weights wtd.hist


plot.kdeAlgo <- function(x, indicator = NULL, ...) {
  oecd <- NULL

  if (is.null(indicator)) {
    for (i in 1:dim(x$Point_estimate.run)[1]) {
      graphics::par(mfrow = c(2, 1))
      name <- rownames(x$Point_estimate.run)
      plot(x$Point_estimate.run[i, ],
        xlab = "Iteration step",
        ylab = "Point estimate for each iteration",
        main = paste0("Convergence ", name[i])
      )
      abline(v = x$burnin)

      means <- NULL
      for (j in 1:x$samples) {
        means <- c(means, mean(x$Point_estimate.run[i, ][x$burnin:j]))
      }
      point <- c(rep(means[1] - .Machine$double.xmin, x$burnin), means)
      plot(point,
        xlim = c(0, (x$burnin + x$samples)),
        col = ifelse(point == means[1] - .Machine$double.xmin, "white", "black"),
        xlab = "Iteration step", ylab = "Average up to iteration step M"
      )
      abline(h = x$Point_estimate[i])
      abline(v = x$burnin)

      cat("Press [enter] to continue")
      line <- readline()
    }
  }
  if (!is.null(indicator)) {
    for (i in 1:length(indicator)) {
      name <- indicator
      graphics::par(mfrow = c(2, 1))
      plot(x$Point_estimate.run[indicator[i], ],
        xlab = "Iteration step",
        ylab = "Point estimate for each iteration",
        main = paste0("Convergence ", name[i])
      )
      abline(v = x$burnin)

      means <- NULL
      for (j in 1:x$samples) {
        means <- c(means, mean(x$Point_estimate.run[indicator[i], ][x$burnin:j]))
      }
      point <- c(rep(means[1] - .Machine$double.xmin, x$burnin), means)
      plot(point,
        xlim = c(0, (x$burnin + x$samples)),
        col = ifelse(point == means[1] - .Machine$double.xmin, "white", "black"),
        xlab = "Iteration step", ylab = "Average up to iteration step M"
      )
      abline(h = x$Point_estimate[indicator[i]])
      abline(v = x$burnin)

      cat("Press [enter] to continue")
      line <- readline()
    }
  }
  graphics::par(mfrow = c(1, 1))
  classes <- x$classes
  xclass <- x$xclass
  if (max(classes) == Inf) {
    classes[length(classes)] <- x$upper * classes[length(classes) -
      1]
  }
  classmeans <- sapply(1:(length(classes) - 1), function(x) {
    1 / 2 *
      (classes[x + 1] + classes[x])
  })
  levels(xclass) <- classmeans
  lengths <- as.vector(table(xclass))
  xclass <- as.numeric(as.character(xclass))

  if (is.null(oecd) & is.null(weights)) {
    maxhist <- max(hist(xclass,
      breaks = classes, main = "Estimated Density",
      xlab = "x", ylab = "f(x)",
      xlim = c(classes[1], classes[length(classes)] / 2)
    )$density)
    maxden <- max(x$Mestimates)
    maxylim <- max(maxhist, maxden)
    hist(xclass,
      breaks = classes, main = "Estimated Density", xlab = "x",
      ylab = "f(x)",
      xlim = c(classes[1], classes[length(classes)] / 2), ylim = c(0, maxylim)
    )
    lines(x$Mestimates ~ x$gridx, col = "purple", lwd = 2)
  }
  if (!is.null(oecd)) {
    maxylim <- max(x$Mestimates)
    plot(x$Mestimates ~ x$gridx, col = "purple", lwd = 2, type = "l")
  }
  if (is.null(oecd) & !is.null(weights)) {
    maxhist <- max(wtd.hist(xclass,
      breaks = classes, main = "Estimated Density",
      xlab = "x", ylab = "f(x)",
      xlim = c(classes[1], classes[length(classes)] / 2),
      weight = x$weights
    )$density)
    maxden <- max(x$Mestimates)
    maxylim <- max(maxhist, maxden)
    wtd.hist(xclass,
      breaks = classes, main = "Estimated Density", xlab = "x",
      ylab = "f(x)",
      xlim = c(classes[1], classes[length(classes)] / 2), ylim = c(0, maxylim),
      weight = x$weights
    )
    lines(x$Mestimates ~ x$gridx, col = "purple", lwd = 2)
  }
}
