#' @title Plot Diagnostics for a kdeAlgo Object
#'
#' @description Plots the esimtated density from the interval censored variable.
#' Also, convergence plots are given for all estimated statistical indicators.
#' The estimated indicator is plotted against the iteration step of the KDE-Algorithm.
#' A vertical line indicates the end of the burn-in period.
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

  if(is.null(indicator)){
  for (i in 1:dim(x$Point_estimate.run)[1]) {
   name <- rownames(x$Point_estimate.run)
   plot(x$Point_estimate.run[i,], xlab = "Iteration step", ylab = name[i],
        main = "Convergence")
   abline(v = x$burnin)
   cat("Press [enter] to continue")
   line <- readline()
 }

}
  if(!is.null(indicator)){
  for (i in 1:length(indicator)) {
      name <- indicator
      plot(x$Point_estimate.run[indicator[i],], xlab = "Iteration step", ylab = name[i],
           main = "Convergence")
      abline(v = x$burnin)
      cat("Press [enter] to continue")
      line <- readline()
    }

  }
  classes <- x$classes
  xclass <- x$xclass
  if (max(classes) == Inf) {
    classes[length(classes)] = x$upper * classes[length(classes) -
                                                 1]
  }
  classmeans <- sapply(1:(length(classes) - 1), function(x) 1/2 *
                         (classes[x + 1] + classes[x]))
  levels(xclass) <- classmeans
  lengths = as.vector(table(xclass))
  xclass <- as.numeric(as.character(xclass))

  if(is.null(oecd) & is.null(weights)) {
  maxhist <- max(hist(xclass,breaks=classes, main = "Estimated Density",
                      xlab = "x", ylab = "f(x)",
                      xlim = c(classes[1],classes[length(classes)]/2))$density)
  maxden <- max(x$Mestimates)
  maxylim <- max(maxhist, maxden)
  hist(xclass,breaks=classes, main = "Estimated Density", xlab = "x",
       ylab = "f(x)",
       xlim = c(classes[1],classes[length(classes)]/2), ylim = c(0,maxylim))
  lines(x$Mestimates~x$gridx,col="purple",lwd=2)
  }
  if(!is.null(oecd)){
    maxylim <- max(x$Mestimates)
    plot(x$Mestimates~x$gridx,col="purple",lwd=2, type="l")
  }
  if(is.null(oecd)&!is.null(weights)) {
    maxhist <- max(wtd.hist(xclass,breaks=classes, main = "Estimated Density",
                        xlab = "x", ylab = "f(x)",
                        xlim = c(classes[1],classes[length(classes)]/2),
                   weight = x$weights)$density)
    maxden <- max(x$Mestimates)
    maxylim <- max(maxhist, maxden)
    wtd.hist(xclass,breaks=classes, main = "Estimated Density", xlab = "x",
         ylab = "f(x)",
         xlim = c(classes[1],classes[length(classes)]/2), ylim = c(0,maxylim),
         weight = x$weights)
    lines(x$Mestimates~x$gridx,col="purple",lwd=2)
  }
}
