#' @title Estimation of Statistical Indicators from Interval Censored Data
#'
#' @description The function applies an iterative kernel density
#' algorithm for the estimation of a variaty of statistical indicators
#' (e.g. mean, median, quantiles, gini) from interval censored data. The estimation
#' of the standard errors is fasciliated by a non-parametric bootstrap.
#' @param xclass interval censored values; factor with ordered factor values,
#' as in \code{\link[Kernelheaping]{dclass}}
#' @param classes numeric vector of classes; Inf as last value is allowed,
#' as in \code{\link[Kernelheaping]{dclass}}
#' @param burnin burn-in sample size, as in \code{\link[Kernelheaping]{dclass}}
#' @param samples sampling iteration size, as in \code{\link[Kernelheaping]{dclass}}
#' @param bootstrap.se if \code{TRUE} standard errors for the statistical
#'  indicators are estimated
#' @param b number of bootstrap iterations for the estimation of the standard errors
#' @param boundary TRUE for positive only data (no positive density for negative values),
#' as in \code{\link[Kernelheaping]{dclass}}
#' @param bw bandwidth selector method, defaults to "nrd0", as in
#' \code{\link[stats]{density}}
#' @param evalpoints number of evaluation grid points, as in
#' \code{\link[Kernelheaping]{dclass}}
#' @param adjust the user can multiply the bandwidth by a certain factor such
#' that bw=adjust*bw as in \code{\link[stats]{density}}
#' @param threshold used for the Head-Count Ratio and Poverty Gap, default is 60\%
#' of the median e.g. \code{threshold=0.6}
#' @param custom_indicator a list of functions containing the indicators to be
#' calculated additionaly.
#' Such functions must only depend on the target variable \code{y} and the
#' \code{threshold}. Defaults to \code{NULL}.
#' @param upper if the upper bound of the upper interval is \code{Inf} e.g.
#' \code{(15000,Inf)}, then \code{Inf} is replaced by \code{15000*upper}
#' @param weights any kind of survey or design weights that will be used for the
#' weighted estimation of the statistical indicators
#' @param oecd OECD weights for equivalized household size
#' @return An object of class "kdeAlgo" that provides estimatates for statistical indicators
#' and optionally, corresponding standard error estimates. Generic
#' functions such as, \code{\link{print}},
#' \code{\link{plot}}, and \code{\link{summary}} have methods that can be used
#' to obtain further information. See \code{\link{kdeAlgoObject}} for a description
#' of components of objects of class "kdeAlgo".
#' @details The statistical indicators are estimated using pseudo samples as
#' proxy for the interval censored variable. The object \code{resultX} returns the
#' pseudo samples of each iteration run of the KDE-algorithm.
#' @references
#' Groß, M., U. Rendtel, T. Schmid, S. Schmon, and N. Tzavidis (2017).
#' Estimating the density of ethnic minorities and aged people in Berlin:
#' Multivariate
#' Kernel Density Estimation applied to sensitive georeferenced administrative
#' data
#' protected via measurement error. Journal of the Royal Statistical Society:
#' Series A
#' (Statistics in Society), 180.
#' @seealso \code{\link[Kernelheaping]{dclass}}, \code{\link{print.kdeAlgo}},
#' \code{\link{plot.kdeAlgo}}
#' @export
#' @importFrom ineq ineq
#' @importFrom spatstat weighted.quantile
#' @importFrom laeken gini
#' @importFrom laeken arpr
#' @importFrom laeken qsr
#' @import formula.tools
#' @return NULL
#' @examples
#' x=rlnorm(500, meanlog = 8, sdlog = 1)
#' classes <- c(0,500,1000,1500,2000,2500,3000,4000,5000, 6000,8000,10000, 15000,Inf)
#' xclass <- cut(x,breaks=classes)
#' weights <- abs(rnorm(500,0,1))
#' oecd <- rep(seq(1,6.9,0.3),25)
#' Indicator <- kdeAlgo(xclass = xclass, classes = classes, burnin = 40,
#' samples =200)
#' Indicator_custom <- kdeAlgo(xclass = xclass, classes = classes, burnin = 40,
#' samples =200, custom_indicator = list(quant5 = function(y, threshold)
#' {quantile(y, probs = 0.05)}))
#' Indicator_weights <- kdeAlgo(xclass = xclass, classes = classes, burnin = 40,
#' samples =200, weights = weights, oecd = oecd)


kdeAlgo <- function(xclass, classes, threshold = 0.6 , burnin = 10, samples = 50,
                     bootstrap.se = FALSE, b = 50, boundary = FALSE,
                     bw = "nrd0", evalpoints = 2000, adjust = 1,
                     custom_indicator = NULL, upper = 3, weights = NULL,
                     oecd = NULL) {
  Standard.Error = NULL
  density.est <- dclassICD(xclass = xclass, classes = classes,
                        burnin = burnin, samples = samples, boundary = boundary,
                        bw = bw, evalpoints = evalpoints, adjust = adjust,
                        upper = upper, weights = weights, oecd = oecd)

  Indicators.run <- NULL
  for (i in 1:dim(density.est$resultX)[2]) {
    Indicators.run <- cbind(Indicators.run,indicators.est(x=density.est$resultX[,i],
                                                          threshold = threshold,
                                                          custom_indicator = custom_indicator,
                                                          weights = density.est$resultW[,i]))

  }
  Indicators <- rowMeans(Indicators.run)

  if (bootstrap.se==TRUE) {
    Standard.Error <- standardError.est(b = b, xclass = xclass,
                                        classes = classes, burnin = burnin,
                                samples = samples, boundary = boundary, bw = bw,
                                evalpoints = evalpoints, adjust = adjust,
                                threshold = threshold,
                                custom_indicator = custom_indicator, upper = upper,
                                weights = weights, oecd = oecd)
  }

    results <- list(Point_estimate = Indicators, Standard_Error = Standard.Error,
                    Mestimates = density.est$Mestimates,
                    resultDensity = density.est$resultDensity,
                    resultX = density.est$resultX,
                    xclass = xclass, gridx = density.est$gridx,
                    classes = classes, burnin = burnin,
                    samples = samples, Point_estimate.run = Indicators.run,
                    oecd = oecd, weights = weights)

  class(results) <- "kdeAlgo"
return(results)
}
