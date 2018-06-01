#' Statistical Methods for Interval Censored (Grouped) Data
#'
#' The package \pkg{ICD} supports the estimation of linear and mixed linear regression
#' models (random slope and random intercept models)
#' with interval censored dependent variable. Parameter estimates are obtain
#' by a stochastic expectation maximaziation (SEM) algorithm (\cite{Walter et al. 2017}).
#' Standard errors are estimated by a non-parametric bootstrap in the linear
#' regression model and by a parametric bootstrap in the mixed linear regression
#' model. To handle departures
#' from the model assumptions transformations (log and Box-Cox) are incorporated into
#' the algorithm (\cite{Walter et al. 2017}). Furthermore, the package \pkg{ICD} has
#' implemented a non-parametric kernel density algorithm for the direct (without
#' covariates) estimation of statistical indicators from interval censored data
#' (\cite{Gross et al. 2017}). The standard errors of the statistical indicators
#'  are estimated by a non-parametric bootstrap.
#'
#' @details
#' The two estimation functions for the linear and mixed linear regression model
#' are called \code{\link{semLm}} and \code{\link{semLme}}. So far, only random
#' intercept and random slope models are implemented. For both functions
#' the following methods are available: \code{\link{summary.sem}},
#' \code{\link{print.sem}} and \code{\link{plot.sem}}.
#'
#' The function for the direct estimation of statistical indicators is called
#' \code{\link{kdeAlgo}}. The following methods are available:
#' \code{\link{print.kdeAlgo}} and \code{\link{plot.kdeAlgo}}.
#'
#' An overview of all currently provided functions can be requested by
#' \code{library(help=ICD)}.
#'
#' @references
#' Walter, P., Gross, M., Schmid, T. and Tzavidis, N. (2017). Estimation of Linear and Non-Linear Indicators
#' using Interval Censored Income Data. School of Business & Economics, Discussion
#' Paper. \cr \cr
#' Groß, M., U. Rendtel, T. Schmid, S. Schmon, and N. Tzavidis (2017).
#' Estimating the density of ethnic minorities and aged people in Berlin: Multivariate
#' Kernel Density Estimation applied to sensitive georeferenced administrative data
#' protected via measurement error. Journal of the Royal Statistical Society: Series A
#' (Statistics in Society), 180.
#' @docType package
#' @name ICD
NULL
