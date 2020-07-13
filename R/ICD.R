#' Statistical Methods for Interval Censored (Grouped) Data
#'
#' The package \pkg{smicd} supports the estimation of linear and linear mixed regression
#' models (random slope and random intercept models)
#' with interval censored dependent variable. Parameter estimates are obtain
#' by a stochastic expectation maximization (SEM) algorithm (\cite{Walter, 2019}).
#' Standard errors are estimated by a non-parametric bootstrap in the linear
#' regression model and by a parametric bootstrap in the linear mixed regression
#' model. To handle departures
#' from the model assumptions transformations (log and Box-Cox) of the interval censored
#' dependent variable are incorporated into
#' the algorithm (\cite{Walter, 2019}). Furthermore, the package \pkg{smicd} has
#' implemented a non-parametric kernel density algorithm for the direct (without
#' covariates) estimation of statistical indicators from interval censored data
#' (\cite{Walter, 2019}; \cite{Gross et al., 2017}). The standard
#' errors of the statistical indicators
#' are estimated by a non-parametric bootstrap.
#'
#' @details
#' The two estimation functions for the linear and linear mixed regression model
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
#' \code{library(help=smicd)}.
#'
#' @references
#' Walter, P. (2019). A Selection of Statistical Methods for Interval-Censored
#' Data with Applications to the German Microcensus, PhD thesis,
#' Freie Universitaet Berlin\cr \cr
#' Gross, M., U. Rendtel, T. Schmid, S. Schmon, and N. Tzavidis (2017).
#' Estimating the density of ethnic minorities and aged people in Berlin: Multivariate
#' Kernel Density Estimation applied to sensitive georeferenced administrative data
#' protected via measurement error. Journal of the Royal Statistical Society: Series A
#' (Statistics in Society), 180.
#' @docType package
#' @name smicd
NULL
