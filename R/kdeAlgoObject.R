#' Fitted kdeAlgoObject
#'
#' An object of class "kdeAlgo" that represents the estimated
#' statistical indicators and the estimated standard errors.
#' Objects of this class have methods for the generic functions
#' \code{\link{print}} and \code{\link{plot}}.
#'
#' @return
#' An object of class "kdeAlgo" is a list containing at least the following
#' components.
#' \item{\code{Point_estimate}}{the estimated statistical indicators:
#' Mean, Gini, Head-Count Ratio, Quantiles (10\%, 25\%, 50\%, 75\%,
#' 90\%), Poverty-Gap, Quintile-Share Ratio and if specified the selected
#' custom indicators.}
#' \item{\code{Standard_Error}}{if \code{bootstrap.se = TRUE},
#' the standard errors for the statistical indicator are estimated}
#' \item{\code{Mestimates}}{kde object containing the corrected density estimate,
#' as in \code{\link[Kernelheaping]{dclass}}}
#' \item{\code{resultDensity}}{estimated density for each iteration,
#' as in \code{\link[Kernelheaping]{dclass}}}
#' \item{\code{resultX}}{true latent values X estimates,
#' as in \code{\link[Kernelheaping]{dclass}}}
#' \item{\code{xclass}}{classified values; factor with ordered factor values,
#' as in \code{\link[Kernelheaping]{dclass}}}
#' \item{\code{gridx}}{grid on which density is evaluated,
#' as in \code{\link[Kernelheaping]{dclass}}}
#' \item{\code{classes}}{classes; Inf as last value is allowed,
#' as in \code{\link[Kernelheaping]{dclass}}}
#' \item{\code{burnin}}{burn-in sample size,
#' as in \code{\link[Kernelheaping]{dclass}}}
#' \item{\code{samples}}{sampling iteration size,
#' as in \code{\link[Kernelheaping]{dclass}}}
#' \item{\code{Point_estimates.run}}{the estimated statistical indicators:
#' Mean, Gini, Head-Count Ratio, Quantiles (10\%, 25\%, 50\%, 75\%,
#' 90\%), Poverty-Gap, Quintile-Share Ratio and if specified the selected
#' custom indicators for each iteration run of the
#' KDE-algorithm}
#' \item{\code{oecd}}{the weights used for the estimation of the equivalised
#' household income}
#' \item{\code{weights}}{any kind of survey or design weights that will be used
#' for the weighted estimation of the statistical indicators}
#' \item{\code{upper}}{if the upper bound of the upper interval is \code{Inf} e.g.
#' \code{(15000,Inf)}, then \code{Inf} is replaced by \code{15000*upper}}
#' @references
#' Walter, P., Weimer, K. (2018). Estimating Poverty and Inequality Indicators
#' using Interval Censored Income Data from the German Microcensus.
#' FU-Berlin School of Business & Economics, Discussion
#' Paper. \cr \cr
#' Groß, M., U. Rendtel, T. Schmid, S. Schmon, and N. Tzavidis (2017).
#' Estimating the density of ethnic minorities and aged people in Berlin: Multivariate
#' Kernel Density Estimation applied to sensitive georeferenced administrative data
#' protected via measurement error. Journal of the Royal Statistical Society: Series A
#' (Statistics in Society), 180.
#' @seealso \code{\link{smicd}},  \code{\link[Kernelheaping]{dclass}}
#' @name kdeAlgoObject
NULL
