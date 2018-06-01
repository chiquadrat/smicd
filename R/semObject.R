#' Fitted semObject
#'
#' An object of class "sem" that represents the estimated model
#' parameters and standard errors.
#'  Objects of this class have methods for the generic functions
#' \code{\link{print}}, \code{\link{plot}}  and \code{\link{summary}}.
#'
#' @return
#' An object of class "sem" is a list containing the following components. Some
#' parameters are only estimated for liner mixed regression models (and vice versa).
#' \item{\code{pseudo.y}}{a matrix containing the pseudo samples of the interval
#' censored variable from each iteration}
#' \item{\code{coef}}{the estimated coefficients (fixed effects) from the regression model}
#' \item{\code{ranef}}{the estimated rnadom effects from the regression model}
#' \item{\code{sigmae}}{estimated variance  \eqn{\sigma_e}}
#' \item{\code{VaVoc}}{estimated covariance matrix of the random effects}
#' \item{\code{se}}{bootstraped standard error of the coefficients}
#' \item{\code{ci}}{bootstraped 95\% confidence interval for the coefficients}
#' \item{\code{lambda}}{estimated lambda for the Box-Cox transformation}
#' \item{\code{bootstraps}}{number of bootstrap iterations for the estimation
#' of the standard errors}
#' \item{\code{r2}}{estimated coefficient of determination}
#' \item{\code{r2m}}{estimated marginal coefficient of determination for
#' Generalized mixed-effect models, as in \code{\link[MuMIn]{r.squaredGLMM}}}
#' \item{\code{r2c}}{estimated conditional coefficient of determination for
#' Generalized mixed-effect models, as in \code{\link[MuMIn]{r.squaredGLMM}}}
#' \item{\code{icc}}{estimated interclass corelation coefficient}
#' \item{\code{adj.r2}}{estimated adjusted coefficient of determination}
#' \item{\code{formula}}{an object of class \code{formula}, as in \code{\link[stats]{lm}} or
#' \code{\link[lme4]{lmer}}}
#' \item{\code{transformation}}{the specified transformation "log" for logarithmic
#' and "bc" for Box-Cox transformation}
#' \item{\code{n.classes}}{the number of classes, the dependent variable is censored to}
#' \item{\code{conv.coef}}{estimated coefficients for each iteration run of the SEM-algorothm}
#' \item{\code{conv.sigmae}}{estimated variance  \eqn{\sigma_e} for each iteration run of the SEM-algorothm}
#' \item{\code{conv.VaCov}}{estimated covariance  matrix
#' of the random effects for each iteration run of the SEM-algorithm}
#' \item{\code{conv.lambda}}{estimated lambda for the Box-Cox transformation for each
#' iteration step of the SEM-algorithm}
#' \item{\code{b.lambda}}{the number of burn-in iteration of the SEM-algorithm used to
#' estimate lambda}
#' \item{\code{m.lambda}}{the number of additional iteration of the SEM-algorithm used to
#' estimate lambda}
#' \item{\code{burnin}}{the number of burn-in iterations of the SEM-algorithm}
#' \item{\code{samples}}{the number of additional iterations of the SEM-algorithm}
#' \item{\code{classes}}{specified classes}
#' \item{\code{original.y}}{the dependent variable of the regression model measured on
#' an interval censored scale}
#' \item{\code{call}}{the function call}
#' \item{\code{formula}}{the specified formula}
#' @references
#' Walter, P., Gross, M., Schmid, T. and Tzavidis, N. (2017). Estimation of Linear and Non-Linear Indicators
#' using Interval Censored Income Data. School of Business & Economics, Discussion
#' Paper.
#' @seealso \code{\link{ICD}},  \code{\link[stats]{lm}}, \code{\link[lme4]{lmer}},
#' \code{\link[MuMIn]{r.squaredGLMM}}
#' @name semObject
NULL
