#' @title Linear Regression with Interval-Censored Dependent Variable
#'
#' @description This function estimates the linear regression model when the
#' dependent
#' variable is interval-censored. The estimation of the standard errors is
#' fasciliated
#' by a non-parametric bootstrap.
#' @param formula an object of class \code{formula}, as in
#' \code{\link[stats]{lm}}. The
#' dependent variable is measured as interval-censored values; factor with
#' ordered factor values
#' @param data a data frame containing the variables of the model
#' @param classes numeric vector of classes; \code{-Inf} as lower interval bound and
#' \code{Inf} as upper interval bound
#' is allowed. If the Box-Cox or
#' logarithmic transformation is chosen, the minimum interval bound must be
#' \eqn{\ge 0}.
#' @param burnin the number of burn-in iterations of the SEM-algorithm
#' @param samples the number of additional iterations of the SEM-algorithm
#' for parameter estimation
#' @param trafo transformation of the dependent variable to fulfill the model
#' assumptions
#'   \itemize{
#'   \item "log" for Logarithmic transformation
#'   \item "bc" for Box-Cox transformation
#' }
#' default is \code{"None"}. Transformations can only be used if the minimum
#' interval bound is \eqn{\ge 0}.
#' @param adjust extends the number of iteration steps of the SEM-algorithm
#' for finding the optimal lambda of the Box-Cox transformation. The number of
#' iterations
#' is extended in the following way: \code{(burnin+samples)*adjust}
#' @param bootstrap.se if \code{TRUE} standard errors of the regression
#' parameters
#'  are estimated
#' @param b number of bootstrap iterations for the estimation of the standard
#' errors
#' @return An object of class "sem" that provides parameter estimates for linear
#' regression models with interval-censored dependent variable. Generic
#' functions such as, \code{\link{print}},
#' \code{\link{plot}}, and \code{\link{summary}} have methods that can be used
#' to obtain further information.  See \code{\link{semObject}} for a description
#' of the components
#' of objects of class "sem".
#' @details The model parameters are estimated using pseudo samples as a proxy
#'  for the
#' interval-censored dependent variable. The object \code{pseudo.y} returns the
#' pseudo samples of each iteration step of the SEM-algorithm.
#' @references
#' Walter, P. (2019). A Selection of Statistical Methods for Interval-Censored
#' Data with Applications to the German Microcensus, PhD thesis,
#' Freie Universitaet Berlin
#' @seealso \code{\link[stats]{lm}}, \code{\link{print.sem}},
#' \code{\link{plot.sem}}, \code{\link{summary.sem}}
#' @export
#' @importFrom truncnorm rtruncnorm
#' @import formula.tools
#' @importFrom stats as.formula density lm logLik median model.frame
#' @importFrom stats model.matrix model.response optimise optimize predict
#' @importFrom stats quantile rnorm sigma
#' @importFrom utils capture.output setTxtProgressBar tail txtProgressBar
#' @return NULL
#' @examples
#' \dontrun{
#' # Load and prepare data
#' data <- Exam
#' classes <- c(1, 1.5, 2.5, 3.5, 4.5, 5.5, 6.5, 7.7, 8.5, Inf)
#' data$examsc.class <- cut(data$examsc, classes)
#'
#' # Run model with default settings
#' model <- semLm(
#'   formula = examsc.class ~ standLRT + schavg, data = data,
#'   classes = classes
#' )
#' summary(model)
#' }
#' \dontshow{
#' # Load and prepare data
#' data <- Exam
#' classes <- c(1, 1.5, 2.5, 3.5, 4.5, 5.5, 6.5, 7.7, 8.5, Inf)
#' data$examsc.class <- cut(data$examsc, classes)
#'
#' # Run model with default settings
#' model <- semLm(
#'   formula = examsc.class ~ standLRT + schavg, data = data,
#'   classes = classes, burnin = 4, samples = 10
#' )
#' summary(model)
#' }
#'
semLm <- function(formula, data, classes, burnin = 40, samples = 200, trafo = "None",
                  adjust = 2, bootstrap.se = FALSE, b = 100) {
  call <- match.call()

  o.data <- data
  o.formula <- formula
  o.classes <- classes
  lambda <- result_lambda <- b.lambda <- m.lambda <- se <- ci <- NULL

  if (trafo == "log") {
    classes <- logEst(y = classes)
  }
  if (trafo == "bc") {
    lambda.est <- lambda.lm.est(
      formula = formula, data = data, burnin = burnin,
      classes = classes, samples = samples, adjust = adjust
    )
    lambda <- lambda.est$lambda
    result_lambda <- lambda.est$it.lambda
    b.lambda <- lambda.est$b.lambda
    m.lambda <- lambda.est$m.lambda

    BoxCoxClasses <- boxcox.lme.est(dat = classes, lambda = lambda, inverse = FALSE)
    classes <- BoxCoxClasses[[1]]
  }

  data <- midpoints.est(formula = formula, data = data, classes = classes)

  formula <- as.formula(gsub(".*~", "pseudoy~", formula))
  regclass <- lm(formula, data = data)

  resulty <- matrix(ncol = c(burnin + samples), nrow = nrow(data))
  resultcoef <- matrix(ncol = c(burnin + samples), nrow = length(regclass$coefficients))
  result_sigmae <- vector(mode = "numeric", length = burnin + samples)
  result_r2 <- vector(mode = "numeric", length = burnin + samples)
  result_r2adj <- vector(mode = "numeric", length = burnin + samples)

  for (j in 1:(burnin + samples)) {
    data$predict <- predict(regclass, data)
    sigmahat <- stats::sigma(regclass)
    for (i in 1:(length(classes) - 1)) {
      if (nrow(data[data$yclassl == i, ]) != 0) {
        mean <- data$predict[data$yclassl == i]
        pseudoy <- rtruncnorm(length(mean),
          a = classes[i], b = classes[i + 1],
          mean = mean, sd = sigmahat
        )
        data$pseudoy[data$yclassl == i] <- pseudoy
      }
    }
    regclass <- lm(formula, data = data)
    resultcoef[, j] <- regclass$coefficients
    result_sigmae[j] <- sigmahat
    result_r2[j] <- summary(regclass)$r.squared
    result_r2adj[j] <- summary(regclass)$adj.r.squared
    resulty[, j] <- data$pseudoy
  }



  parameter <- list(
    coef = resultcoef, sigmae = result_sigmae, r2 = result_r2,
    adjr2 = result_r2adj
  )
  parameter.final <- parameters.est(parameter = parameter, burnin = burnin)

  names(parameter.final$coef) <- names(regclass$coefficients)


  if (bootstrap.se == TRUE) {
    result_se <- standardErrorLM.est(
      formula = o.formula,
      data = o.data,
      classes = o.classes,
      burnin = burnin,
      samples = samples,
      trafo = trafo,
      adjust = adjust,
      b = b
    )
    se <- result_se$se
    ci <- result_se$ci
    rownames(ci) <- names(parameter.final$coef)
  }



  est <- list(
    pseudo.y = resulty, coef = parameter.final$coef, sigmae = parameter.final$sigmae,
    se = se, ci = ci, lambda = lambda, bootstraps = b, r2 = parameter.final$r2,
    adj.r2 = parameter.final$adjr2, formula = o.formula,
    transformation = trafo, n.classes = length(classes) - 1, conv.coef = resultcoef,
    conv.sigmae = result_sigmae, conv.lambda = result_lambda, b.lambda = b.lambda,
    m.lambda = m.lambda, burnin = burnin, samples = samples, classes = o.classes,
    original.y = data$y, call = call
  )
  class(est) <- c("sem", "lm")
  return(est)
}
