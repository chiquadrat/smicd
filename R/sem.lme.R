#' @title Linear Mixed Regression with Interval Censored Dependent Variable
#'
#' @description This function estimates the mixed linear regression model when
#' the dependent
#' variable is interval censored. The estimation of the standard errors is
#' fasciliated by a parametric bootstrap.
#' @param formula a two-sided linear formula object describing both the fixed-effects
#' and random-effects part of the model, with the response on the left of a ~ operator
#' and the terms, separated by + operators, on the right. Random-effects terms are
#' distinguished by vertical bars (|) separating expressions for design matrices from
#' grouping factors, as in \code{\link[lme4]{lmer}}. Note: Only random intercept
#' and random slope models are implemented at that point (e.g. \code{y ~ x + (1|
#' ID)}, or \code{y ~ x + (x|ID)}). The
#' dependent variable is measuread as interval censored values; factor with
#' ordered factor values
#' @param data a data frame containing the variables of the model
#' @param classes numeric vector of classes; -Inf as first and Inf as last value
#' is allowed. If the Box-Cox or
#' logarithmic transformation is chosen, the minimum interval bound must be
#' \eqn{\ge 0}.
#' @param burnin the number of burn-in iterations of the SEM-algorithm
#' @param samples the number of additional iterations of the SEM-algorithm
#' for parameter estimation
#' @param trafo transformation of the dependent variable to fulfil the model assumptions
#'   \itemize{
#'   \item "log" for Logarithmic transformation
#'   \item "bc" for Box-Cox transformation
#' }
#' default is \code{"None"}. Transformations can only be used if the minimum
#' interval bound is \eqn{\ge 0}.
#' @param adjust extends the number of iteration steps of the SEM-algorithm
#' for finding the optimal lambda of the Box-Cox transformation. The number of iterations
#' is extended in the following way: \code{(burnin+samples)*adjust}
#' @param bootstrap.se if \code{TRUE} standard errors for the regression parameters
#'  are estimated
#' @param b number of bootstrap iterations for the estimation of the standard errors
#' @return An object of class "sem" that provides parameter estimated for linear
#' regression models with interval censored dependent variable. Generic
#' functions such as, \code{\link{print}},
#' \code{\link{plot}}, and \code{\link{summary}} have methods that can be used
#' to obtain further information. See \code{\link{semObject}} for descriptions
#' of components
#' of objects of class "sem".
#' @details The model parameters are estimated using pseudo samples of the
#' interval censored dependent variable. The object \code{pseudo.y} returns the
#' pseudo samples of each iteration step of the SEM-algorithm.
#' @references
#' Walter, P., Gross, M., Schmid, T. and Tzavidis, N. (2017). Estimation of
#' Linear and Non-Linear Indicators
#' using Interval Censored Income Data. FU-Berlin School of Business & Economics,
#' Discussion
#' Paper.
#' @seealso \code{\link[lme4]{lmer}}, \code{\link{print.sem}},
#' \code{\link{plot.sem}}, \code{\link{summary.sem}}
#' @export
#' @importFrom truncnorm rtruncnorm
#' @import formula.tools
#' @importFrom lme4 lmer ranef VarCorr
#' @importFrom MuMIn r.squaredGLMM
#' @importFrom mvtnorm rmvnorm
#' @return NULL
#' @examples
#' \donttest{
#' # Load and prepare data
#' data <- Exam
#' classes <- c(1,1.5,2.5,3.5,4.5,5.5,6.5,7.7,8.5, Inf)
#' data$examsc.class<- cut(data$examsc, classes)
#'
#' # Run model with random intercept and default settings
#' model1 <- semLme(formula = examsc.class ~ standLRT + schavg + (1|school),
#' data = data, classes = classes)
#' summary(model1)
#'
#' # Run model with random intercept + random slope with default settings
#' model2 <- semLme(formula = examsc.class ~ standLRT + schavg +
#' (standLRT|school), data = data, classes = classes)
#' summary(model2)
#' }\dontshow{
#' # Load and prepare data
#' data <- Exam
#' classes <- c(1,1.5,2.5,3.5,4.5,5.5,6.5,7.7,8.5, Inf)
#' data$examsc.class<- cut(data$examsc, classes)
#'
#' #Run model with random intercept and default settings
#' model1 <- semLme(formula = examsc.class ~ standLRT + schavg + (1|school),
#' data = data, classes = classes, burnin = 4, samples = 10)
#' summary(model1)}

semLme <- function(formula, data, classes, burnin = 40, samples = 200, trafo = "None",
                    adjust = 2, bootstrap.se = FALSE, b = 100) {

  call <- match.call()
  o.classes = classes
  o.data = data
  o.formula = formula
  lambda <- result_lambda <- b.lambda <- m.lambda <- se <- ci <- NULL

  if (trafo=="log"){classes <- log.est(y = classes)}
  if (trafo=="bc") {suppressWarnings(lambda.est <-
                                       lambda.lme.est(formula = formula,
                                                      data = data,
                                                      classes = classes,
                                                      burnin = burnin,
                                                      samples = samples,
                                                      adjust = adjust))

  lambda <- lambda.est$lambda
  result_lambda <- lambda.est$it.lambda
  b.lambda <- lambda.est$b.lambda
  m.lambda <- lambda.est$m.lambda

  BoxCoxClasses <- boxcox.lme.est(dat=classes,lambda = lambda,  inverse=FALSE)
  classes <- BoxCoxClasses[[1]]
  }

  data <- midpoints.est(formula = formula, data = data, classes = classes)

  formula <- as.formula(gsub(".*~","pseudoy~",formula))

  regclass <- lmer(formula,data=data)
  resulty <- matrix(ncol = c(burnin + samples), nrow = nrow(data))
  resultcoef <- matrix(ncol = c(burnin + samples), nrow = length(regclass@beta))
  result_ranef <- vector("list", burnin + samples)
  result_sigmae<-vector(mode = "numeric", length = burnin+samples)
  result_r2c<-vector(mode = "numeric", length = burnin+samples)
  result_r2m<-vector(mode = "numeric", length = burnin+samples)
  result_icc <- vector(mode = "numeric", length = burnin+samples)
  VaCovMa <- vector("list", burnin+samples)

  for (j in 1:(burnin + samples)) {
    data$predict <- predict(regclass,data)
    sigmahat <- sigma(regclass)
    for (i in 1:(length(classes) - 1)) {
      if (nrow(data[data$yclassl==i,])!=0) {
        mean <- data$predict[data$yclassl==i]
        pseudoy <- rtruncnorm(length(mean), a=classes[i], b=classes[i+1], mean=mean, sd=sigmahat )
        data$pseudoy[data$yclassl==i] <- pseudoy
      }
    }
    regclass=lmer(formula,data=data )
    resultcoef[,j] <- regclass@beta
    result_ranef[[j]] <- as.matrix(ranef(regclass)[[1]])
    result_sigmae[j]<- sigmahat
    #result_sigmau[j]<-as.data.frame(VarCorr(regclass))$sdcor[1]
    result_r2m[j] <- unname(r.squaredGLMM(regclass)[1])
    result_r2c[j] <- unname(r.squaredGLMM(regclass)[2])
    result_icc[j] <- icc.est(model = regclass)
    resulty[,j] <- data$pseudoy
    VaCovMa[[j]] <- as.matrix(unclass(VarCorr(regclass))[[1]][1:ncol(ranef(regclass)[[1]]),])
  }

  parameter.ma <- list(ranef = result_ranef, VaCov = VaCovMa)
  parameter.final.ma <- parameters.est.ma(parameter = parameter.ma, burnin = burnin)

  colnames(parameter.final.ma$VaCov) <- colnames(VaCovMa[[1]])
  rownames(parameter.final.ma$VaCov) <- rownames(VaCovMa[[1]])

  colnames(parameter.final.ma$ranef) <- colnames(result_ranef[[1]])

  parameter <- list(coef = resultcoef,
                    sigmae = result_sigmae,
                    r2m = result_r2m, r2c = result_r2c,
                    icc = result_icc)
  parameter.final <- parameters.est(parameter = parameter, burnin = burnin)

  names(parameter.final$coef) <- rownames(summary(regclass)$coefficients)

  if (bootstrap.se == TRUE) {result_se <- standardErrorLME.est(formula = o.formula,
                                                               data = o.data,
                                                               classes = o.classes,
                                                               burnin = burnin,
                                                               samples = samples,
                                                               trafo = trafo,
                                                               adjust = adjust,
                                                               b = b,
                                                               coef = parameter.final$coef,
                                                               sigmae = parameter.final$sigmae,
                                                               VaCov = parameter.final.ma$VaCov,
                                                               nameRI = names(ranef(regclass)),
                                                               nameRS = names(ranef(regclass)[[1]])[2],
                                                               regmodell = regclass,
                                                               lambda = m.lambda)

  se <- result_se$se
  ci <- result_se$ci
  rownames(ci) <- names(parameter.final$coef)}

  est <- list(pseudo.y = resulty, coef = parameter.final$coef, ranef = parameter.final.ma$ranef,
              sigmae = parameter.final$sigmae, VaCov = parameter.final.ma$VaCov,
              se = se, ci = ci, lambda = lambda, bootstraps = b, r2m = parameter.final$r2m,
              r2c = parameter.final$r2c, icc = parameter.final$icc, formula = o.formula,
              transformation = trafo, n.classes = length(classes)-1, conv.coef = resultcoef,
              conv.sigmae = result_sigmae, conv.lambda = result_lambda, conv.VaCov = VaCovMa,
              b.lambda = b.lambda, m.lambda = m.lambda, burnin = burnin, samples = samples,
              classes = o.classes, original.y = data$y, call = call)
  class(est) <- c("sem", "lme")
  return(est)
}




