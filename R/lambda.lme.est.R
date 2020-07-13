# Internal documentation -------------------------------------------------------
#
# Estimate Lambda LME

lambda.lme.est <- function(formula, data, classes, burnin, samples, adjust) {
  data <- midpoints.est(formula = formula, data = data, classes = classes)

  formula.bc <- as.formula(gsub(".*~", "pseudoy~", formula))

  # transform data and obtain lambda
  BoxCox <- boxcox.lme.est(dat = data, inverse = FALSE, formula = formula.bc)
  BoxCox_lambda <- BoxCox[[3]]
  data$pseudoy <- BoxCox[[1]]

  # transform classes
  BoxCoxClasses <- boxcox.lme.est(dat = classes, lambda = BoxCox_lambda, inverse = FALSE)
  classesBox <- BoxCoxClasses[[1]]

  regclass <- lmer(formula.bc, data = data)
  it_lambda <- (burnin + samples) * adjust

  result_lambda <- vector(mode = "numeric", length = it_lambda)
  pb <- txtProgressBar(min = 1, max = it_lambda, style = 3)
  print("Box-Cox is slow, sry :/")
  for (j in 1:(it_lambda)) {
    Sys.sleep(0.1)
    data$predict <- predict(regclass, data)
    sigmahat <- sigma(regclass)
    for (i in 1:(length(classesBox) - 1)) {
      if (nrow(data[data$yclassl == i, ]) != 0) {
        mean <- data$predict[data$yclassl == i]
        pseudoy <- rtruncnorm(length(mean),
          a = classesBox[i], b = classesBox[i + 1],
          mean = mean, sd = sigmahat
        )
        data$pseudoy[data$yclassl == i] <- pseudoy
      }
    }
    result_lambda[j] <- BoxCox_lambda

    # Transform the data back
    rueck <- boxcox.lme.est(dat = data, lambda = BoxCox_lambda, inverse = T)
    data$pseudoy <- rueck[[1]]

    # Transform data and obtain lambda
    BoxCox <- boxcox.lme.est(dat = data, inverse = FALSE, formula = formula.bc)
    BoxCox_y <- BoxCox[[1]]
    BoxCox_lambda <- BoxCox[[3]]
    data$pseudoy <- BoxCox_y

    # Transform classes with next lambda
    BoxCoxClasses <- boxcox.lme.est(dat = classes, lambda = BoxCox_lambda, inverse = FALSE)
    classesBox <- BoxCoxClasses[[1]]
    classesM <- BoxCoxClasses[[2]]

    regclass <- lmer(formula.bc, data = data)
    setTxtProgressBar(pb, j)
  }

  lambda <- mean(result_lambda[-c(1:(burnin * adjust))])
  est <- list(
    lambda = lambda, it.lambda = result_lambda, b.lambda = burnin * adjust,
    m.lambda = samples * adjust
  )
  return(est)
}
