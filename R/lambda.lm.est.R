# Internal documentation -------------------------------------------------------
#
# Estimate Lambda LM

lambda.lm.est <- function(formula, data, classes, burnin, samples, adjust) {


  data <- midpoints.est(formula = formula, data = data, classes = classes)

  formula.bc <- as.formula(gsub(".*~","pseudoy~",formula))

  # Transformieren der pseudo y
  BoxCox <- boxcox.lm.est(dat=data, formula = formula.bc, tr = FALSE)
  BoxCox_lambda <- BoxCox$lambdahat
  data$pseudoy <- BoxCox$yt

  # Transformieren der KLassen
  BoxCoxClasses <- boxcox.lme.est(dat=classes,lambda = BoxCox_lambda, inverse=FALSE)
  classesBox <- BoxCoxClasses[[1]]

  regclass <- lm(formula.bc,data=data )

  it_lambda <- (burnin+samples)*adjust

  result_lambda <- vector(mode = "numeric", length = it_lambda)
  pb <- txtProgressBar(min = 1, max = it_lambda, style = 3)
  print("Box-Cox is slow, sry :/")
  for (j in 1:(it_lambda)) {
    data$predict <- predict(regclass,data)
    sigmahat <- stats::sigma(regclass)
    for (i in 1:(length(classesBox) - 1)) {
      if (nrow(data[data$yclassl==i,])!=0) {
        mean <- data$predict[data$yclassl==i]
        pseudoy <- rtruncnorm(length(mean), a=classesBox[i], b=classesBox[i+1], mean=mean, sd=sigmahat )
        data$pseudoy[data$yclassl==i] <- pseudoy
      }
    }

    result_lambda[j] <- BoxCox_lambda

    # Jetzt die Daten Rücktransformieren
    rueck <- boxcox.lme.est(dat=data, lambda = BoxCox_lambda, inverse = T)
    data$pseudoy <- rueck[[1]]

    # Transform data and obtain lambda
    BoxCox <- boxcox.lm.est(dat=data, inverse=FALSE, formula = formula.bc)
    BoxCox_lambda <- BoxCox$lambdahat
    data$pseudoy <- BoxCox$yt

    # Jetzt die Klassen neu transformieren
    BoxCoxClasses <- boxcox.lme.est(dat=classes,lambda = BoxCox_lambda,  inverse=FALSE)
    classesBox <- BoxCoxClasses[[1]]

    # Jetzt das Modell neu berechnen
    regclass = lm(formula.bc,data=data)
    setTxtProgressBar(pb, j)
  }


  # Jetzt mit dem optimierten Lambda, die Daten transformieren und den Algorithmus erneut berechnen
  lambda <- mean(result_lambda[-c(1:(burnin*adjust))])
  est <- list(lambda = lambda, it.lambda = result_lambda, b.lambda = burnin*adjust,
              m.lambda = samples*adjust)
  return(est)

}
