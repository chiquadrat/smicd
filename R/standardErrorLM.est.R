# Internal documentation -------------------------------------------------------
#
# Estimate standard errors

standardErrorLM.est <- function(formula, data, classes, burnin, samples, trafo,
                                adjust, b) {
  coef <- NULL
  pb <- txtProgressBar(min = 1, max = b, style = 3)
  print("Bootstrapping...")
  for (i in 1:b) {
    boot_rows <- sample(nrow(data), nrow(data), replace = TRUE)
    boot_data <- data[boot_rows, ]
    capture.output(SEM <- semLm(
      formula = formula, data = boot_data, classes = classes,
      burnin = burnin, samples = samples, trafo = trafo,
      adjust = adjust, bootstrap.se = FALSE, b = b
    ))
    coef <- rbind(coef, SEM$coef)
    setTxtProgressBar(pb, i)
  }

  se.boot <- apply(coef, 2, function(x) {
    sqrt(mean((x - mean(x))^2))
  })

  ci <- apply(coef, 2, function(x) {
    x <- sort(x)
    ci.lower <- ifelse(round(length(x) * .025) < 1, x[1], x[round(length(x) * .025)])
    ci.upper <- ifelse(round(length(x) * .975) > length(x), x[length(x)],
      x[round(length(x) * .975)]
    )
    return(list(ci.low = ci.lower, ci.up = ci.upper))
  })

  ci.final <- NULL
  for (i in 1:length(ci)) {
    ci.final <- rbind(ci.final, c(ci[[i]]$ci.low, ci[[i]]$ci.up))
  }

  est <- list(se = se.boot, ci = ci.final)
  return(est)
}
