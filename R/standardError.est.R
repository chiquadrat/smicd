# Internal documentation -------------------------------------------------------

# The function estimates the standard errors of the indicators

standardError.est <- function(b, xclass, classes, burnin, samples, boundary, bw, evalpoints, adjust, threshold,
                              custom_indicator, upper, weights, oecd){

  pb <- txtProgressBar(min = 1, max = b, style = 3)
  results.kernel <- NULL
  for (i in 1:b) {
    Sys.sleep(0.1)
    boot_samp <- xclass[sample(length(xclass), size=length(xclass), replace = TRUE)]

    density.est <- dclassICD(xclass = xclass, classes = classes,
                         burnin = burnin, samples = samples, boundary = boundary, bw = bw,
                         evalpoints = evalpoints, adjust = adjust, upper = upper,
                         weights= weights, oecd = oecd)

    Indicators.run <- NULL
    for (j in 1:dim(density.est$resultX)[2]) {
      Indicators.run <- cbind(Indicators.run,indicators.est(x=density.est$resultX[,j],
                                                            threshold = threshold,
                                                            custom_indicator = custom_indicator,
                                                            weights = density.est$resultW[,j]))

    }
    results.kernel <- rbind(results.kernel,rowMeans(Indicators.run))


    setTxtProgressBar(pb, i)
  }

  se.boot <- apply(results.kernel,2,function(x){sqrt(mean((x-mean(x))^2))})

  return(se.boot)
}

