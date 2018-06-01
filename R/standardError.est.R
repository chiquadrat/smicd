# Internal documentation -------------------------------------------------------

# The function estimates the standard errors of the indicators

standardError.est <- function(b, xclass, classes, burnin, samples, boundary, bw, evalpoints, adjust, threshold,
                              custom_indicator){

  pb <- txtProgressBar(min = 1, max = b, style = 3)
  results.kernel <- NULL
  for (i in 1:b) {
    Sys.sleep(0.1)
    boot_samp <- xclass[sample(length(xclass), size=length(xclass), replace = TRUE)]

    capture.output(densityEst <- dclass(xclass = xclass, classes = classes,
                         burnin = burnin, samples = samples, boundary = boundary, bw = bw,
                         evalpoints = evalpoints, adjust = adjust))

    results.kernel <- rbind(results.kernel, rowMeans(apply(densityEst$resultX[,-c(1:burnin)],2,
                                                           function(x) {indicators.est(x,threshold,
                                                                                       custom_indicator)})))
    setTxtProgressBar(pb, i)
  }

  se.boot <- apply(results.kernel,2,function(x){sqrt(mean((x-mean(x))^2))})

  return(se.boot)
}

