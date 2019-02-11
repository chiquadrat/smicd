# Internal documentation -------------------------------------------------------

# The function estimates the standard errors of the indicators

standardError.est <- function(b, xclass, classes, burnin, samples, bw, evalpoints, adjust, threshold,
                              custom_indicator, upper, weights, oecd){

  pb <- txtProgressBar(min = 1, max = b, style = 3)
  results.kernel <- NULL
  for (i in 1:b) {
    Sys.sleep(0.1)

    if (is.null(weights) & is.null(oecd)) {
      boot_samp <- xclass[sample(length(xclass), size=length(xclass), replace = TRUE)]
      boot_weights <- weights
      oecd_weights <- oecd
    }

    if (!is.null(weights) & is.null(oecd)) {
      bdata <- data.frame(xclass, weights)
      boot_index <- sample(1:nrow(bdata), size = nrow(bdata), replace = TRUE)
      boot_data <- bdata[boot_index,]
      boot_samp <- boot_data$xclass
      boot_weights <- boot_data$weights
      oecd_weights <- oecd
    }

    if (!is.null(weights) & !is.null(oecd)) {
      bdata <- data.frame(xclass, weights, oecd)
      boot_index <- sample(1:nrow(bdata), size = nrow(bdata), replace = TRUE)
      boot_data <- bdata[boot_index,]
      boot_samp <- boot_data$xclass
      boot_weights <- boot_data$weights
      oecd_weights <- boot_data$oecd
    }

    if (is.null(weights) & !is.null(oecd)) {
      bdata <- data.frame(xclass, oecd)
      boot_index <- sample(1:nrow(bdata), size = nrow(bdata), replace = TRUE)
      boot_data <- bdata[boot_index,]
      boot_samp <- boot_data$xclass
      boot_weights <- weights
      oecd_weights <- boot_data$oecd
    }


    density.est <- dclassICD(xclass = boot_samp, classes = classes,
                         burnin = burnin, samples = samples, bw = bw,
                         evalpoints = evalpoints, adjust = adjust, upper = upper,
                         weights = boot_weights, oecd = oecd_weights)

    Indicators.run <- NULL
    for (j in (burnin+1):dim(density.est$resultX)[2]) {
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

