# Internal documentation -------------------------------------------------------

# The function estimates the pre-defined indicators

indicators.est <- function(x,threshold, custom_indicator, weights) {

  if (is.null(weights)) {
  if(is.na(threshold)) {
    threshold <- 0.6*median(x)
  } else {
    threshold <- threshold*median(x)
  }

results <- c(mean = mean(x),
    gini = ineq(x),
    hcr = mean(x<threshold),
    quant10 = unname(quantile(x, probs = c(0.1))),
    quant25 = unname(quantile(x, probs = c(0.25))),
    quant50 = unname(quantile(x, probs = c(0.50))),
    quant75 = unname(quantile(x, probs = c(0.75))),
    quant90 = unname(quantile(x, probs = c(0.90))) ,
    pgap = mean((x<threshold)*(threshold-x)/threshold),
    qsr = sum(x[(x>quantile(x,0.8))])/sum(x[(x<quantile(x,0.2))])
    )

if (!is.null(custom_indicator)){
c.indicator <- NULL
for(i in 1:length(custom_indicator)) {
  c.indicator <- c(c.indicator, custom_indicator[[i]](x, threshold))
}
names(c.indicator) <- names(custom_indicator)
results <- c(results, c.indicator)
}
  } else {
    sw <- sum(weights)
    if(is.na(threshold)) {
      thresholdW <-  0.6 * weighted.quantile(x,
                                                   w = weights,
                                                   probs = .5)
    } else {
      thresholdW <-  threshold * weighted.quantile(x,
                                                   w = weights,
                                                   probs = .5)
    }

    results <- c(mean = weighted.mean(x, w = weights),
    gini =  gini(x, weights = weights)$value/100,
    hcr =  arpr(x, weights = weights, p = threshold)$value/100,
    quant10 =  unname(weighted.quantile(x, w = weights, probs = 0.1)),
    quant25 =  unname(weighted.quantile(x, w = weights, probs = 0.25)),
    quant50 =  unname(weighted.quantile(x, w = weights, probs = 0.5)),
    quant75 =  unname(weighted.quantile(x, w = weights, probs = 0.75)),
    quant90 =  unname(weighted.quantile(x, w = weights, probs = 0.9)),
    pgap = sum(weights * (x < thresholdW) * (thresholdW - x)
                       / thresholdW) / sw,
    qsr = qsr(x, weights = weights)$value)

    if (!is.null(custom_indicator)){
      c.indicator <- NULL
      for(i in 1:length(custom_indicator)) {
        c.indicator <- c(c.indicator, custom_indicator[[i]](x, threshold, weights))
      }
      names(c.indicator) <- names(custom_indicator)
      results <- c(results, c.indicator)

    }
  }

return(results)

}
