# Internal documentation -------------------------------------------------------

# The function estimates the interval midpoints

midpoints.est <- function(formula, data, classes) {
  model_frame <- model.frame(formula, data = data)
  yclass <- model.response(model_frame)
  yclassl <- model.response(model_frame)
  levels(yclassl) <- 1:length(levels(yclassl))
  yclassl <- as.numeric(as.vector(yclassl))
  data$yclassl <- yclassl
  intervals <- vector("list", length(classes) - 1)

  for (i in seq(length = length(classes) - 1)) {
    intervals[[i]] <- c(classes[i], classes[i + 1])
  }

  means <- sapply(intervals, mean)
  widths <- sapply(intervals, function(x) x[2] - x[1])
  meanWidth <- mean(widths[!is.infinite(widths)])

  negInf <- is.infinite(means) & means < 0
  if (any(negInf)) {
    means[negInf] <- sapply(intervals[negInf], function(x) {
      (x[2] + (x[2] -
        meanWidth)) / 2
    })
  }
  posInf <- is.infinite(means) & means > 0
  if (any(posInf)) {
    means[posInf] <- sapply(intervals[posInf], function(x) {
      (x[1] + (x[1] +
        meanWidth)) / 2
    })
  }

  yclassmeans <- means
  levels(yclass) <- yclassmeans
  data$pseudoy <- as.numeric(as.vector(yclass))
  return(data)
}
