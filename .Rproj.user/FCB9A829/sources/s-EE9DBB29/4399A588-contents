# Internal documentation -------------------------------------------------------
#
# dclassICD


dclassICD <- function (xclass, classes, burnin, samples, boundary,
          bw, evalpoints, adjust, upper, weights)
{
  xclassF <- xclass
  if (max(classes) == Inf) {
    classes[length(classes)] = upper * classes[length(classes) -
                                             1]
  }
  classmeans <- sapply(1:(length(classes) - 1), function(x) 1/2 *
                         (classes[x + 1] + classes[x]))
  levels(xclass) <- classmeans
  lengths = as.vector(table(xclass))
  xclass <- as.numeric(as.character(xclass))
  gridx = seq(min(classes), max(classes), length = evalpoints)
  if (boundary == FALSE) {
    Mestimates <- density(xclass, from = min(gridx), to = max(gridx),
                          n = length(gridx), bw = 2 * max(classes)/length(classes))$y
  }
  if (boundary == TRUE) {
    Mestimates <- dbc(gridx = gridx, x = xclass, bw = 2 *
                        max(classes)/length(classes))
  }
  resultDensity = matrix(ncol = c(burnin + samples), nrow = length(gridx))
  resultX = matrix(ncol = c(burnin + samples), nrow = length(xclass))
  resultW = matrix(ncol = c(burnin + samples), nrow = length(xclass))
  selectionGrid <- lapply(1:(length(classes) - 1), function(k) {
    selection = which(gridx >= classes[k] & gridx < classes[k +
                                                              1])
    selection
  })
  if(!is.null(weights)){data <- cbind(xclassF,weights)}
  for (j in 1:(burnin + samples)) {
    new = c()
    newW = c()
    for (i in 1:(length(classes) - 1)) {
      probs = as.vector(Mestimates[selectionGrid[[i]]])
      points = gridx[selectionGrid[[i]]]
      npoints = lengths[i]
      new = c(new, points[sample(1:length(points), size = npoints,
                                 replace = T, prob = probs)])
      if(!is.null(weights)){newW = c(newW,  data[data[,1]==i,][,2])}

    }
    NewDensity <- density(new, from = min(gridx), to = max(gridx),
                          n = length(gridx), bw = bw, adjust = adjust)
    Mestimates <- NewDensity$y
    if (boundary == TRUE) {
      Mestimates <- dbc(gridx = gridx, x = new, bw = NewDensity$bw)
    }
    resultDensity[,j] = Mestimates
    resultX[,j] = new
    if(!is.null(weights)){resultW[,j] = newW}
    print(paste("Iteration:", j, "of", burnin + samples))
  }
  if (is.null(weights)){resultW <- NULL}
  Mestimates = apply(resultDensity[, -c(1:burnin)], c(1), mean)
  est <- list(Mestimates = Mestimates, resultDensity = resultDensity,
              resultX = resultX, xclass = xclass, gridx = gridx, classes = classes,
              burnin = burnin, samples = samples, resultW = resultW)
  class(est) <- "classDensity"
  return(est)
}

