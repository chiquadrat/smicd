# Internal documentation -------------------------------------------------------
#
# dclassICD


dclassICD <- function(xclass, classes, burnin, samples,
                      bw, evalpoints, adjust, upper, weights, oecd) {
  if (is.null(oecd)) {
    xclassF <- xclass
    if (max(classes) == Inf) {
      classes[length(classes)] <- upper * classes[length(classes) -
        1]
    }
    classmeans <- sapply(1:(length(classes) - 1), function(x) {
      1 / 2 *
        (classes[x + 1] + classes[x])
    })
    levels(xclass) <- classmeans
    lengths <- as.vector(table(xclass))
    xclass <- as.numeric(as.character(xclass))
    gridx <- seq(min(classes), max(classes), length = evalpoints)

    Mestimates <- density(xclass,
      from = min(gridx), to = max(gridx),
      n = length(gridx), bw = 2 * max(classes) / length(classes)
    )$y

    resultDensity <- matrix(ncol = c(burnin + samples), nrow = length(gridx))
    resultX <- matrix(ncol = c(burnin + samples), nrow = length(xclass))
    resultW <- matrix(ncol = c(burnin + samples), nrow = length(xclass))
    selectionGrid <- lapply(1:(length(classes) - 1), function(k) {
      selection <- which(gridx >= classes[k] & gridx < classes[k +
        1])
      selection
    })
    if (!is.null(weights)) {
      data <- cbind(xclassF, weights)
    }
    for (j in 1:(burnin + samples)) {
      new <- c()
      newW <- c()
      for (i in 1:(length(classes) - 1)) {
        probs <- as.vector(Mestimates[selectionGrid[[i]]])
        points <- gridx[selectionGrid[[i]]]
        npoints <- lengths[i]
        new <- c(new, points[sample(1:length(points),
          size = npoints,
          replace = T, prob = probs
        )])
        if (!is.null(weights)) {
          newW <- c(newW, data[data[, 1] == i, ][, 2])
        }
      }
      NewDensity <- density(new,
        from = min(gridx), to = max(gridx),
        n = length(gridx), bw = bw, adjust = adjust
      )
      Mestimates <- NewDensity$y

      resultDensity[, j] <- Mestimates
      resultX[, j] <- new
      if (!is.null(weights)) {
        resultW[, j] <- newW
      }
    }
    if (is.null(weights)) {
      resultW <- NULL
    }
    Mestimates <- apply(resultDensity[, -c(1:burnin)], c(1), mean)
  } else {
    if (is.null(weights)) {
      weights <- rep(1, length(xclass))
    }
    data <- cbind(xclass, weights, oecd)
    LB <- NULL
    UB <- NULL
    for (i in 1:nrow(data)) {
      LB <- c(LB, classes[data[i, 1]])
      UB <- c(UB, classes[data[i, 1] + 1])
    }
    data <- data.frame(xclass, weights, oecd, LB, UB)
    data$UB[data$UB == Inf] <- upper * classes[length(classes) - 1]
    data$LBadj <- data$LB / data$oecd
    data$UBadj <- data$UB / data$oecd
    data$NIntervals <- paste0(round(data$LBadj, digits = 0), round(data$UBadj,
      digits = 0
    ))
    data$Mid <- 1 / 2 * (data$UBadj + data$LBadj)
    gridx <- seq(min(data$LBadj), max(data$UBadj), length = evalpoints)

    Mestimates <- density(data$Mid,
      from = min(gridx), to = max(gridx),
      n = length(gridx), bw = 2 * max(data$UBadj) /
        length(unique(data$NIntervals))
    )$y

    resultDensity <- matrix(ncol = c(burnin + samples), nrow = length(gridx))
    resultX <- matrix(ncol = c(burnin + samples), nrow = length(data$Mid))
    resultW <- matrix(ncol = c(burnin + samples), nrow = length(xclass))

    for (j in 1:(burnin + samples)) {
      new <- c()
      newWeights <- c()
      for (i in 1:(length(unique(data$NIntervals)))) {
        dataTemp <- data[data$NIntervals == unique(data$NIntervals)[i], ]
        LBTemp <- unique(dataTemp$LBadj)
        UBTemp <- unique(dataTemp$UBadj)
        selectionGrid <- gridx[gridx >= LBTemp & gridx < UBTemp]
        newWeights <- c(newWeights, dataTemp$weights)
        probs <- as.vector(Mestimates[gridx %in% selectionGrid])
        points <- gridx[gridx %in% selectionGrid]
        npoints <- nrow(dataTemp)
        new <- c(new, points[sample(1:length(points),
          size = npoints,
          replace = T, prob = probs
        )])
      }

      NewDensity <- density(new,
        from = min(gridx), to = max(gridx),
        n = length(gridx), bw = "nrd0", adjust = 1
      )
      Mestimates <- NewDensity$y

      resultDensity[, j] <- Mestimates
      resultX[, j] <- new
      if (is.null(weights)) {
        resultW <- NULL
      } else {
        resultW[, j] <- newWeights
      }
    }
    Mestimates <- apply(resultDensity[, -c(1:burnin)], c(1), mean)
  }

  est <- list(
    Mestimates = Mestimates, resultDensity = resultDensity,
    resultX = resultX, xclass = xclass, gridx = gridx, classes = classes,
    burnin = burnin, samples = samples, resultW = resultW
  )
  return(est)
}
