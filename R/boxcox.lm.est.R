# Internal documentation -------------------------------------------------------

# Box-Cox transformation for linear regression

boxcox.lm.est <- function(dat, formula,lambdarange = c(-2, 2), tr = FALSE, ...) {

  model.frame <- NULL
  model.response <- NULL
  model.matrix <- NULL
  optimize <- NULL
  lm <- NULL


  model_frame <- model.frame(formula = formula, data = dat)
  y <- model.response(model_frame)
  x <- model.matrix(attr(model_frame,"terms"), data = model_frame)

  qr <- qr(x)
  n <- length(y)
  k <- ncol(x)
  yt <- rep(NA, n)
  lglike <- function(lambda, ...) {
    if (abs(lambda) != 0) {
      yt <- (y^lambda - 1)/lambda

    }
    else {
      yt <- log(y)
    }
    zt <- yt/exp((lambda - 1)*mean(log(y)))
    llike <- -n/2 * log((sum(qr.resid(qr, zt)^2))/n)
    llike
  }
  res <-suppressWarnings( optimize(f = function(lambda) lglike(lambda), lambdarange, tol = 0.0001, maximum = TRUE) )
  lambdaoptim <-  res$maximum
  logoptim <-res$objective
  lambdavector <- seq(lambdarange[1], lambdarange[2], 0.01)
  l <- length(lambdavector)
  lambdavector[l + 1]  <- lambdaoptim
  lambdavector <- sort(lambdavector)
  logvector <- sapply(lambdavector, lglike)
  if (abs(lambdaoptim) > 0.05)  {
    yt <- (y^lambdaoptim - 1)/lambdaoptim } else {yt <- log(y) }
  zt <- yt/exp((lambdaoptim - 1)*mean(log(y)))
  suppressWarnings( modelt <- lm(zt ~ ., data.frame(zt, x[, 2:k] )))

  ans <- list()
  if(is.infinite(ans$llike <- logoptim ) & tr !=TRUE)
    stop(("log-likelihood is infinite or not defined for components y and x"))
  ans$lambdahat <- lambdaoptim
  ans$logvector <- logvector
  ans$lambdavector <- lambdavector
  ans$family <- "Box-Cox"
  ans$yt <- yt
  ans$zt <- zt
  ans$modelt <- modelt
  ans
}

