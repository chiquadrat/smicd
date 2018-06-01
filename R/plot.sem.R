#' @title Plot Diagnostics for sem Objects
#'
#' @description Available are convergence plots for the estimated
#' fixed effects model parameters and the residual variance of
#' the linear or mixed linear regression model. If the Box-Cox transformation
#' is used
#' for the transformation of the dependent variable, a convergence plot
#' of the transformation parameter lambda is also available.
#' In each of the convergence plots, the estimated parameter is plotted against
#' the iteration step of the SEM-algorithm.
#' A vertical line indicates the end of the burn-in period. Furthermore, the
#' estimated density of the simulated dependent variable of the last iteration
#' step
#' is plotted with a histogramm of the interval censored true dependent variable
#' in the back.
#' @param x an object of type "sem", typical result of \code{\link{semLm}} or
#' \code{\link{semLme}}.
#' @param ... optional arguments passed to generic function.
#' @return Convergence and density plots.
#' @references
#' Walter, P., Gross, M., Schmid, T. and Tzavidis, N. (2017). Estimation of
#' Linear and Non-Linear Indicators
#' using Interval Censored Income Data. School of Business & Economics,
#' Discussion
#' Paper.
#' @seealso \code{\link{semObject}}, \code{\link{semLm}}, \code{\link{semLme}}
#' @export
#' @examples
#' data <- Exam
#' classes <- c(1,1.5,2.5,3.5,4.5,5.5,6.5,7.7,8.5, Inf)
#' data$examsc.class<- cut(data$examsc, classes)
#' model <- semLm(formula = examsc.class ~ standLRT + schavg, data = data,
#' classes = classes, burnin = 4, samples = 10)
#' plot(model)
plot.sem <- function(x,...) {

  if (!is.null(x$conv.sigmau)){
    plot(x$conv.sigmau, main = "Convergence", xlab = "Iteration Step", ylab = expression(sigma[u]))
    abline(v=x$burnin)
    cat("Press [enter] to continue")
    line <- readline()
  }

  plot(x$conv.sigmae, main = "Convergence",  xlab = "Iteration Step", ylab = expression(sigma[e]))
  abline(v=x$burnin)

  cat("Press [enter] to continue")
  line <- readline()

  for(i in 1:(dim(x$conv.coef)[1])) {
    lbs <- parse(text=(paste0("beta[",i-1,"]")))
    plot(x$conv.coef[i,], main = "Convergence", xlab = "Iteration Step", ylab = lbs)
    abline(v=x$burnin)
    cat("Press [enter] to continue")
    line <- readline()
    }



  if (!is.null(x$conv.lambda)){
    plot(x$conv.lambda, main = "Convergence", xlab = "Iteration Step", ylab = expression(lambda))
    abline(v=x$b.lambda)

    cat("Press [enter] to continue")
    line <- readline()
  }

  if (x$transformation == "None"){
    if (x$classes[1]==-Inf) {x$classes[1] <- min(x$pseudo.y[,ncol(x$pseudo.y)])-1}
    if (x$classes[length(x$classes)]==Inf) {x$classes[length(x$classes)] <- max(x$pseudo.y[,ncol(x$pseudo.y)])+1}

    maxhist <- max(hist(x$pseudo.y[,ncol(x$pseudo.y)], breaks = x$classes,
                        main = "Density from last iteration step", xlab = "Pseudo Y")$density)
    maxden <- max(density(x$pseudo.y[,ncol(x$pseudo.y)])$y)
    maxylim <- max(maxhist, maxden)

    hist(x$pseudo.y[,ncol(x$pseudo.y)], breaks = x$classes, main = "Density from last iteration step",
         xlab = "Pseudo Y", ylim = c(0,maxylim))
    lines(density(x$pseudo.y[,ncol(x$pseudo.y)]),  col="purple",lwd=2)}

  if (x$transformation == "log"){
    pseudo.y <- exp(x$pseudo.y[,ncol(x$pseudo.y)])
    if (x$classes[1]==-Inf) {x$classes[1] <- min(pseudo.y)-1}
    if (x$classes[length(x$classes)]==Inf) {x$classes[length(x$classes)] <- max(pseudo.y)+1}
    classes <- x$classes

    maxhist <- max(hist(pseudo.y, breaks = classes, main = "Density from last iteration step",
                        xlab = "Pseudo Y on original scale")$density)
    maxden <- max(density(pseudo.y)$y)
    maxylim <- max(maxhist, maxden)

    hist(pseudo.y, breaks = classes, main = "Density from last iteration step",
         xlab = "Pseudo Y on original scale", ylim = c(0,maxylim))
    lines(density(pseudo.y),  col="purple",lwd=2)}

  if (x$transformation == "bc"){
    pseudo.y <- boxcox.lme.est(dat=x$pseudo.y[,ncol(x$pseudo.y)],
                            lambda = x$lambda, inverse = T)[[1]]
    if (x$classes[1]==-Inf) {x$classes[1] <- min(pseudo.y)-1}
    if (x$classes[length(x$classes)]==Inf) {x$classes[length(x$classes)] <- max(pseudo.y)+1}
    classes <- x$classes

    maxhist <- max(hist(pseudo.y, breaks = classes, main = "Density from last iteration step",
                        xlab = "Pseudo Y on original scale")$density)
    maxden <- max(density(pseudo.y)$y)
    maxylim <- max(maxhist, maxden)

    hist(pseudo.y, breaks = classes, main = "Density from last iteration step",
     xlab = "Pseudo Y on original scale", ylim = c(0,maxylim))
     lines(density(pseudo.y),  col="purple",lwd=2)}


}
