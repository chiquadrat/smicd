#' @title Prints a sem Object
#'
#' @description Basic information of a sem object is printed
#' @param x an object of class \code{"sem"}.
#' @param ... optional arguments passed to generic function
#' @seealso \code{\link{semObject}}, \code{\link{semLm}}, \code{\link{semLme}}
#' @export
#' @return NULL

print.sem <- function(x, ...) {
  coefficients <- NULL
  cat("Call:\n")
  print(x$call)
  cat("\n")
  cat("Coefficients:\n")
  if (all(inherits(x, which = TRUE, c("sem", "lm")))) {
    if (is.null(x$se)) {
      coefficients <- cbind(x$coef)
      dimnames(coefficients) <- list(
        names(x$coef),
        c("Estimate")
      )
      print(coefficients)
    } else if (!is.null(x$se)) {
      coefficients <- cbind(x$coef, x$se, x$ci)
      dimnames(coefficients) <- list(
        names(x$coef),
        c(
          "Estimate", "Std. Error",
          "Lower", "Upper"
        )
      )
      print(coefficients)
    }
  } else if (all(inherits(x, which = TRUE, c("sem", "lme")))) {
    if (is.null(x$se)) {
      coefficients <- cbind(x$coef)
      dimnames(coefficients) <- list(
        names(x$coef),
        c("Estimate")
      )
      print(coefficients)
    } else if (!is.null(x$se)) {
      coefficients <- cbind(x$coef, x$se, x$ci)
      dimnames(coefficients) <- list(
        names(x$coef),
        c(
          "Estimate", "Std. Error",
          "Lower", "Upper"
        )
      )
      print(coefficients)
    }
  }
}
