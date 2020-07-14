#' @title Summarizing Linear and Linear Mixed Models estimated with the SEM
#'
#' @description \code{summary} method for class \code{"sem"}.
#' @param object an object of class \code{"sem"}.
#' @param ... additional arguments that are not used in this method.
#' @export
#' @return an object of type "summary.sem" with following
#' components:
#' \item{call}{a list containing an image of the function call that produced the
#'             object.}
#' \item{coefficients}{a table that returns the estimation parameters and the
#' standard errors and confidence intervals in case that the standard erros are
#' estimated.}
#' \item{standard errors}{bootstraped standard errors}
#' \item{confidence intervals}{bootstraped confidence intervals}
#' \item{two R2 measures}{a multiple and adjusted R-squared in case of an
#' object of class \code{"sem","lm"} and a marginal and conditional R-squared in
#' case of an object of class \code{"sem","lme"}}


summary.sem <- function(object, ...) {
  ans <- NULL
  ans$call <- object$call
  ans$nclasses <- object$n.classes
  ans$formula <- object$formula

  if (!is.null(object$lambda)) {
    ans$trafo <- "Box-Cox"
    ans$lambda <- object$lambda
  }


  if (all(inherits(object, which = TRUE, c("sem", "lm")))) {
    ans$multipR <- round(object$r2, 4)
    ans$adjR <- round(object$adj.r2, 4)
    if (is.null(object$se)) {
      ans$coefficients <- cbind(object$coef)
      dimnames(ans$coefficients) <- list(
        names(object$coef),
        c("Estimate")
      )
    } else if (!is.null(object$se)) {
      ans$coefficients <- cbind(object$coef, object$se, object$ci)
      dimnames(ans$coefficients) <- list(
        names(object$coef),
        c("Estimate", "Std. Error", "Lower 95%-level", "Upper 95%-level")
      )
    }
  } else if (all(inherits(object, which = TRUE, c("sem", "lme")))) {
    ans$marginalR2 <- round(object$r2m, 4)
    ans$conditionalR2 <- round(object$r2c, 4)
    if (dim(object$VaCov)[1] == 1) {
      randomIntercept <- strsplit(as.character(object$formula[[3]][3]), "\\|")[[1]][2]
      randomIntercept <- strsplit(randomIntercept, ")")
      randomIntercept <- trimws(randomIntercept, "l")
      ans$random <- data.frame(
        Groups = c(randomIntercept, "Residual"),
        Name = c("(Intercept)", ""),
        Variance = c(
          as.numeric(object$VaCov),
          as.numeric(object$sigmae)^2
        ),
        Std.Dev. = c(
          sqrt(as.numeric(object$VaCov)),
          object$sigmae
        )
      )
      rownames(ans$random) <- c()
    } else if (dim(object$VaCov)[1] == 2) {
      randomIntercept <- strsplit(as.character(object$formula[[3]][3]), "\\|")[[1]][2]
      randomIntercept <- strsplit(randomIntercept, ")")
      randomIntercept <- trimws(randomIntercept, "l")


      ans$random <- data.frame(
        Groups = c(
          randomIntercept,
          rownames(object$VaCov)[2],
          "Residual"
        ),
        Name = c("(Intercept)", "", ""),
        Variance = c(
          as.numeric(object$VaCov[1, 1]),
          as.numeric(object$VaCov[2, 2]),
          as.numeric(object$sigmae)^2
        ),
        Std.Dev. = c(
          sqrt(as.numeric(as.numeric(object$VaCov[1, 1]))),
          sqrt(as.numeric(as.numeric(object$VaCov[2, 2]))),
          object$sigmae
        )
      )

      rownames(ans$random) <- c()
    }
    if (is.null(object$se)) {
      ans$coefficients <- cbind(object$coef)
      dimnames(ans$coefficients) <- list(
        names(object$coef),
        c("Estimate")
      )
    } else if (!is.null(object$se)) {
      ans$coefficients <- cbind(object$coef, object$se, object$ci)
      dimnames(ans$coefficients) <- list(
        names(object$coef),
        c("Estimate", "Std. Error", "Lower 95%-level", "Upper 95%-level")
      )
    }
  }


  class(ans) <- "summary.sem"
  ans
}


#' @title Prints a summary.sem Object
#'
#' @description The elements described in summary.sem are printed.
#' @param x an object of class "summary.sem".
#' @param ... additional arguments that are not used in this method.
#' @export
#' @return NULL


print.summary.sem <- function(x, ...) {
  cat("Call:\n")
  print(x$call)
  cat("\n")
  if (!is.null(x$random)) {
    cat("Random effects:\n")
    print(x$random)
    cat("\n")
  }
  cat("Fixed effects:\n")
  print(x$coefficients)
  cat("\n")
  if (!is.null(x$multipR)) {
    cat("Multiple R-squared: ", x$multipR, "Adjusted R-squared: ", x$adjR)
  } else if (!is.null(x$marginalR2)) {
    cat("Marginal R-squared: ", x$marginalR2, "Conditional R-squared: ", x$conditionalR2)
  }
  cat("\n")
  if (!is.null(x$trafo)) {
    cat("Lambda of the Box-Cox transformation: ", x$lambda)
  }
  cat("\n")
  # cat("\n")
  # cat("Number of intervals:\n")
  cat(paste0(
    "Variable ", x$formula[2], " is divided into ",
    x$nclasses, " intervals.\n"
  ))
}
