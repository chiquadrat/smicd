#' @title Prints a kdeAlgo Object
#'
#' @description Basic information of a kdeAlgo object is printed.
#' @param x an object of class \code{"kdeAlgo"}
#' @param ... optional arguments passed to generic function
#' @seealso \code{\link{kdeAlgoObject}}, \code{\link{kdeAlgo}}
#' @export
#' @return NULL

print.kdeAlgo <- function(x,...){
  cat("Value:\n")
  print(round(x$Point_estimate, digits = 3))
  cat("\n")
  if (!is.null(x$Standard_Error)) {
    cat("Standard error:\n")
    print(round(x$Standard_Error, digits = 3))
  }
}
