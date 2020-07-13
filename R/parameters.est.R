# Internal documentation -------------------------------------------------------

# The function estimates the final parameters

parameters.est <- function(parameter, burnin) {
  for (i in 1:length(parameter)) {
    if (is.null(dim(parameter[[i]]))) {
      parameter[[i]] <- mean(parameter[[i]][-c(1:burnin)])
    } else {
      parameter[[i]] <- apply(parameter[[i]][, -c(1:burnin)], 1, mean)
    }
  }
  return(parameter)
}
