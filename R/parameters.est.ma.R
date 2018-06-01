# Internal documentation -------------------------------------------------------

# The function estimates the final parameters from matrices

parameters.est.ma <- function(parameter, burnin) {
  for (i in 1:length(parameter)) {
    parameter.tmp <- array(unlist(parameter[[i]]), dim = c(nrow(parameter[[i]][[1]]),
                                              ncol(parameter[[i]][[1]]),
                                              length(parameter[[i]])))

    parameter[[i]] <- apply(parameter.tmp, c(1,2), function(x){mean(x[-c(1:burnin)])})}
  return(parameter)
}
