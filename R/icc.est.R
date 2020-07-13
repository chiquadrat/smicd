# Internal documentation -------------------------------------------------------
#
# ICC

icc.est <- function(model) {
  u <- as.data.frame(VarCorr(model))$sdcor[1]
  e <- sigma(model)
  icc <- u / (u + e)
  return(icc)
}
