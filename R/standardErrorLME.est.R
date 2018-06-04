# Internal documentation -------------------------------------------------------
#
# Estimate standard errors

standardErrorLME.est <- function(formula, data, classes, burnin, samples, trafo,
                                adjust, b, coef, sigmae, VaCov, nameRI, nameRS,
                                regmodell, lambda){

    # Fixed effect prediction to find mu
    boot_data <- data
    fixedformula <- as.formula(lme4::nobars(formula(regmodell))[-2])
    fixedeffect <- model.matrix(fixedformula,data = boot_data)
    mu <- as.matrix(fixedeffect) %*% coef

    # Get names from FE
    name.fe <- names(coef)
    # Get name from random intercept
    name.RI <- nameRI
    # Get name from random  slope
    name.RS <- nameRS
    # Get name of dependent variable from formula
    name.dep <- all.vars(formula)[1]

    coef <- NULL
    pb <- txtProgressBar(min = 1, max = b, style = 3)
    print("Bootstrapping...")
    for (i in 1:b){
      p.ranef <- NULL
      for (j in 1 :length(unique(boot_data[,tail(all.vars(formula), n=1)]))){
        p.ranef <- rbind(p.ranef, apply(rmvnorm(n = 1, mean = rep(0, nrow(VaCov))
                                                ,sigma = VaCov), 2,
                                        function(x) {rep(x,table(boot_data[,tail(all.vars(formula), n=1)])[j])}))
      }

    if(is.na(nameRS)) {boot_data$ynew <- mu + p.ranef[,1] + rnorm(nrow(boot_data), 0 , sigmae)}
    if(!is.na(nameRS)) {boot_data$ynew <- mu + p.ranef[,1] + p.ranef[,2]*boot_data[,name.RS]+
        rnorm(nrow(boot_data), 0 , sigmae)}

      # Hier die Daten rücktransformieren
      if (trafo=="log"){boot_data$ynew <- exp(boot_data$ynew)}
      if (trafo=="bc") {rueck <- boxcox.lme.est(dat=boot_data$ynew, lambda = lambda, inverse = T)
                        boot_data$ynew <- rueck[[1]]}

      ##
      boot_data$ynew[boot_data$ynew<classes[1]] <- classes[1]+.Machine$double.eps
      boot_data$ynew[boot_data$ynew>classes[length(classes)]] <-
          classes[length(classes)]+.Machine$double.eps


      assign(paste0("boot_data$",name.dep), cut(boot_data$ynew, classes))
      #boot_data$examsc.class <- cut(boot_data$ynew, classes) # Stimmt das mit examsc.

      capture.output(SEM <- semLme(formula = formula, data = boot_data,
                     classes = classes, burnin = burnin, samples = samples,
                     trafo = trafo, adjust = adjust, bootstrap.se = FALSE))

      coef <- rbind(coef, SEM$coef)
      setTxtProgressBar(pb, i)
    }

    se.boot <- apply(coef,2,function(x){sqrt(mean((x-mean(x))^2))})

    ci <- apply(coef,2,function(x){
      x <- sort(x)
      ci.lower <- ifelse(round(length(x)*.025)<1,x[1],x[round(length(x)*.025)])
      ci.upper <- ifelse(round(length(x)*.975)>length(x),x[length(x)],
                         x[round(length(x)*.975)])
      return(list(ci.low = ci.lower, ci.up = ci.upper))
    })

    ci.final <- NULL
    for (i in 1:length(ci)){
      ci.final <- rbind(ci.final, c(ci[[i]]$ci.low, ci[[i]]$ci.up))
    }

    est <- list(se = se.boot, ci = ci.final)
    return(est)
}
