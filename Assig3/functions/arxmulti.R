# Multi-step prediction
arxmulti <- function(istart, fit, X){
  # For making newdata
  nmout <- names(fit$model)[1]
  nmcoef <- names(fit$coefficients)
  lagmax <- max(as.numeric(getse(strsplit(nmcoef[grep(nmout, nmcoef)], ".l"), 2)))
  nmoutcoef <- paste0(nmout,".l",1:lagmax)
  #
  y <- rep(NA,nrow(X))
  y[1:istart] <- X[1:istart, nmout]
  # Go predict
  for(i in (istart+1):nrow(X)){
    newdata <- X[i, nmcoef]
    newdata[ ,nmoutcoef] <- y[(i-1):(i-lagmax)]
    y[i] <- predict(fit, newdata)
  }
  return(y)
}
