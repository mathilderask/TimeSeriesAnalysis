validate <- function(fit){
  if(class(fit)[1] == "lm"){
    i <- as.integer(names(fit$residuals))
    res <- rep(NA,max(i))
    res[i] <- fit$residuals
  }else if(class(fit)[1] == "marima"){
    res <- fit$residuals[1, ]
  }
  layout(rbind(1,2:3))
  par(mar=c(3,3,1,1), mgp=c(2, 0.7,0))
  plot(res, type="b")
  lines(X$Pinner, type="l", xlim=c(0,length(res)), col=2)
  legend("topright", c("Residuals","Power"), lty=1, col=1:2)
  acf(res, na.action=na.pass)
  title(main="ACF(residuals)", line=0.2)
  pacf(res, na.action=na.pass)
  title(main="PACF(residuals)", line=0.2)
}
