ccfplot <- function(fit, data){
  if(class(fit)[1] == "lm"){
    res <- rep(NA,nrow(data))
    res[as.integer(names(fit$residuals))] <- fit$residuals
  }else if(class(fit)[1] == "marima"){
    res <- fit$residuals[1, ]
  }
  par(mfrow=c(1,3), mar=c(3,3,3,1), mgp=c(2, 0.7,0))
  (i <- as.integer(names(fit$residuals)))
  ccf(fit$residuals[i], D$Pv[i], na.action=na.pass, main="", xlim=c(0,20))
  title(main="CCF(residuals,Pv)", line=0.2)
  ccf(fit$residuals[i], D$Gv[i], na.action=na.pass, main="", xlim=c(0,20))
  title(main="CCF(residuals,Gv)", line=0.2)
  ccf(fit$residuals[i], D$Tdelta[i], na.action=na.pass, main="", xlim=c(0,20))
  title(main="CCF(residuals,Tdelta)", line=0.2)
}
