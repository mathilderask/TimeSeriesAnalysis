ARX <- function(output, inputs, lags){
  paste0(output," ~ 0 + ", paste0(c(sapply(c(output,inputs), paste0, ".l",lags)), collapse=" + "))
}
