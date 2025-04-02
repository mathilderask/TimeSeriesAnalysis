# See models extrapolated in report / on paper

# Arima defines ar part with opposite signs on coefficients.

# plot function
plotit <- function(x,i) {
    layout(rbind(1,2:3))
  par(mar=c(3,3,1,1), mgp=c(2, 0.7,0))
  plot(x[1:120],type = "l", ylab="X", xaxp=c(0,120,10))
  acf(x, lag.max=50, lwd=2)
  pacf(x, lag.max=50, lwd=2)
}

# set n
n <- 240

# SARIMA (1,0,0)x(0,0,0)12 with phi1 = 0.6

ph <- c(-0.6)
png(file="2_SARIMA1.png")
plotit(arima.sim(model=list(ar = ph), n=n))
dev.off()

# SARIMA (0,0,0)x(1,0,0)12 with Phi1 = -0.9

ph <- c(rep(0,11),0.9)
png(file="2_SARIMA2.png")
plotit(arima.sim(model=list(ar = ph), n=n))
dev.off()

# SARIMA (1,0,0)x(0,0,1)12 with phi1 = 0.9 and Th1=-0.7

ph <- c(-0.9)
th <- c(rep(0,11),-0.7)
png(file="2_SARIMA3.png")
plotit(arima.sim(model=list(ar = ph, ma = th), n=n))
dev.off()

# SARIMA (1,0,0)x(1,0,0)12 with phi1 = -0.6 and Phi1 = -0.8

ph <- c(0.6,rep(0,10),0.8,-0.48)
png(file="2_SARIMA4.png")
plotit(arima.sim(model=list(ar = ph), n=n))
dev.off()
 
# SARIMA (0,0,1)x(0,0,1)12 with th1 = 0.4 and Th1 = -0.8

th <- c(0.4, rep(0,10), -0.8, -0.32)
png(file="2_SARIMA5.png")
plotit(arima.sim(model=list(ma = th), n=n))
dev.off()

# SARIMA (0,0,1)x(1,0,0)12 with th1 = -0.4 and Phi1 = 0.7

ph <- c(rep(0,11), -0.7)
th <- c(-0.4)
png(file="2_SARIMA6.png")
plotit(arima.sim(model=list(ar = ph, ma = th), n=n))
dev.off()