
# WLS LOCAL LINEAR TREND MODEL

# 3.1 VARIANCE-COVARIANCE MATRIX IS 1/w_t in the diagonal and 0 elsewhere.


# 3.2 WEIGHTS VS TIME PLOT
# EQ 2.31 for covariance matrix
# th = (t(x) sigma^-1 x)^-1 t(x) sigma^-1 y

lambda = 0.9

weight <- rep(0, N)

for (j in 1:N) {
    weight[N-(j-1)] = lambda^(j-1)
}

png(file="5_weigth_v_time.png")
barplot(weight, xlab="time from 1 to N", ylab="weights")
dev.off()

# 3.3 SUM OF WEIGHTS

ws = sqrt(t(weight)) %*% sqrt(weight)
#print(ws)

# WHAT WOULD IT BE IN OLS MODEL? N?


# 3.4 PARAMETER ESTIMATES WITH LAMBDA = 0.9

Sigma <- diag(1/weight)
Sigmainv <- solve(Sigma)

thw <- solve(t(X) %*% Sigmainv %*% X) %*% t(X) %*% Sigmainv %*% y

print(thw[1]) # -52.48286
print(thw[2]) # 0.0275299

ypw <- Xtest %*% thw
varw <- t(y - X %*% thw) %*% Sigmainv %*% (y - X %*% thw)/(N-2)
varw = varw[1,1]

ypwp <- ypw + qt(1-0.05/2,N-2) * sqrt(diag(varw * (1 + Xtest %*% solve(t(X) %*%  X) %*% t(Xtest))))
ypwm <- ypw - qt(1-0.05/2,N-2) * sqrt(diag(varw * (1 + Xtest %*% solve(t(X) %*%  X) %*% t(Xtest))))

# 3.5 PREDICTIONS 

png(file="6_weightedmodel.png")
plot(x, y, xlim=c(min(x),max(xtest)), ylim=c(min(y),max(ypp)), xlab = "Time x [year]", ylab = "Vehicles registered [million]")
par(new=TRUE)
plot(xtest, Dtest$total, xlim=c(min(x),max(xtest)), ylim=c(min(y),max(ypp)),col = "red", xlab = "Time x [year]", ylab = "Vehicles registered [million]")
# WEIGHTED PREDICTION
lines(xn,Xn %*% thw,col="blue", lwd=2, lty=1)
lines(xtest,ypwp,col = "blue", lwd=2, lty=2)
lines(xtest,ypwm,col = "blue", lwd=2, lty=2)
# UNWEIGHTED PREDICTION
lines(xn, Xn %*% th, lwd=2, lty=1)
lines(xtest, ypm, lwd=2, lty=3)
lines(xtest, ypp, lwd=2, lty=3)
dev.off()

# 3.6 COMPARING PREDICTIONS FOR DIFFERENT LAMBDA

png(file="7_weightedmodel.png")

plot(xtest, Dtest$total, ylim=c(min(Dtest$total),max(yp)), xlab = "Time x [year]", ylab = "Vehicles registered [million]")

i = 1
color <- c("black","black","blue","red","green")

for (l in c(0.99,0.9,0.8,0.7)) {

    i = i+1
    for (j in 1:N) {
        weight[N-(j-1)] = l^(j-1)
    }

    Sigma <- diag(1/weight)
    Sigmainv <- solve(Sigma)

    thw <- solve(t(X) %*% Sigmainv %*% X) %*% t(X) %*% Sigmainv %*% y

    lines(xn,Xn %*% thw, lwd=3, lty=i, col=color[i])
}

legend(2024.5, 3.3, c("data","lambda=0.99","lambda=0.9","lambda=0.8","lambda=0.7"), lwd=rep(3,5), lty=0:4, col=color)

dev.off()


