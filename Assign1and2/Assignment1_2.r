
# LINEAR TREND MODEL

# Yt = th1 + th2 * xt + et

# Assumes residuals et are normally distributed. Mutually independent/uncorrelated and random 
# normal distributed with mean 0 and variance sigma^2 (independent and identically distributed i.i.d.).

# FOR UNWEIGHTED THE COVARIANCE MATRIX IS IDENTITY


# 2.2 ESTIMATES AND STANDARD ERROS. MEAN VS POINTS PLOT

X <- cbind(1,x)

th <- solve(t(X) %*% X) %*% t(X) %*% y

var <- t(y - X %*% th)%*%(y - X %*% th)/(N-2)
var = var[1,1]

estvar = var * solve(t(X) %*% X)

# print(th)
# print(var)
# print(sqrt(estvar[1,1])) # Intercept
# print(sqrt(estvar[2,2])) # Slope

png(file="2_mean_and_points.png")
plot(x, y)
lines(x, X %*% th)
dev.off()

# 2.3 FORECAST FOR NEXT YEAR.

xp <- rep(0,12)

for (j in 1:12) {
    xp[j] = 2024 + (j-1) * 1/12
}

Xp <- cbind(1,xp)

yp <- Xp %*% th

# print(yp)


# 2.4 FITTED MODEL AND ALL DATA

xn <- rep(0, length(D$total))

for (j in 1:length(D$total)) {
    xn[j] = 2018 + (j-1) * 1/12
}

Xn <- cbind(1,xn)

png(file="3_prediction_and_data.png")
plot(xn, D$total)
lines(xn, Xn %*% th)
dev.off()

# 2.6 RESIDUALS

et <- y - X %*% th

png(file="4_residuals.png", width = 600, height = 400)
par(mfrow = c(1,2))
plot(x,et, xlab = "time variable x", ylab = "Residuals")
hist(et,
xlab = "Residuals",
xlim = c(-0.06,0.06),
freq=FALSE
)
xfit<-seq(min(et),max(et),length=40)
yfit<-dnorm(xfit, mean = 0, sd=sd(et))
lines(xfit,yfit)
dev.off()

# MODEL ASSUMPTIONS NOT FULLFILLED










