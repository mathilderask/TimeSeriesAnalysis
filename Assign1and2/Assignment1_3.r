
# WLS LOCAL LINEAR TREND MODEL

lambda = 0.9

weight <- rep(0, N)

for (j in 0:N-1) {
    weight[N-j] = lambda^(j)
}



# EQ 2.31 for covariance matrix

# th = (t(x) sigma^-1 x)^-1 t(x) sigma^-1 y


