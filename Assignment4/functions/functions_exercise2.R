kf_logLik_dt <- function(par, df) {
  # par: vector of parameters
  # df: data frame with observations and inputs as columns (Y, Ta, S, I)
  # par: Could be on the form c(A11, A12, A21, A22, B11, B12,B13 , B21, B22,B23, Q11, Q12, Q22,C1,C2,sigma2,X01,X02)
  A   <- t(matrix(c(par[1:4]),nrow = 2,ncol = 2)) # transition matrix
  B   <- t(matrix(par[5:8],nrow = 2,ncol = 2)) # input matrix
  Sigma1lt <- t(matrix(par[9],0,par[10:11],nrow = 2, ncol = 2)) # lower-triangle of system covariance matrix
  Sigma1   <- Sigma1lt %*% t(Sigma1lt) # THAT IS!!! The system covariance matrix is given by Qlt %*% t(Qlt) (and is this symmetric positive definite)
  C   <- matrix(C1,C2) # observation matrix
  Sigma2 <- par[16] # observation noise covariance matrix
  X0  <- matrix(X0) # initial state

  # Variables
  obs_cols <- c("Y") # observation column names
  input_cols <- c("Ta","S","I") # input column names

  # pull out data
  Y  <- as.matrix(df[, obs_cols])     # m×T
  U  <- as.matrix(df[, input_cols])   # p×T
  Tn <- nrow(df)

  # init
  n      <- nrow(A)
  x_est  <- matrix(Y[1,], n, 1)            # start state from first obs
  x_est[1] <- X0 
  P_est  <- diag(1e1, n)                   # X0 prior covariance
  logLik <- 0

  for (t in 1:Tn) {
    # prediction step
    x_pred <- A %*% x_est[t] + B %*% U[t] # write the prediction step
    P_pred <- A %*% P_est %*% t(A) + Sigma1 # write the prediction step (Sigma_xx)

    # innovation step
    y_pred  <- C %*% x_pred # predicted observation
    S_t     <- C %*% P_pred %*% t(C) + Sigma2 # predicted observation covariance (Sigma_yy)
    innov   <- Y[t] - y_pred # innovation (one-step prediction error)

    # log-likelihood contribution
    logLik <- logLik - 0.5*(sum(log(2*pi*S_t)) + t(innov) %*% solve(S_t, innov))

    # update step
    K_t    <- P_pred %*% t(C) %*% solve(S_t)# Kalman gain
    x_est[t+1]  <- x_pred + K_t * innov # reconstructed state
    P_est  <- P_pred - K_t %*% C %*% P_pred # reconstructed covariance
  }

  as.numeric(logLik)
}

# Optimizer wrapper
estimate_dt <- function(start_par, df, lower=NULL, upper=NULL) {
  negLL <- function(par){ -kf_logLik_dt(par, df) }
  optim(
    par    = start_par, fn = negLL,
    method = "L-BFGS-B",
    lower  = lower, upper = upper,
    control= list(maxit=1000, trace=1)
  )
}




