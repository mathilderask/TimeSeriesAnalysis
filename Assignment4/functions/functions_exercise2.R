#--------------- 1-dimensional Kalman filter ---------------#

kf_logLik_dt1d <- function(par, df) {
  # par: vector of parameters
  # df: data frame with observations and inputs as columns (Y, Ta, S, I)
  # par: Could be on the form c(A, B1, B2, B3, Q, C, sigma2)
    # par: Could be on the form c(A11, A12, A21, A22, B11, B12,B13 , B21, B22,B23, Q11, Q12, Q22,C1,C2,sigma2)
  A   <- par[1] # transition matrix
  B   <- c(par[5],par[6],par[7]) # input matrix
  Sigma1lt <- par[11] # lower-triangle of system covariance matrix
  Sigma1   <- Sigma1lt^2 # THAT IS!!! The system covariance matrix is given by Qlt %*% t(Qlt) (and is this symmetric positive definite)
  C   <- par[14] # observation matrix
  Sigma2 <- par[16] # observation noise covariance matrix
  X0  <- 20 # initial state

  # Variables
  obs_cols <- c("Y") # observation column names
  input_cols <- c("Ta","S","I") # input column names

  # pull out data
  Y  <- as.matrix(df[, obs_cols])     # m×T
  U  <- as.matrix(df[, input_cols])   # p×T
  Tn <- nrow(df)

  # init
  n  <- 1
  x_est <- X0
  P_est  <- 10                 # X0 prior covariance
  logLik <- 0
  yy <- c(rep(0,Tn))
  for (t in 1:Tn) {
    # prediction step
    x_pred <- A * x_est + B %*% U[t,] # write the prediction step
    P_pred <- A^2 * P_est + Sigma1 # write the prediction step (Sigma_xx)

    # innovation step
    y_pred  <- C * x_pred # predicted observation
    S_t     <- C^2 * P_pred + Sigma2 # predicted observation covariance (Sigma_yy)
    innov   <- Y[t] - y_pred # innovation (one-step prediction error)

    # log-likelihood contribution
    logLik <- logLik - 0.5 * (log(S_t) + innov^2 / S_t)

    # update step
    K_t    <- P_pred * C / S_t # Kalman gain
    x_est  <- x_pred + K_t * innov # reconstructed state
    P_est  <- P_pred - K_t * C * P_pred # reconstructed covariance
    yy[t] <- y_pred
  }
  
  #as.numeric(logLik)
  return(yy)
}


# Optimizer wrapper
estimate_dt1d <- function(start_par, df, lower=NULL, upper=NULL) {
  negLL <- function(par){ -kf_logLik_dt1d(par, df) }
  optim(
    par    = start_par, fn = negLL,
    method = "L-BFGS-B",
    lower  = lower, upper = upper,
    control= list(maxit=1000, trace=1)
  )
}


#--------------- 2-dimensional Kalman filter ---------------#


kf_logLik_dt <- function(par, df) {
  # par: vector of parameters
  # df: data frame with observations and inputs as columns (Y, Ta, S, I)
  # par: Could be on the form c(A11, A12, A21, A22, B11, B12,B13 , B21, B22,B23, Q11, Q12, Q22,C1,C2,sigma2)
  A <- matrix(par[1:4], nrow = 2, ncol = 2, byrow = TRUE)
  B <- matrix(par[5:10], nrow = 2, ncol = 3, byrow = TRUE) # 2x3 for 3 inputs
  Qlt <- matrix(c(par[11], 0, par[12], par[13]), nrow = 2, byrow = TRUE) # 2x2 lower triangular
  Q <- Qlt %*% t(Qlt)
  C <- matrix(par[14:15], nrow = 1, ncol = 2) # 1x2 for observation
  Sigma2 <- par[16] # observation noise variance (scalar)

  X0 <- c(20, 20)
  P0 <- diag(10, 2)
  
  # Extract data
  Y <- as.matrix(df$Y)       # Tx1
  U <- as.matrix(df[, c("Ta", "S", "I")]) # Tx3
  Tn <- nrow(df)
  
  # Pre-allocate for states
  x_est <- matrix(0, nrow = Tn + 1, ncol = 2)
  P_est <- array(0, c(2, 2, Tn + 1))
  x_est[1, ] <- X0
  P_est[, , 1] <- P0
  logLik <- 0
  yy <- c(rep(0,Tn))
  for (t in 1:Tn) {
    # Prediction
    x_pred <- as.vector(A %*% x_est[t, ] + B %*% U[t, ])
    P_pred <- A %*% P_est[,,t] %*% t(A) + Q

    # Observation prediction
    y_pred <- as.numeric(C %*% x_pred)
    S_t <- C %*% P_pred %*% t(C) + Sigma2
    innov <- Y[t] - y_pred

    # Log-likelihood
    logLik <- logLik - 0.5 * (log(S_t) + (innov^2)/S_t)

    # Update
    K_t <- P_pred %*% t(C) / as.numeric(S_t) # (2x2) * (2x1) / (1x1) = (2x1)
    x_est[t + 1, ] <- x_pred + K_t * as.numeric(innov)
    P_est[,,t + 1] <- P_pred - K_t %*% C %*% P_pred
    yy[t] <- y_pred
  }
  
  #as.numeric(logLik)
  return(x_est)
}

estimate_dt <- function(start_par, df, lower=NULL, upper=NULL) {
  negLL <- function(par){ -kf_logLik_dt(par, df) }
  optim(
    par    = start_par, fn = negLL,
    method = "L-BFGS-B",
    lower  = lower, upper = upper,
    control= list(maxit=1000, trace=1)
  )
}
