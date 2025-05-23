oiaKalmanFilter <- function(
  y,             # Vector of observations y_t
  theta,       # Model parameters for X_{t+1} = b + a*X_t + c*e_t
  R,             # Measurement noise variance
  x_prior ,   # Initial prior mean for X_0
  P_prior   # Initial prior variance for X_0
) {
  a <- theta[2]
  b <- theta[1]
  sigma1 <- theta[3] # Process noise variance! covariance of error c*e_t?!
  N <- length(y)
  x_pred  <- numeric(N)  # Predicted means
  P_pred  <- numeric(N)  # Predicted variances
  x_filt  <- numeric(N)  # Filtered means
  P_filt  <- numeric(N)  # Filtered variances
  innovation     <- numeric(N)  # Pre-fit residuals: y[t] - x_pred[t]
  innovation_var <- numeric(N)  # Innovation covariance: P_pred[t] + R
  
  for (t in seq_len(N)) {
    # the prediction step
    if (t == 1) {
      x_pred[t] <- a*x_prior + b        # the mean prediction using the prior
      P_pred[t] <- a*P_prior*a + sigma1 # the variance prediction using the prior #s
    } else {
      x_pred[t] <- a*x_filt[t-1] + b   # the mean prediction using the previous filtered estimate
      P_pred[t] <- a*P_filt[t-1]*a + sigma1   # the variance prediction using the previous filtered estimate
    }
    
    # the update step
    innovation[t] <- y[t] - x_pred[t] # the prediction error
    #print("y[t]")
    #print(y[t])
    innovation_var[t] <- P_pred[t]+ R # the prediction error variance # this is sigma^yy_t+t|t
    K_t <- P_pred[t]/innovation_var[t]# the Kalman gain 
    #print(K_t)

    x_filt[t] <- x_pred[t] + K_t*innovation[t] # the filtered estimate
    P_filt[t] <- P_pred[t] - K_t*P_pred[t]   # the filtered estimate variance # sigma^xx_t|t
  }
  
  return(list(
    x_pred = x_pred,
    P_pred = P_pred,
    x_filt = x_filt,
    P_filt = P_filt,
    innovation = innovation,
    innovation_var = innovation_var
  ))
}
