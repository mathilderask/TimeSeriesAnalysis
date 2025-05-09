myLogLikFun <- function(theta, y, R, x_prior = 0, P_prior = 10) {
  a <- theta[1]
  b <- theta[2]
  sigma1 <- theta[3]
  
  kf_result <- myKalmanFilter(y,theta = c(a,b,sigma1),R,x_prior = x_prior,P_prior = P_prior) # call the Kalman filter function
  err <- kf_result$innovation       # Innovations
  S <- kf_result$innovation_var     # Innovation covariances
  # Compute log-likelihood contributions from each time step

  logL <- -1/2 * sum(log(S)+err^2/S)
  return(-logL)  # Return negative log-likelihood for minimization
}
