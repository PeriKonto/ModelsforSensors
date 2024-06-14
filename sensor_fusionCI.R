# adopted from matlab in R 
# peri 15/10 
# included the CI, Peri 26/10

library(dlm)
library(ggplot2)

# Function to generate a noisy signal
generate_signal <- function(signal, var) {
  noise <- rnorm(length(signal), mean = 0, sd = sqrt(var))
  
  s <- data.frame(
    signal = signal + noise,
    var = var
  )
  
  return(s)
}

# Function to initialize the Kalman filter
init_kalman <- function(y) {
  X <- matrix(c(y, 0), nrow = 2, byrow = TRUE)
  P <- matrix(c(100, 0, 0, 100), nrow = 2, byrow = TRUE)
  return(list(X = X, P = P))
}

# Function for the prediction step of Kalman filter
prediction <- function(X, P, Q, F) {
  X <- F %*% X
  P <- F %*% P %*% t(F) + Q
  return(list(X = X, P = P))
}

# Function for the update step of Kalman filter
update <- function(X, P, y, R, H) { 
  Inn <- y - H %*% X   # residual between the actual and predicted measurement 
  S <- H %*% P %*% t(H) + R
  K <- P %*% t(H) %*% solve(S)
  
  X <- X + K %*% Inn
  P <- P - K %*% H %*% P
  
  return(list(X = X, P = P))
}


# Function to calculate R^2
calculate_R_squared <- function(estimated_values, true_values) {
  # Calculate the mean of estimated and true values
  mean_estimated <- mean(estimated_values)
  mean_true <- mean(true_values)
  
  # Calculate the numerator and denominator for R^2
  numerator <- sum((estimated_values - mean_estimated) * (true_values - mean_true))
  denominator <- sqrt(sum((estimated_values - mean_estimated)^2) * sum((true_values - mean_true)^2))
  
  # Calculate R^2
  R_squared <- (numerator / denominator)^2
  return(R_squared)
}

# Main function to run the Kalman filter and plot the results
main <- function() {
  dt <- 0.005
  t <- seq(0, 2, by = dt)
  n <- length(t)
  
  # Define transition matrix (F) and process noise covariance matrix (Q)
  F <- matrix(c(1, dt, 0, 1), nrow = 2, byrow = TRUE)
  Q <- matrix(c(0.001, 0, 0, 0.001), nrow = 2, byrow = TRUE)
  # system noise matrix Q, so you can control the smoothness of the fused output
  # Ground truth signal
  signal <- sin(t) + t
  
  # Initialize the Kalman filter
  initial_observation <- sin(t[1]) + t[1]
  kalman_state <- init_kalman(initial_observation)
  
  # Create empty arrays to store results
  X_arr <- matrix(0, n, 2)

  X_confidence_upper <- vector("numeric", length = n)
  X_confidence_lower <- vector("numeric", length = n)
  
  # Observation noise covariances
  s1_var <- 0.04 * rep(1, n)
  s2_var <- 0.06 * (cos(8 * t) + 10 * t)
  s3_var <- 0.04 * (sin(2 * t) + 2)
  s4_var <- 0.07 * rep(1, n)
  
  # Generate noisy signals
  s1 <- generate_signal(signal, s1_var)
  s2 <- generate_signal(signal, s2_var)
  s3 <- generate_signal(signal, s3_var)
  s4 <- generate_signal(signal, s4_var)
  
  #s1$signal[140:150]=0
  # s2$signal[49:150]=0
  # s3$signal[100:250]=0
  # s4$signal[100:250]=0   
  
  # Fusion
  for (i in 1:n) {
    if (i == 1) {
      kalman_state <- init_kalman(s1[i, 1])
      
    } else {
      kalman_state <- prediction(kalman_state$X, kalman_state$P, Q, F)
      kalman_state <- update(kalman_state$X, kalman_state$P, s1[i, 1], s1[i, 2], matrix(c(1, 0), nrow = 1, byrow = TRUE))
      kalman_state <- update(kalman_state$X, kalman_state$P, s2[i, 1], s2[i, 2], matrix(c(1, 0), nrow = 1, byrow = TRUE))
      kalman_state <- update(kalman_state$X, kalman_state$P, s3[i, 1], s3[i, 2], matrix(c(1, 0), nrow = 1, byrow = TRUE))
      kalman_state <- update(kalman_state$X, kalman_state$P, s4[i, 1], s4[i, 2], matrix(c(1, 0), nrow = 1, byrow = TRUE))
    }
    
    X_arr[i, ] <- kalman_state$X

    # Calculate the confidence intervals
    X_confidence_upper[i] <- X_arr[i, 1] + 1.96 * sqrt(kalman_state$P[1, 1])  # 95% confidence interval
    X_confidence_lower[i] <- X_arr[i, 1] - 1.96 * sqrt(kalman_state$P[1, 1])
  }
  
  # Calculate R^2 for each signal
  R_squared_s1 <- calculate_R_squared(s1$signal, signal)
  R_squared_s2 <- calculate_R_squared(s2$signal, signal)
  R_squared_s3 <- calculate_R_squared(s3$signal, signal)
  R_squared_s4 <- calculate_R_squared(s4$signal, signal)
  
  R_squared_fuse <- calculate_R_squared(X_arr[, 1], signal)
  
  # Print R^2 for each signal
  cat("Rt^2 for signal#1:", R_squared_s1, "\n")
  cat("Rt^2 for signal#2:", R_squared_s2, "\n")
  cat("Rt^2 for signal#3:", R_squared_s3, "\n")
  cat("Rt^2 for signal#4:", R_squared_s4, "\n")
  cat("Rt^2 for fused:", R_squared_fuse, "\n")
  

  
  plot(t, signal, type = "l", lwd = 2, col = "black", xlab = "Time", ylab = "Value",
       ylim = c(min(signal) - 2, max(signal) + 3))
  
  lines(t, s1$signal, lty = 2, lwd = 0.5, col = "red")
  lines(t, s2$signal, lty = 2, lwd = 0.5, col = "blue")
  lines(t, s3$signal, lty = 2, lwd = 0.5, col = "green")
  lines(t, s4$signal, lty = 2, lwd = 0.5, col = "purple")
  polygon(c(t, rev(t)), c(X_confidence_upper, rev(X_confidence_lower)), col = "lightblue", border = NA)
  lines(t, X_arr[, 1], lwd = 2, col = "orange")
  
  legend("topleft", legend = c("Ground Truth", "Sensor Input 1", "Sensor Input 2", "Sensor Input 3",
                               "Sensor Input 4", "Fused Output", "Confidence Interval"),
         col = c("black", "red", "blue", "green", "purple", "orange", "lightblue"),
         lty = c(1, 2, 2, 2, 2, 1, 1),
         lwd = c(2, 0.1, 0.1, 0.1, 0.1, 1, 0),
         fill = c(NA, NA, NA, NA, NA, NA, "lightblue",alpha = 0.05))
  
  
  
 # hist(signal - X_arr[, 1],breaks=50, xlab="Ground truth - Signal", ylab="Frequency")
  
#  acf(signal - X_arr[, 1])
  grid()

  }




# Run the main function
main()


