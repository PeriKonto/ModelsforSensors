library("pracma")

# A matrix
A_k_minus_1 <- matrix(c(1.0, 0, 0,
                        0, 1.0, 0,
                        0, 0, 1.0), nrow = 3, byrow = TRUE)

# Noise applied to the forward kinematics
process_noise_v_k_minus_1 <- c(0.01, 0.01, 0.003)

# State model noise covariance matrix Q_k
Q_k <- matrix(c(1.0, 0, 0,
                0, 1.0, 0,
                0, 0, 1.0), nrow = 3, byrow = TRUE)

# Measurement matrix H_k
H_k <- matrix(c(1.0, 0, 0,
                0, 1.0, 0,
                0, 0, 1.0), nrow = 3, byrow = TRUE)

# Sensor measurement noise covariance matrix R_k
R_k <- matrix(c(1.0, 0, 0,
                0, 1.0, 0,
                0, 0, 1.0), nrow = 3, byrow = TRUE)

# Sensor noise
sensor_noise_w_k <- c(0.07, 0.07, 0.04)

getB <- function(yaw, deltak) {
  B <- matrix(c(cos(yaw) * deltak, 0,
                sin(yaw) * deltak, 0,
                0, deltak), nrow = 3, byrow = TRUE)
  return(B)
}

ekf <- function(z_k_observation_vector, state_estimate_k_minus_1, 
                control_vector_k_minus_1, P_k_minus_1, dk) {
  
  # Predict
  state_estimate_k <- A_k_minus_1 %*% state_estimate_k_minus_1 + 
    getB(state_estimate_k_minus_1[3], dk) %*% control_vector_k_minus_1 +
    process_noise_v_k_minus_1
  
  print(paste("State Estimate Before EKF:", state_estimate_k))
  
  P_k <- A_k_minus_1 %*% P_k_minus_1 %*% t(A_k_minus_1) + Q_k
  
  # Update
  measurement_residual_y_k <- z_k_observation_vector - 
    (H_k %*% state_estimate_k + sensor_noise_w_k)
  
  print(paste("Observation:", z_k_observation_vector))
  
  S_k <- H_k %*% P_k %*% t(H_k) + R_k
  K_k <- P_k %*% t(H_k) %*% pinv(S_k)
  state_estimate_k <- state_estimate_k + K_k %*% measurement_residual_y_k
  P_k <- P_k - K_k %*% H_k %*% P_k
  
  print(paste("State Estimate After EKF:", state_estimate_k))
  
  return(list(state_estimate_k, P_k))
}

main <- function() {
  k <- 1
  dk <- 1
  
  z_k <- matrix(c(4.721, 0.143, 0.006,
                  9.353, 0.284, 0.007,
                  14.773, 0.422, 0.009,
                  18.246, 0.555, 0.011,
                  22.609, 0.715, 0.012), nrow = 5, byrow = TRUE)
  
  state_estimate_k_minus_1 <- c(0.0, 0.0, 0.0)
  control_vector_k_minus_1 <- c(4.5, 0.0)
  P_k_minus_1 <- matrix(c(0.1, 0, 0,
                          0, 0.1, 0,
                          0, 0, 0.1), nrow = 3, byrow = TRUE)
  
  for (i in 1:5) {
    cat(paste("Timestep k =", k, "\n"))
    result <- ekf(z_k[i,], state_estimate_k_minus_1, control_vector_k_minus_1, P_k_minus_1, dk)
    state_estimate_k_minus_1 <- result[[1]]
    P_k_minus_1 <- result[[2]]
    cat("\n")
    k <- k + 1
  }
}

main()
