rm(list=ls())

library(tidyverse)
library(KFAS)
library(tseries)
library(forecast)
library(pracma)
library(dplyr)
library(xts)
library(dplyr)
library(stats)
library(forecast)
library(zoo)


file_path <- "C:/..../231019Dual_personal_sensor_measurements.rds"
sensor_data <- readRDS(file_path)


tidy_sensor_data <- sensor_data %>%
  pivot_wider(
    names_from = variable,
    values_from = value
  )


custom_colors <- c("red", "blue", "green", "orange", "purple", "brown",   "gray70", "gray40", "gray1")


ggplot(tidy_sensor_data, aes(x = datetime, color = as.factor(number))) +
  geom_line(aes(y = PM10), size=0.8) +
  #geom_point(aes(y = light), size=0.8) + 
  labs(x = "Time Difference (mins)", y = "PM10 ug/m3") +
  ggtitle("Dual personal measurement sensor data") +
  facet_wrap( ~ sensorID ,  ncol = 2, scales = "free")  +
  scale_color_manual(values = custom_colors) +
  scale_y_continuous(breaks = seq(0, 100, by = 20), limits = c(0, 100), name ="PM10 ug/m3")




## sensor fusion

filtered_sensors <- tidy_sensor_data %>%
  filter(sensorID %in% c("MAEPH034", "MAEPH043", "MAEPH028") & number  %in% c("pers2", "stat1") )

#filtered_sensors  <- tidy_sensor_data %>%
#  filter(sensorID %in% c("MAEPH034", "MAEPH043", "MAEPH037") & number  %in% c("pers2", "stat1") )

#filtered_sensors  <- tidy_sensor_data %>%
#  filter(sensorID %in% c("MAEPH034", "MAEPH043", "MAEPH041") & number  %in% c("pers2", "stat3") ) ## no, dont use

#filtered_sensors <- tidy_sensor_data %>%
#  filter(sensorID %in% c("MAEPH037", "MAEPH034", "MAEPH043") & number  %in% c("pers2", "stat1") )

#filtered_sensors <- tidy_sensor_data %>%
#  filter(sensorID %in% c("MAEPH026", "MAEPH034", "MAEPH043") & number  %in% c("pers2", "stat3") )


#ggplot(filtered_sensors, aes(x = datetime, color = as.factor(sensorID))) +
#  geom_line(aes(y = PM10)) +
  # geom_point(aes(y = UVB), size=0.8) + 
#  labs(x = "Time", y = "PM10 ug/m3") +
#  ggtitle("Dual personal measurement sensor data") +
  # facet_wrap( ~ sensorID ,  ncol = 2, scales = "free")  +
#  scale_color_manual(values = custom_colors) +
#  scale_y_continuous(breaks = seq(0, 100, by = 20), limits = c(0, 100))


data_sens1 <- filtered_sensors %>%
  filter(sensorID == "MAEPH034"  & number == "pers2") 
ts_data1 <- xts(data_sens1[, -1], order.by = data_sens1$datetime)
ts_data1 <- as.zoo(ts_data1)
#new_start_time1 <- as.POSIXct("2022-10-06 09:14:37", format = "%Y-%m-%d %H:%M:%S")
#new_start_time1 <- as.POSIXct("2022-10-06 10:02:08", format = "%Y-%m-%d %H:%M:%S")
new_start_time1 <- as.POSIXct("2022-10-06 09:12:08", format = "%Y-%m-%d %H:%M:%S")
time_diff1 <- new_start_time1 - index(ts_data1)[1]
index(ts_data1) <- index(ts_data1) + time_diff1
ts_data1 <- as.xts(ts_data1)

data_sens2 <- filtered_sensors %>%
  filter(sensorID ==  "MAEPH043"  & number == "pers2") 
ts_data2 <- xts(data_sens2[, -1], order.by = data_sens2$datetime)
ts_data2 <- as.zoo(ts_data2)
#new_start_time2 <- as.POSIXct("2022-10-06 09:14:37", format = "%Y-%m-%d %H:%M:%S")
#new_start_time2 <- as.POSIXct("2022-10-06 10:02:37", format = "%Y-%m-%d %H:%M:%S")
new_start_time2 <- as.POSIXct("2022-10-06 09:12:37", format = "%Y-%m-%d %H:%M:%S")
time_diff2 <- new_start_time2 - index(ts_data2)[1]
index(ts_data2) <- index(ts_data2) + time_diff2
ts_data2 <- as.xts(ts_data2)


#data_sens3 <- filtered_sensors %>%
#  filter(sensorID ==  "MAEPH037"  & number == "stat1") 
#data_sens3 <- filtered_sensors %>%
#  filter(sensorID ==  "MAEPH037"  & number == "stat1") 
#data_sens3 <- filtered_sensors %>%
#  filter(sensorID ==  "MAEPH041"  & number == "stat3") 
data_sens3 <- filtered_sensors %>%
  filter(sensorID ==  "MAEPH028"  & number == "stat1") 
#data_sens3 <- filtered_sensors %>%
#  filter(sensorID ==  "MAEPH026"  & number == "stat3") 
ts_data3 <- xts(data_sens3[, -1], order.by = data_sens3$datetime)
#ts_data3 <- as.zoo(ts_data3)
#new_start_time3 <- as.POSIXct("2022-10-06 10:02:18", format = "%Y-%m-%d %H:%M:%S")
#time_diff3 <- new_start_time3 - index(ts_data3)[1]
#index(ts_data3) <- index(ts_data3) + time_diff3
#ts_data3 <- as.xts(ts_data3)




insertTable <- function(existingDF, newTable, r) {
  existingDF <- rbind(existingDF[seq(1, r - 1), ], newTable, existingDF[seq(r, nrow(existingDF)), ])

}



timestamps <- index(ts_data3)
time_diffs <- diff(timestamps)
numeric_values <- as.numeric(time_diffs)
gaps_indices <- which(time_diffs >= 2)

modified_data <- ts_data3

for (i in 1:length(gaps_indices)) {
  
  gap_start <- gaps_indices[i]
  
  # Ensure correct indexing for the timestamp
  start_time <- as.POSIXct(ts_data3[gap_start, 1], format = "%Y-%m-%d %H:%M:%S")
  
  # Create a matrix with the same number of columns as ts_data3
  new_data <- matrix(NA, nrow = numeric_values[gaps_indices[i]], ncol = ncol(ts_data3))
  
  # Generate time values with a 60-second interval
  time_values <- seq(start_time, by = 60, length.out = numeric_values[gaps_indices[i]])
  
  # Create an xts object with new_data and time_values
  new_data <- xts(new_data, order.by = time_values)
  
  # Insert the new_data into ts_data3 at the specified gap_start
  modified_data <- insertTable(modified_data, new_data[2:numeric_values[gaps_indices[i]]], gap_start)
}

# Verify that the modifications resolve the issues
ts_data3 <- as.xts(modified_data)






timestamps <- index(ts_data1)
time_diffs <- diff(timestamps)
numeric_values <- as.numeric(time_diffs)
gaps_indices <- which(time_diffs >= 2)

modified_data <- ts_data1

for (i in 1:length(gaps_indices)) {
  
  gap_start <- gaps_indices[i]
  
  # Ensure correct indexing for the timestamp
  start_time <- as.POSIXct(ts_data1[gap_start, 1], format = "%Y-%m-%d %H:%M:%S")
  
  # Create a matrix with the same number of columns as ts_data3
  new_data <- matrix(NA, nrow = numeric_values[gaps_indices[i]], ncol = ncol(ts_data1))
  
  # Generate time values with a 60-second interval
  time_values <- seq(start_time, by = 60, length.out = numeric_values[gaps_indices[i]])
  
  # Create an xts object with new_data and time_values
  new_data <- xts(new_data, order.by = time_values)
  
  # Insert the new_data into ts_data3 at the specified gap_start
  modified_data <- insertTable(modified_data, new_data[2:numeric_values[gaps_indices[i]]], gap_start)
}

# Verify that the modifications resolve the issues
ts_data1 <- as.xts(modified_data)



timestamps <- index(ts_data2)
time_diffs <- diff(timestamps)
numeric_values <- as.numeric(time_diffs)
gaps_indices <- which(time_diffs >= 2)
modified_data <- ts_data2

for (i in 1:length(gaps_indices)) {
  
  gap_start <- gaps_indices[i]
  
  # Ensure correct indexing for the timestamp
  start_time <- as.POSIXct(ts_data2[gap_start, 1], format = "%Y-%m-%d %H:%M:%S")
  
  # Create a matrix with the same number of columns as ts_data3
  new_data <- matrix(NA, nrow = numeric_values[gaps_indices[i]], ncol = ncol(ts_data2))
  
  # Generate time values with a 60-second interval
  time_values <- seq(start_time, by = 60, length.out = numeric_values[gaps_indices[i]])
  
  # Create an xts object with new_data and time_values
  new_data <- xts(new_data, order.by = time_values)
  
  # Insert the new_data into ts_data3 at the specified gap_start
  modified_data <- insertTable(modified_data, new_data[2:numeric_values[gaps_indices[i]]], gap_start)
}

# Verify that the modifications resolve the issues
ts_data2 <- as.xts(modified_data)


y1.LLM <- SSModel(ts_data1$PM10 ~ SSMtrend(1, Q=10), H=NA )
y2.LLM <- SSModel(ts_data2$PM10 ~ SSMtrend(1, Q=10), H=NA )
y3.LLM <- SSModel(ts_data3$PM10 ~ SSMtrend(1, Q=NA), H=NA )

# maximum likelihood estimation of paramaters of Q and H (i.e. variance of the two inovations)
pars1 <- log( rep(var(ts_data1$PM10, na.rm=TRUE), 2) )
pars2 <- log( rep(var(ts_data2$PM10, na.rm=TRUE), 2) )
pars3 <- log( rep(var(ts_data3$PM10, na.rm=TRUE), 2) )


y1.fit <- fitSSM( model=y1.LLM, inits=pars1, method='BFGS' )
y2.fit <- fitSSM( model=y2.LLM, inits=pars2, method='BFGS' )
y3.fit <- fitSSM( model=y3.LLM, inits=pars3, method='BFGS' )

# run Kalman Filter and Smoother with estimated parameters
y1.out <- KFS(y1.fit$model)
y2.out <- KFS(y2.fit$model)
y3.out <- KFS(y3.fit$model)

# construct 90% confidence intervals for smoothed state
y1.KS.CI <- predict(y1.fit$model, interval="confidence", level=0.9)
y2.KS.CI <- predict(y2.fit$model, interval="confidence", level=0.9)
y3.KS.CI <- predict(y3.fit$model, interval="confidence", level=0.9)

# Set a common x-axis range for all plots
common_xrange <- c(0, 300)
common_yrange <- c(0, 300)
par(mfrow=c(2,2), cex=0.6); 
ts.plot(ts_data1$PM10, y1.KS.CI, col = c("black", "red", "red", "red"), lty = c(1, 1, 2, 2), lwd = c(2, 2, 1, 1),
        xlab = "Time", ylab = "PM10 ug/m", main = "MAEPH034 (pers2)", xlim = common_xrange, ylim=common_yrange);grid()
ts.plot(ts_data2$PM10, y2.KS.CI, col = c("black", "red", "red", "red"), lty = c(1, 1, 2, 2), lwd = c(2, 2, 1, 1),
        xlab = "Time", ylab = "PM10 ug/m", main = "MAEPH043 (pers2)", xlim = common_xrange, ylim=common_yrange);grid()
ts.plot(ts_data3$PM10, y3.KS.CI, col = c("black", "red", "red", "red"), lty = c(1, 1, 2, 2), lwd = c(2, 2, 1, 1),
        xlab = "Time", ylab = "PM10 ug/m", main = "MAEPH028 (stat1)", xlim = common_xrange, ylim=common_yrange);grid()


cor(y1.KS.CI[1:322,1], y3.KS.CI[1:322,1])
cor(y2.KS.CI[1:322,1], y3.KS.CI[1:322,1])
cor(y1.KS.CI[1:322,1], y2.KS.CI[1:322,1])


# Function to initialize the Kalman filter
init_kalman <- function(y) {
  X <- matrix(c(y, 0), nrow = 2, byrow = TRUE)
  P <- matrix(c(1, 0, 0, 1), nrow = 2, byrow = TRUE)
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


# Main function to run the Kalman filter and plot the results
main <- function() {
  dt <- 1
  t <- seq(1, 322, by = dt)
  n <- 322
  
  # Define transition matrix (F) and process noise covariance matrix (Q)
  F <- matrix(c(1, dt, 0, 1), nrow = 2, byrow = TRUE)
  Q <- matrix(c(20, 0, 0, 20), nrow = 2, byrow = TRUE)
  
  # Ground truth signal
  signalX <- y3.KS.CI[1:322,1] # as.zoo(ts_data3$PM10[1:322])#y3.KS.CI[1:322,1] #as.zoo(ts_data3$PM10[1:322])  ## 
  
  # Initialize the Kalman filter
  initial_observation <- y3.KS.CI[1,1] #data_sens3$PM10[1]
  kalman_state <- init_kalman(initial_observation)
  
  # Create empty arrays to store results
  X_arr <- matrix(0, n, 2)
  
  X_confidence_upper <- vector("numeric", length = n)
  X_confidence_lower <- vector("numeric", length = n)
  
  s1 <- y1.KS.CI[1:322,1]
  s2 <- y2.KS.CI[1:322,1]
  
  # Fusion
  for (i in 1:n) {
    if (i == 1) {
      kalman_state <- init_kalman(signalX[i]) 
      
    } else {
      kalman_state <- prediction(kalman_state$X, kalman_state$P, Q, F)
      kalman_state <- update(kalman_state$X, kalman_state$P, s1[i], var(s1), matrix(c(1, 0), nrow = 1, byrow = TRUE))
      kalman_state <- update(kalman_state$X, kalman_state$P, s2[i], var(s2), matrix(c(1, 0), nrow = 1, byrow = TRUE))
    }
    
    X_arr[i, ] <- kalman_state$X
    
    # Calculate the confidence intervals
    X_confidence_upper[i] <- X_arr[i, 1] + 1.96 * sqrt(kalman_state$P[1, 1])  
    X_confidence_lower[i] <- X_arr[i, 1] - 1.96 * sqrt(kalman_state$P[1, 1])
  }
  

  plot(t,signalX, type = "l", lwd =1 , col = "black", xlab = "Time", ylab = "PM10 ug/m3",
       xlim = common_xrange, ylim=common_yrange);grid()

  polygon(c(t, rev(t)), c(X_confidence_upper, rev(X_confidence_lower)), col = "lightgrey", border = NA)
  lines(t, X_arr[, 1], lwd = 4, col = "grey20")
  lines(t, s1, lty = 2, lwd = 2, col = "black")
  lines(t, s2, lty = 2, lwd = 2, col = "blue")
  lines(t,signalX, type = "l", lwd =4 , col = "red")
  
 legend("topleft", legend = c("Ground Truth",  "Confidence Interval", "Fused Output", "Sensor Input 1", "Sensor Input 2" ),
         col = c("red", "lightgrey", "grey20", "black", "blue"),
         lty = c(1, 1, 1,  1, 1),
         lwd = c(2, 0.5, 2,  2, 1),
         fill = c(NA,  "lightgrey", NA, NA,  NA, alpha = 0.05))
  
  grid()
  
  cor(y3.KS.CI[1:322,1],X_arr[1:322,1])
  
  
}


# Run the main function
main()


