
# Periklis Kontoroupis
# 3/12/24
# Fit the best ARIMA stucture to data 

library(tidyverse)
library(forecast)
library(ggplot2)
library(data.table)

# Replace with the correct absolute file path to your .rds file
file_path <- "C:/.../20230210 lab data for kriging_v2.rds"

# Read the RDS file
sensor_data <- readRDS(file_path)


tidy_sensor_data <- sensor_data %>%
  pivot_longer(
    cols = c(PM2.5, x, y, PM2.5_sub),
    names_to = "variable",
    values_to = "value"
  )


tidy_sensors_day1 <- tidy_sensor_data %>%
  filter ( exp_no == "B" & variable == "PM2.5_sub" )


num_samples <- 901


# Filter data for day 1 and PM2.5_sub variable
tidy_sensors_day1 <- tidy_sensor_data %>%  filter(exp_no == "A" & variable == "PM2.5_sub")

# Introduce 10% missing data to the PM2.5_sub variable
missing_data_percentage <- 0
tidy_sensors_day1 <- tidy_sensors_day1 %>%
  mutate(value = ifelse(runif(n()) < missing_data_percentage, NA, value))


# Function to calculate R-squared
calculate_r_squared <- function(observed, predicted) {
  mean_observed <- mean(observed)
  numerator <- sum((observed - predicted)^2)
  denominator <- sum((observed - mean_observed)^2)
  r_squared <- 1 - (numerator / denominator)
  return(r_squared)
}


filtered_data_list <- list()
acf_data_list <- list()
forecast_state_list <- list()

# Loop through each unique sensorID and apply Kalman filter
unique_sensor_ids <- tidy_sensors_day1$sensorID %>% unique()

for (sensor_id in unique_sensor_ids) {
  # Subset data for the current sensorID
  sensor_data <- tidy_sensors_day1 %>%
    filter(sensorID == sensor_id)
  
  
  # Process sensor measurements through Kalman filter
  arima_model <- auto.arima(sensor_data$value[1:800])
  forecast_state <- forecast(arima_model, h=100)
  forecast_state_list[[sensor_id]] <- forecast_state 
  
  #ts.plot(sensor_data$value)
  #plot(forecast_state)

  # Compute residuals
  arima_model <- auto.arima(sensor_data$value)
  residuals <- residuals(arima_model)# sensor_data$value - filtered_state$series
  filtered_state <- sensor_data$value -  residuals(arima_model)
  
##
  #fit <- auto.arima(sensor_data)
  #order <- fit$arma
  #Est_value <- sensor_data - residuals(fit)
  # Generate the forecast
  #forecast_data <- forecast(fit, h = 100)
  # Plot the forecasted data
  plot(forecast_state, xlim = c(1, 900))#, 
    #   main = paste("Forecast with ARIMA(", paste(order, collapse = ", "), ") Model Order"))
  # Add the original sensor_data
  lines(sensor_data$value, col = "black", lty = 1, xlab="Time (min)", ylab="PM 2_5")
  # Add legend
  legend("bottomleft", legend = c("Forecasted Data", "Original Sensor Data"),
         col = c("blue", "black", "black"), lty = c(1, 1, 2))
  ##
  
  # Calculate the ACF for original sensor data
  acf_original <- acf(sensor_data$value, lag.max = 30, plot = FALSE)
  
  # Calculate the ACF for filtered data (residuals)
  acf_filtered <- acf(residuals, lag.max = 30, plot = FALSE)
  
#  acf(sensor_data$value, lag.max = 30, plot = FALSE)
 # acf(residuals, lag.max = 30, plot = FALSE)
  acf(residuals)
  # Combine ACF data for both original and filtered data
  acf_combined <- rbind(data.frame(lag = acf_original$lag, acf = acf_original$acf, type = "Original", sensorID = sensor_id),
                        data.frame(lag = acf_filtered$lag, acf = acf_filtered$acf, type = "ARIMA", sensorID = sensor_id))
  
  acf_data_list[[sensor_id]] <- acf_combined
  

  
  # Calculate R-squared
  r_squared <- calculate_r_squared(sensor_data$value, filtered_state)
  print(paste("R-squared:", round(r_squared, 4)))
  
  
  # Create a data frame with filtered data
  filtered_data <- data.frame(
    sensorID = sensor_id,
    value = filtered_state,
    difftime = sensor_data$difftime,
    residuals = residuals#,
    #forecasts = forecast_state$Forecast
  )
  
  # Store filtered data for the current sensorID
  filtered_data_list[[sensor_id]] <- filtered_data

}

filtered_data_combined <- do.call(rbind, filtered_data_list)

# Combine ACF data for all sensors
acf_combined_all <- do.call(rbind, acf_data_list)



# Create a data.table from the list of data frames
converted_data_table <- rbindlist(forecast_state_list, idcol = "deviceID")

# Print the structure of the resulting data.table
#str(converted_data_table)


# Plot the panel plot with ACF comparison for all sensors
ggplot(acf_combined_all, aes(x = lag, y = acf)) +
  geom_bar(stat = "identity", position = "dodge", aes(fill = type)) +
  labs(x = "Lag", y = "ACF", title = "ACF Comparison for Sensors") +
  facet_wrap(~ sensorID, ncol = 2) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))



# Plot the panel plot with the filtered data, residuals, and histograms
ggplot(filtered_data_combined, aes(x = residuals)) +
  geom_histogram(binwidth = 1, fill = "blue", alpha = 0.5) +  # Adjust binwidth as needed
  labs(x = "Residuals", y = "Count") +
  ggtitle("Residuals of Kalman Filtered Data for Sensors 1 ~ 6 DAY 1") +
  facet_wrap(~ sensorID, ncol = 2) +
  theme(legend.position = "bottom") +
  coord_cartesian(xlim = c(-50, 50))






#filtered_data_combined <- do.call(rbind, filtered_data_list)
forecast_state_combined <- do.call(rbind, forecast_state_list)


plot_data <- forecast_state_combined %>%
  select(device, method, model, level, mean, lower, upper, sensorID)

# Step 2: Create ggplot with facet_wrap
ggplot(plot_data, aes(x = x, y = mean, color = method, fill = method)) +
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
  labs(title = "Forecast for each sensorID",
       x = "X-axis label",
       y = "Y-axis label") +
  facet_wrap(~ sensorID, ncol = 2) +
  theme_minimal()

# Combine ACF data for all sensors
acf_combined_all <- do.call(rbind, acf_data_list)

