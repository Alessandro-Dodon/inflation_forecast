################################################################################
# AR(1) OLS 
################################################################################

# Convert to numeric vectors
Y_train_vec <- as.numeric(Y_train) # Y is already defined, just need to convert it to numeric vector
X_train_vec <- as.numeric(X_train[, "CPIULFSL"]) # Extracts vector from the matrix, this is inflation lagged 
n_train <- length(Y_train_vec)

Y_test_vec <- as.numeric(Y_test)
X_test_vec <- as.numeric(X_test[, "CPIULFSL"])
n_test <- length(Y_test_vec)

# Create vectors to store predictions
predictions <- numeric(n_test)

# Perform recursive OLS and predict the next value
for (i in 1:n_test) {
  # Standardize the current training data
  mean_Y_train <- mean(Y_train_vec)
  sd_Y_train <- sd(Y_train_vec)
  Y_train_standardized <- (Y_train_vec - mean_Y_train) / sd_Y_train
  
  mean_X_train <- mean(X_train_vec)
  sd_X_train <- sd(X_train_vec)
  X_train_standardized <- (X_train_vec - mean_X_train) / sd_X_train
  
  # Fit the linear model using current standardized training data
  model <- lm(Y_train_standardized ~ X_train_standardized)
  
  # Use the corresponding value from the test set for prediction
  new_X <- scale(matrix(X_test_vec[i], nrow = 1), center = mean_X_train, scale = sd_X_train)
  new_X <- as.data.frame(new_X) # Convert to a data frame to match model input
  colnames(new_X) <- "X_train_standardized" # Ensure column names match
  
  # Predict using the model
  prediction_standardized <- predict(model, newdata = new_X)
  
  # De-standardize the prediction
  prediction <- prediction_standardized * sd_Y_train + mean_Y_train
  predictions[i] <- prediction
  
  # Append the actual test value to the training data
  actual_value <- Y_test_vec[i]
  Y_train_vec <- c(Y_train_vec, actual_value)
  X_train_vec <- c(X_train_vec, X_test_vec[i])
  
  cat("Iteration:", i, "\n")
}

################################################################################
# Evaluate and Visualize Results
################################################################################

# Calculate prediction errors for the test set
test_errors <- Y_test_vec - predictions
test_mse <- mean(test_errors^2)

cat("Test Mean Squared Error (MSE):", test_mse, "\n")

# Prepare data frame for visualization
df_test <- data.frame(Date = Y_test_with_date$date, Actual = Y_test_vec, Predicted = predictions)

# Plot: Actual vs Predicted Time Series
time_series_plot <- ggplot(df_test, aes(x = Date)) +
  geom_line(aes(y = Actual, color = "Actual")) +
  geom_line(aes(y = Predicted, color = "Predicted")) +
  labs(title = "OLS: Actual vs Predicted Time Series",
       x = "Date", y = "CPIULFSL", color = "Legend") +
  theme_minimal() +
  scale_color_manual(values = c("Actual" = "blue", "Predicted" = "red"))

ggsave("actual_vs_predicted_ols.pdf", 
       plot = time_series_plot, width = 10, height = 8, dpi = 300, units = "in")

################################################################################
# OLS with All Predictors 
################################################################################

# Initialize vectors
Y_train_vec <- as.numeric(Y_train)
X_train_matrix <- as.matrix(X_train) # Here we use the entire matrix for X

Y_test_vec <- as.numeric(Y_test)
X_test_matrix <- as.matrix(X_test)
n_test <- length(Y_test_vec)

# Create vectors to store predictions
predictions <- numeric(n_test)

# Perform recursive OLS with all predictors
for (i in 1:n_test) {
  # Standardize the current training data
  mean_Y_train <- mean(Y_train_vec)
  sd_Y_train <- sd(Y_train_vec)
  Y_train_standardized <- (Y_train_vec - mean_Y_train) / sd_Y_train
  
  mean_X_train <- colMeans(X_train_matrix)
  sd_X_train <- apply(X_train_matrix, 2, sd)
  X_train_standardized <- scale(X_train_matrix, center = mean_X_train, scale = sd_X_train)
  
  # Fit the linear model using current standardized training data
  model <- lm(Y_train_standardized ~ ., data = as.data.frame(X_train_standardized))
  
  # Use the corresponding row from the test set for prediction
  new_X <- scale(X_test_matrix[i, , drop = FALSE], center = mean_X_train, scale = sd_X_train)
  new_X <- as.data.frame(new_X)  # Convert to a data frame to match model input
  colnames(new_X) <- colnames(X_train_standardized)  # Ensure column names match
  
  # Predict using the model
  prediction_standardized <- predict(model, newdata = new_X)
  
  # De-standardize the prediction
  prediction <- prediction_standardized * sd_Y_train + mean_Y_train
  predictions[i] <- prediction
  
  # Append the actual test value to the training data
  actual_value <- Y_test_vec[i]
  Y_train_vec <- c(Y_train_vec, actual_value)
  X_train_matrix <- rbind(X_train_matrix, X_test_matrix[i, ])
  
  cat("Iteration:", i, "\n")
}

# NOTE:
# Prediction with `lm` requires both column order and column names to match 
# between the predictors in the test data and the training data.

################################################################################
# Evaluate and Visualize Results for All Predictors
################################################################################

# Calculate prediction errors for the test set
test_errors <- Y_test_vec - predictions
test_mse <- mean(test_errors^2)

cat("Test Mean Squared Error (MSE):", test_mse, "\n")

# Prepare data frame for visualization
df_test <- data.frame(Date = Y_test_with_date$date, Actual = Y_test_vec, Predicted = predictions)

# Plot: Actual vs Predicted Time Series
time_series_plot <- ggplot(df_test, aes(x = Date)) +
  geom_line(aes(y = Actual, color = "Actual")) +
  geom_line(aes(y = Predicted, color = "Predicted")) +
  labs(title = "OLS with All Predictors: Actual vs Predicted Time Series",
       x = "Date", y = "CPIULFSL", color = "Legend") +
  theme_minimal() +
  scale_color_manual(values = c("Actual" = "blue", "Predicted" = "red"))

ggsave("actual_vs_predicted_ols_all.pdf", 
       plot = time_series_plot, width = 10, height = 8, dpi = 300, units = "in")

################################################################################
# Historical Average 
################################################################################

# Calculate the mean of the actual values in the test set
mean_forecast <- mean(X_train_vec)

# Create a vector of forecasts, all equal to the mean
mean_predictions <- rep(mean_forecast, length(Y_test_vec))

# Calculate the prediction errors
mean_forecast_errors <- Y_test_vec - mean_predictions

# Calculate the MSE
mean_forecast_mse <- mean(mean_forecast_errors^2)

# Print the result
print(mean_forecast_mse)

# Prepare data frame for visualization
df_average <- data.frame(Date = Y_test_with_date$date, Actual = Y_test_vec, Predicted = mean_predictions)

# Plot: Actual vs Predicted Time Series for Historical Average
average_time_series_plot <- ggplot(df_average, aes(x = Date)) +
  geom_line(aes(y = Actual, color = "Actual")) +
  geom_line(aes(y = Predicted, color = "Predicted")) +
  labs(title = "Historical Average: Actual vs Predicted Time Series",
       x = "Date", y = "CPIULFSL", color = "Legend") +
  theme_minimal() +
  scale_color_manual(values = c("Actual" = "blue", "Predicted" = "red"))

# Save the plot as PDF
ggsave("actual_vs_predicted_average.pdf", 
       plot = average_time_series_plot, width = 10, height = 8, dpi = 300, units = "in")

