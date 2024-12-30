################################################################################
# Script Overview: Autoregressive OLS and Multivariate OLS Forecast
################################################################################

# Author: Alessandro Dodon
# Last Update: 12-12-2024
# Description:
# This script implements two forecasting approaches for CPIULFSL (inflation proxy):
# 1. Autoregressive OLS (AR(1)): Uses only CPIULFSL past values.
# 2. OLS with All Predictors: Uses all available predictors.
# Both models use the updated lagging and splitting logic for consistency and precision.
# Evaluation includes error metrics (MAE, MSE, RMSE) and visualization of results.

################################################################################
# Dependencies:
# - This script requires preprocessed dataset `transformed_data_cleaned_no_COVID` 
#   already split into X_train, Y_train, X_test, Y_test.
# - Packages required: `stats`, `ggplot2`, `dplyr`
################################################################################

################################################################################
# Prepare Packages
################################################################################

# Uncomment to install necessary packages if not already installed:
# install.packages("stats")
# install.packages("ggplot2")
# install.packages("dplyr")

suppressWarnings(suppressMessages({
  library(stats)
  library(ggplot2)
  library(dplyr)
}))

################################################################################
# AR(1) OLS Forecast
################################################################################

plots_folder <- "ols_results"
if (!dir.exists(plots_folder)) {
  dir.create(plots_folder)
}

# Convert to numeric vectors
Y_train_vec <- as.numeric(Y_train)
X_train_vec <- as.numeric(X_train[, "CPIULFSL"])
n_train <- length(Y_train_vec)

Y_test_vec <- as.numeric(Y_test)
X_test_vec <- as.numeric(X_test[, "CPIULFSL"])
n_test <- length(Y_test_vec)

# Create vectors to store predictions
predictions <- numeric(n_test)

# Perform recursive rolling OLS and predict the next value
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
  new_X <- (X_test_vec[i] - mean_X_train) / sd_X_train
  prediction_standardized <- predict(model, newdata = data.frame(X_train_standardized = new_X))
  
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
test_mae <- mean(abs(test_errors))
test_mse <- mean(test_errors^2)
test_rmse <- sqrt(test_mse)

cat("Test Mean Absolute Error (MAE):", test_mae, "\n")
cat("Test Mean Squared Error (MSE):", test_mse, "\n")
cat("Test Root Mean Squared Error (RMSE):", test_rmse, "\n")

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

ggsave(file.path(plots_folder, "time_series_actual_vs_predicted_ols.pdf"), 
       plot = time_series_plot, width = 10, height = 8, dpi = 300, units = "in")

# Plot: Residual Plot
residual_plot <- ggplot(df_test, aes(x = Actual, y = Actual - Predicted)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red") +
  labs(title = "OLS: Residual Plot",
       x = "Actual Values", y = "Residuals") +
  theme_minimal()

ggsave(file.path(plots_folder, "residual_plot_ols.pdf"), 
       plot = residual_plot, width = 10, height = 8, dpi = 300, units = "in")

# Plot: Actual vs Predicted Scatter
scatter_plot <- ggplot(df_test, aes(x = Actual, y = Predicted)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  labs(title = "OLS: Actual vs Predicted Scatter Plot",
       x = "Actual Values", y = "Predicted Values") +
  theme_minimal()

ggsave(file.path(plots_folder, "scatter_plot_actual_vs_predicted_ols.pdf"), 
       plot = scatter_plot, width = 10, height = 8, dpi = 300, units = "in")

################################################################################
# OLS with All Predictors Forecast
################################################################################

plots_folder_all <- "ols_all_variables_results"
if (!dir.exists(plots_folder_all)) {
  dir.create(plots_folder_all)
}

# Initialize vectors
Y_train_vec <- as.numeric(Y_train)
X_train_matrix <- as.matrix(X_train)

Y_test_vec <- as.numeric(Y_test)
X_test_matrix <- as.matrix(X_test)

# Create vectors to store predictions
predictions <- numeric(n_test)

# Perform recursive rolling OLS with all predictors
for (i in 1:n_test) {
  # Standardize the current training data
  mean_Y_train <- mean(Y_train_vec)
  sd_Y_train <- sd(Y_train_vec)
  Y_train_standardized <- (Y_train_vec - mean_Y_train) / sd_Y_train
  
  mean_X_train <- colMeans(X_train_matrix)
  sd_X_train <- apply(X_train_matrix, 2, sd)
  X_train_standardized <- scale(X_train_matrix, center = mean_X_train, scale = sd_X_train)
  
  # Fit the linear model using current standardized training data
  model <- lm(Y_train_standardized ~ X_train_standardized)
  
  # Use the corresponding row from the test set for prediction
  new_X <- matrix(X_test_matrix[i, ], nrow = 1)  # Reshape to a 1-row matrix
  new_X <- scale(new_X, center = mean_X_train, scale = sd_X_train)
  prediction_standardized <- predict(model, newdata = as.data.frame(t(new_X)))
  
  # De-standardize the prediction
  prediction <- prediction_standardized * sd_Y_train + mean_Y_train
  predictions[i] <- prediction
  
  # Append the actual test value to the training data
  actual_value <- Y_test_vec[i]
  Y_train_vec <- c(Y_train_vec, actual_value)
  X_train_matrix <- rbind(X_train_matrix, X_test_matrix[i, ])
  
  cat("Iteration:", i, "\n")
}

################################################################################
# Evaluate and Visualize Results for All Predictors
################################################################################

# Calculate prediction errors for the test set
test_errors <- Y_test_vec - predictions
test_mae <- mean(abs(test_errors))
test_mse <- mean(test_errors^2)
test_rmse <- sqrt(test_mse)

cat("Test Mean Absolute Error (MAE):", test_mae, "\n")
cat("Test Mean Squared Error (MSE):", test_mse, "\n")
cat("Test Root Mean Squared Error (RMSE):", test_rmse, "\n")

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

ggsave(file.path(plots_folder_all, "time_series_actual_vs_predicted_ols_all.pdf"), 
       plot = time_series_plot, width = 10, height = 8, dpi = 300, units = "in")

# Plot: Residual Plot
residual_plot <- ggplot(df_test, aes(x = Actual, y = Actual - Predicted)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red") +
  labs(title = "OLS with All Predictors: Residual Plot",
       x = "Actual Values", y = "Residuals") +
  theme_minimal()

ggsave(file.path(plots_folder_all, "residual_plot_ols_all.pdf"), 
       plot = residual_plot, width = 10, height = 8, dpi = 300, units = "in")

# Plot: Actual vs Predicted Scatter
scatter_plot <- ggplot(df_test, aes(x = Actual, y = Predicted)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  labs(title = "OLS with All Predictors: Actual vs Predicted Scatter Plot",
       x = "Actual Values", y = "Predicted Values") +
  theme_minimal()

ggsave(file.path(plots_folder_all, "scatter_plot_actual_vs_predicted_ols_all.pdf"), 
       plot = scatter_plot, width = 10, height = 8, dpi = 300, units = "in")

