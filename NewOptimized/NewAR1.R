################################################################################
# Script Overview: Autoregressive OLS and Multivariate OLS Forecast
################################################################################

# Author: Alessandro Dodon
# Last Update: 12-12-2024
# Description:
# This script implements two approaches for forecasting CPIULFSL (inflation proxy):
# 1. Autoregressive OLS (AR(1)): Uses only the CPIULFSL variable from past values 
#    to predict future values.
# 2. OLS with All Predictors: Uses all available predictors from the dataset.
# Both models employ recursive predictions for the test dataset. 
# Evaluation includes error metrics (MAE, MSE, RMSE) and visualization of 
# actual vs predicted values.

# Dependencies:
# - This script requires preprocessed datasets `train_data_holdout` and 
#   `test_data_holdout`. Ensure these are generated using the preprocessing 
#   script before running this script.

# Packages:
# - Required R packages:
#   `stats`, `forecast`, `ggplot2`, `dplyr`
# - If a required package is missing, uncomment the corresponding `install.packages` 
#   line in the "Prepare Packages" section below and run it to install.

# Outputs:
# - Prediction error metrics (MAE, MSE, RMSE) displayed in the console.
# - Visualization of actual vs predicted values and residuals saved as PDFs in 
#   the working directory.

################################################################################
# Prepare Packages
################################################################################

# Uncomment the lines below to install the necessary packages if not already installed:
# install.packages("stats")
# install.packages("forecast")
# install.packages("ggplot2")
# install.packages("dplyr")

# Load required libraries
suppressWarnings(suppressMessages({
  library(stats)
  library(forecast)
  library(ggplot2)
  library(dplyr)
}))

################################################################################
# AR(1) OLS
################################################################################

# Folder
plots_folder <- "ols_results"
if (!dir.exists(plots_folder)) {
  dir.create(plots_folder)
}

# Initial shifting for training data
CPIULFSL_train <- train_data_holdout[, "CPIULFSL"]
Y_train <- CPIULFSL_train[-1]  # Future values for training set

train_data_holdout_no_date <- train_data_holdout[,-1]
X_train_matrix <- head(train_data_holdout_no_date, -1)

# Convert to numeric vectors
Y_train_vec <- as.numeric(Y_train)
X_train_vec <- as.numeric(X_train_matrix[, "CPIULFSL"])  # Correctly use CPIULFSL column
n_train <- length(Y_train_vec)

# Initial shifting for test data
CPIULFSL_test <- test_data_holdout[, "CPIULFSL"]
Y_test <- CPIULFSL_test[-1]  # Future values for test set
X_test_vec <- as.numeric(CPIULFSL_test[-length(CPIULFSL_test)])  # Past values for test set

# Number of test observations
n_test <- 239

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
  new_X <- (X_test_vec[i] - mean_X_train) / sd_X_train
  prediction_standardized <- predict(model, newdata = data.frame(X_train_standardized = new_X))
  
  # De-standardize the prediction
  prediction <- prediction_standardized * sd_Y_train + mean_Y_train
  predictions[i] <- prediction
  
  # Append the actual test value to the training data
  actual_value <- Y_test[i]
  Y_train_vec <- c(Y_train_vec, actual_value)
  X_train_vec <- c(X_train_vec, X_test_vec[i])
  
  # Debug statements to verify the size of the training data
  cat("Iteration:", i, "\n")
  cat("Size of Y_train_vec:", length(Y_train_vec), "\n")
  cat("Size of X_train_vec:", length(X_train_vec), "\n")
}

################################################################################
# Confront recursive OLS predictions with actual test values
################################################################################

# Extract the actual values from the test data
actual_values <- Y_test[1:n_test]  # Adjusted to match the number of predictions

# Calculate prediction errors for the test set
test_errors <- actual_values - predictions
test_mae <- mean(abs(test_errors))
test_mse <- mean(test_errors^2)
test_rmse <- sqrt(test_mse)

# Display the test errors and error metrics
cat("Test Mean Absolute Error (MAE):", test_mae, "\n")
cat("Test Mean Squared Error (MSE):", test_mse, "\n")
cat("Test Root Mean Squared Error (RMSE):", test_rmse, "\n")

# Prepare data frame for plots
df_test <- data.frame(Date = test_data_holdout$date[1:n_test],
                      Actual = actual_values,
                      Predicted = predictions)

# Plot 1: Actual vs Predicted Scatter Plot
scatter_plot <- ggplot(df_test, aes(x = Actual, y = Predicted)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  ggtitle("OLS: Actual vs Predicted Values") +
  labs(x = "Actual", y = "Predicted") +
  theme_minimal()

ggsave(file.path(plots_folder, "scatter_actual_vs_predicted_ols.pdf"), 
       plot = scatter_plot, width = 10, height = 8, dpi = 300, units = "in")

# Plot 2: Residual Plot
residual_plot <- ggplot(df_test, aes(x = Actual, y = Actual - Predicted)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red") +
  ggtitle("OLS: Residual Plot") +
  labs(x = "Actual", y = "Residual (Actual - Predicted)") +
  theme_minimal()

ggsave(file.path(plots_folder, "residual_plot_ols.pdf"), 
       plot = residual_plot, width = 10, height = 8, dpi = 300, units = "in")

# Plot 3: Actual vs Predicted Time Series Plot
time_series_plot <- ggplot(df_test, aes(x = Date)) +
  geom_line(aes(y = Actual, color = "Actual")) +
  geom_line(aes(y = Predicted, color = "Predicted")) +
  labs(title = "OLS: Actual vs Predicted Time Series",
       x = "Date", y = "CPIULFSL", color = "Legend") +
  theme_minimal() +
  scale_color_manual(values = c("Actual" = "blue", "Predicted" = "red"))

ggsave(file.path(plots_folder, "time_series_actual_vs_predicted_ols.pdf"), 
       plot = time_series_plot, width = 10, height = 8, dpi = 300, units = "in")

################################################################################
# AR(1) OLS with ALL variables
################################################################################

# Folder
plots_folder <- "ols_all_variables_results"
if (!dir.exists(plots_folder)) {
  dir.create(plots_folder)
}

# Re-prepare the data with the correct shifts
# Create vector for CPIULFSL at t+1 (represent future values)
CPIULFSL_train <- train_data_holdout[, "CPIULFSL"]

# Take first value out to shift every observation
Y_train <- CPIULFSL_train[-1]  # Y for OLS

# Modify also the matrix for the training data
train_data_holdout_no_date <- train_data_holdout[, -1]
X_train_matrix <- head(train_data_holdout_no_date, -1)

# Initialize vectors with the original training data
Y_train_vec <- as.numeric(Y_train)
X_train_matrix <- as.matrix(X_train_matrix)
n_train <- length(Y_train_vec)

# Initial shifting for test data
CPIULFSL_test <- test_data_holdout[, "CPIULFSL"]
Y_test <- CPIULFSL_test[-1]  # Future values for test set

test_data_holdout_no_date <- test_data_holdout[, -1]
X_test_matrix <- head(test_data_holdout_no_date, -1)

# Convert to numeric and matrix format
Y_test_vec <- as.numeric(Y_test)
X_test_matrix <- as.matrix(X_test_matrix)

# Number of test observations
n_test <- 239  # Adjusted to match the number of test predictions

# Create vectors to store predictions
predictions <- numeric(n_test)

# Perform recursive OLS and predict the next value
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
  
  # Use the last row of X_train_matrix for prediction
  new_X <- scale(matrix(as.numeric(X_test_matrix[i, ]), nrow = 1), center = mean_X_train, scale = sd_X_train)
  prediction_standardized <- predict(model, newdata = as.data.frame(new_X))
  
  # De-standardize the prediction
  prediction <- prediction_standardized * sd_Y_train + mean_Y_train
  predictions[i] <- prediction
  
  # Append the actual test value to the training data
  actual_value <- Y_test_vec[i]
  Y_train_vec <- c(Y_train_vec, actual_value)
  X_train_matrix <- rbind(X_train_matrix, as.numeric(X_test_matrix[i, ]))
  
  # Debug statements to verify the size of the training data
  cat("Iteration:", i, "\n")
  cat("Size of Y_train_vec:", length(Y_train_vec), "\n")
  cat("Size of X_train_matrix:", nrow(X_train_matrix), "\n")
}

################################################################################
# Confront recursive OLS predictions with actual test values
################################################################################

# Extract the actual values from the test data
actual_values <- Y_test_vec[1:n_test]  # Adjusted to match the number of predictions

# Calculate prediction errors for the test set
test_errors <- actual_values - predictions
test_mae <- mean(abs(test_errors))
test_mse <- mean(test_errors^2)
test_rmse <- sqrt(test_mse)

# Display the test errors and error metrics
cat("Test Mean Absolute Error (MAE):", test_mae, "\n")
cat("Test Mean Squared Error (MSE):", test_mse, "\n")
cat("Test Root Mean Squared Error (RMSE):", test_rmse, "\n")

# Prepare data frame for plotting
df_test <- data.frame(Date = test_data_holdout$date[-1][1:n_test], Actual = actual_values, Predicted = predictions)

# Plot 1: Actual vs Predicted Scatter Plot
scatter_plot <- ggplot(df_test, aes(x = Actual, y = Predicted)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  ggtitle("OLS: Actual vs Predicted Values") +
  labs(x = "Actual", y = "Predicted") +
  theme_minimal()

ggsave(file.path(plots_folder, "scatter_actual_vs_predicted_ols.pdf"), 
       plot = scatter_plot, width = 10, height = 8, dpi = 300, units = "in")

# Plot 2: Residual Plot
residual_plot <- ggplot(df_test, aes(x = Actual, y = Actual - Predicted)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red") +
  ggtitle("OLS: Residual Plot") +
  labs(x = "Actual", y = "Residual (Actual - Predicted)") +
  theme_minimal()

ggsave(file.path(plots_folder, "residual_plot_ols.pdf"), 
       plot = residual_plot, width = 10, height = 8, dpi = 300, units = "in")

# Plot 3: Actual vs Predicted Time Series Plot
time_series_plot <- ggplot(df_test, aes(x = Date)) +
  geom_line(aes(y = Actual, color = "Actual")) +
  geom_line(aes(y = Predicted, color = "Predicted")) +
  labs(title = "OLS: Actual vs Predicted Time Series",
       x = "Date", y = "CPIULFSL", color = "Legend") +
  theme_minimal() +
  scale_color_manual(values = c("Actual" = "blue", "Predicted" = "red"))

ggsave(file.path(plots_folder, "time_series_actual_vs_predicted_ols.pdf"), 
       plot = time_series_plot, width = 10, height = 8, dpi = 300, units = "in")

