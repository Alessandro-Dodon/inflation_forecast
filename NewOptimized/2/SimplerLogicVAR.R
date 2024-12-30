################################################################################
# Script Overview: VAR Forecast with Multiple PCA Configurations (Dynamic PCA)
################################################################################

# Author: Alessandro Dodon
# Last Update: 12-12-2024
# Description:
# This script implements forecasts with Vector Autoregression (VAR) models 
# using different numbers of PCA-transformed predictors. The PCA calculation 
# is dynamic, adapting to updated training data during recursive predictions. 
# Performance is evaluated through error metrics and visualizations for each 
# configuration (time series, residual, and scatter plots).

# Dependencies:
# - Requires preprocessed datasets `X_train`, `Y_train`, `X_test`, `Y_test`.
# - Packages required: `stats`, `vars`, `ggplot2`.

################################################################################
# Prepare Packages
################################################################################

# Uncomment the lines below to install the necessary packages if not already installed:
# install.packages("stats")
# install.packages("vars")
# install.packages("ggplot2")

suppressWarnings(suppressMessages({
  library(stats)
  library(vars)
  library(ggplot2)
}))

################################################################################
# Function: Perform VAR with Dynamic PCA Configurations
################################################################################

perform_var_with_pca <- function(num_components, title, output_folder) {
  # Create output folder if it doesn't exist
  if (!dir.exists(output_folder)) {
    dir.create(output_folder)
  }
  
  # Initialize variables for recursive rolling predictions
  Y_train_vec <- as.numeric(Y_train)
  X_train_matrix <- as.matrix(X_train)
  Y_test_vec <- as.numeric(Y_test)
  X_test_matrix <- as.matrix(X_test)
  
  n_test <- length(Y_test_vec)
  predictions <- numeric(n_test)
  
  for (i in 1:n_test) {
    # Step 1: Standardize the current training data
    mean_Y_train <- mean(Y_train_vec)
    sd_Y_train <- sd(Y_train_vec)
    Y_train_standardized <- (Y_train_vec - mean_Y_train) / sd_Y_train
    
    mean_X_train <- colMeans(X_train_matrix)
    sd_X_train <- apply(X_train_matrix, 2, sd)
    X_train_standardized <- scale(X_train_matrix, center = mean_X_train, scale = sd_X_train)
    
    # Step 2: Apply PCA dynamically to the standardized training data
    pca_model <- prcomp(X_train_standardized, center = FALSE, scale. = FALSE)
    X_train_pca <- pca_model$x[, 1:num_components]  # Use specified number of components
    
    # Step 3: Fit the VAR model using the selected principal components
    train_data_standardized <- data.frame(Y = Y_train_standardized, X_train_pca)
    var_model <- VAR(train_data_standardized, p = 1)
    
    # Step 4: Predict using the VAR model
    new_X <- matrix(X_test_matrix[i, ], nrow = 1)  # Ensure new_X is a 1-row matrix
    new_X <- scale(new_X, center = mean_X_train, scale = sd_X_train)
    
    colnames(new_X) <- colnames(X_train_matrix)  # Match column names
    new_X_pca <- predict(pca_model, newdata = as.data.frame(new_X))[, 1:num_components]
    new_test_data <- data.frame(t(new_X_pca))
    colnames(new_test_data) <- colnames(train_data_standardized)[-1]  # Exclude Y column
    
    var_forecast <- predict(var_model, n.ahead = 1)
    prediction_standardized <- var_forecast$fcst$Y[1, "fcst"]
    
    # Step 5: De-standardize the prediction
    prediction <- prediction_standardized * sd_Y_train + mean_Y_train
    predictions[i] <- prediction
    
    # Update training data
    actual_value <- Y_test_vec[i]
    Y_train_vec <- c(Y_train_vec, actual_value)
    X_train_matrix <- rbind(X_train_matrix, X_test_matrix[i, ])
  }
  
  # Metrics
  test_errors <- Y_test_vec - predictions
  test_mae <- mean(abs(test_errors))
  test_mse <- mean(test_errors^2)
  test_rmse <- sqrt(test_mse)
  
  cat("Number of Principal Components:", num_components, "\n")
  cat("MAE:", test_mae, "MSE:", test_mse, "RMSE:", test_rmse, "\n\n")
  
  # Create diagnostic data frame
  df_test <- data.frame(Date = Y_test_with_date$date, Actual = Y_test_vec, Predicted = predictions)
  
  # Time Series Plot
  time_series_plot <- ggplot(df_test, aes(x = Date)) +
    geom_line(aes(y = Actual, color = "Actual")) +
    geom_line(aes(y = Predicted, color = "Predicted")) +
    labs(title = title, x = "Date", y = "CPIULFSL", color = "Legend") +
    theme_minimal() +
    scale_color_manual(values = c("Actual" = "blue", "Predicted" = "red"))
  
  ggsave(file.path(output_folder, paste0("var_", num_components, "_pcs_timeseries.pdf")),
         plot = time_series_plot, width = 10, height = 8, dpi = 300, units = "in")
  
  # Residual Plot
  residual_plot <- ggplot(df_test, aes(x = Actual, y = Actual - Predicted)) +
    geom_point() +
    geom_hline(yintercept = 0, color = "red") +
    ggtitle(paste("VAR Residual Plot (Components =", num_components, ")")) +
    labs(x = "Actual", y = "Residual (Actual - Predicted)") +
    theme_minimal()
  
  ggsave(file.path(output_folder, paste0("var_", num_components, "_pcs_residuals.pdf")),
         plot = residual_plot, width = 10, height = 8, dpi = 300, units = "in")
  
  # Scatter Plot
  scatter_plot <- ggplot(df_test, aes(x = Actual, y = Predicted)) +
    geom_point() +
    geom_abline(intercept = 0, slope = 1, color = "red") +
    ggtitle(paste("VAR: Actual vs Predicted (Components =", num_components, ")")) +
    labs(x = "Actual", y = "Predicted") +
    theme_minimal()
  
  ggsave(file.path(output_folder, paste0("var_", num_components, "_pcs_scatter.pdf")),
         plot = scatter_plot, width = 10, height = 8, dpi = 300, units = "in")
}

################################################################################
# Run VAR for Multiple PCA Configurations
################################################################################

pc_values <- c(3, 5, 10, 15, 20, 25, 30, 35, 40)
output_folder <- "var_pca_results"

for (num_components in pc_values) {
  title <- paste("VAR with", num_components, "Principal Components")
  perform_var_with_pca(num_components, title, output_folder)
}
