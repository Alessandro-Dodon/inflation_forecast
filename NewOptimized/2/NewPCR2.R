################################################################################
# Script Overview: Principal Component Regression (PCR) Forecast with Different PC 
################################################################################

# Author: Alessandro Dodon
# Last Update: 12-12-2024
# Description:
# This script implements Principal Component Regression (PCR) to forecast the 
# variable CPIULFSL (inflation proxy) using different numbers of principal components. 
# Recursive predictions are made for the test dataset, and evaluation metrics 
# (MAE, MSE, RMSE) are calculated. Diagnostic plots (Actual vs Predicted, Residuals, 
# and Scatter plots) are generated and saved for each configuration.

# Dependencies:
# - This script requires preprocessed dataset `transformed_data_cleaned_no_COVID` 
#   already split into X_train, Y_train, X_test, Y_test.
# - Packages required: `ggplot2`

################################################################################
# Prepare Packages
################################################################################

# Uncomment the lines below to install the necessary packages if not already installed:
# install.packages("ggplot2")

# Load required libraries
suppressWarnings(suppressMessages({
  library(ggplot2)
}))

################################################################################
# PCR 
################################################################################

# Function to perform PCR with additional diagnostic plots and save in a folder
perform_pcr <- function(num_components, title, output_folder) {
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
    
    # Step 2: Apply PCA
    pca_model <- prcomp(X_train_standardized, center = FALSE, scale. = FALSE)
    X_train_pca <- pca_model$x[, 1:num_components]  # Use specified number of components
    
    # Step 3: Fit the PCR model using the selected principal components
    pcr_model <- lm(Y_train_standardized ~ X_train_pca)
    
    # Step 4: Predict using the PCR model
    new_X <- matrix(X_test_matrix[i, ], nrow = 1)  # Ensure new_X is a 1-row matrix
    new_X <- scale(new_X, center = mean_X_train, scale = sd_X_train)
    
    # Add column names to match X_train
    colnames(new_X) <- colnames(X_train_matrix)
    
    new_X_pca <- predict(pca_model, newdata = as.data.frame(new_X))[, 1:num_components]
    prediction_standardized <- predict(pcr_model, newdata = data.frame(new_X_pca))
    
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
  
  # Print metrics
  cat("Number of Principal Components:", num_components, "\n")
  cat("Test MAE:", test_mae, "\n")
  cat("Test MSE:", test_mse, "\n")
  cat("Test RMSE:", test_rmse, "\n\n")
  
  # Create diagnostic data frame
  df_test <- data.frame(Date = Y_test_with_date$date, Actual = Y_test_vec, Predicted = predictions)
  
  # Save plots in the specified folder
  base_filename <- file.path(output_folder, paste0("pcr_", num_components, "_components"))
  
  # Time series plot
  time_series_plot <- ggplot(df_test, aes(x = Date)) +
    geom_line(aes(y = Actual, color = "Actual")) +
    geom_line(aes(y = Predicted, color = "Predicted")) +
    labs(title = title, x = "Date", y = "CPIULFSL", color = "Legend") +
    theme_minimal() +
    scale_color_manual(values = c("Actual" = "blue", "Predicted" = "red"))
  
  ggsave(paste0(base_filename, "_timeseries.pdf"), plot = time_series_plot, width = 10, height = 8, dpi = 300, units = "in")
  
  # Residual plot
  residual_plot <- ggplot(df_test, aes(x = Actual, y = Actual - Predicted)) +
    geom_point() +
    geom_hline(yintercept = 0, color = "red") +
    ggtitle(paste("PCR Residual Plot (Components =", num_components, ")")) +
    labs(x = "Actual", y = "Residual (Actual - Predicted)") +
    theme_minimal()
  
  ggsave(paste0(base_filename, "_residuals.pdf"), plot = residual_plot, width = 10, height = 8, dpi = 300, units = "in")
  
  # Actual vs Predicted scatter plot
  scatter_plot <- ggplot(df_test, aes(x = Actual, y = Predicted)) +
    geom_point() +
    geom_abline(intercept = 0, slope = 1, color = "red") +
    ggtitle(paste("PCR: Actual vs Predicted (Components =", num_components, ")")) +
    labs(x = "Actual", y = "Predicted") +
    theme_minimal()
  
  ggsave(paste0(base_filename, "_scatter.pdf"), plot = scatter_plot, width = 10, height = 8, dpi = 300, units = "in")
}

# Call the function for different numbers of principal components and save to folder
output_folder <- "pcr_results"
perform_pcr(1, "PCR with 1 Principal Component", output_folder)
perform_pcr(2, "PCR with 2 Principal Components", output_folder)
perform_pcr(5, "PCR with 5 Principal Components", output_folder)
perform_pcr(ncol(X_train), "PCR with All Principal Components", output_folder)
