################################################################################
# Script Overview: VAR Forecast with Multiple PCA Configurations
################################################################################

# Author: Alessandro Dodon
# Last Update: 12-12-2024
# Description:
# This script implements forecasts with Vector Autoregression (VAR) models 
# using different numbers of PCA-transformed predictors. The focus is on 
# forecasting the variable CPIULFSL (inflation proxy) and evaluating the model's 
# performance through error metrics and visualizations for each configuration.

# Dependencies:
# - This script requires preprocessed datasets `X_train`, `Y_train`, `X_test`, `Y_test`.
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
# Function: Perform VAR with Multiple PCA Configurations
################################################################################

perform_var_with_pca <- function(pc_values, output_folder) {
  # Create output folder if it doesn't exist
  if (!dir.exists(output_folder)) {
    dir.create(output_folder)
  }
  
  # Initialize variables for recursive rolling predictions
  Y_train_vec <- as.numeric(Y_train)
  X_train_matrix <- as.matrix(X_train)
  Y_test_vec <- as.numeric(Y_test)
  X_test_matrix <- as.matrix(X_test)
  
  # Perform PCA once on the training data
  train_pca <- prcomp(X_train_matrix, center = TRUE, scale. = TRUE)
  test_pca <- predict(train_pca, newdata = X_test_matrix)  # Transform test data
  
  # Data frame to store results
  results <- data.frame(PCs = pc_values, MAE = NA, MSE = NA, RMSE = NA)
  
  for (pc in pc_values) {
    cat("Testing VAR with", pc, "PCs\n")
    
    # Select the required number of PCs
    train_pca_matrix <- train_pca$x[, 1:pc]
    test_pca_matrix <- test_pca[, 1:pc]
    
    n_test <- length(Y_test_vec)
    predictions <- numeric(n_test)
    
    for (i in 1:n_test) {
      # Ensure alignment of `Y_train_vec` and `train_pca_matrix`
      rows_to_keep <- min(length(Y_train_vec), nrow(train_pca_matrix))
      Y_train_aligned <- Y_train_vec[1:rows_to_keep]
      train_pca_aligned <- train_pca_matrix[1:rows_to_keep, ]
      
      # Standardize the current training data
      mean_Y_train <- mean(Y_train_aligned)
      sd_Y_train <- sd(Y_train_aligned)
      Y_train_standardized <- (Y_train_aligned - mean_Y_train) / sd_Y_train
      
      mean_train_pca <- colMeans(train_pca_aligned)
      sd_train_pca <- apply(train_pca_aligned, 2, sd)
      train_pca_standardized <- scale(train_pca_aligned, center = mean_train_pca, scale = sd_train_pca)
      
      train_data_standardized <- data.frame(Y = Y_train_standardized, train_pca_standardized)
      
      # Fit the VAR model without additional lagging
      var_model <- VAR(train_data_standardized, p = 1)
      
      # Forecast the next step
      current_test_pca <- matrix(test_pca_matrix[i, ], nrow = 1)
      test_pca_standardized <- scale(current_test_pca, center = mean_train_pca, scale = sd_train_pca)
      var_forecast <- predict(var_model, n.ahead = 1)
      prediction_standardized <- var_forecast$fcst$Y[1, "fcst"]
      
      # De-standardize the prediction
      prediction <- prediction_standardized * sd_Y_train + mean_Y_train
      predictions[i] <- prediction
      
      # Update training data
      actual_value <- Y_test_vec[i]
      Y_train_vec <- c(Y_train_vec, actual_value)
      train_pca_matrix <- rbind(train_pca_matrix, test_pca_matrix[i, ])
    }
    
    # Metrics
    actual_values <- Y_test_vec[1:n_test]
    test_errors <- actual_values - predictions
    test_mae <- mean(abs(test_errors))
    test_mse <- mean(test_errors^2)
    test_rmse <- sqrt(test_mse)
    
    # Store metrics
    results[results$PCs == pc, ] <- c(pc, test_mae, test_mse, test_rmse)
    
    # Print metrics for the current PC configuration
    cat("PCs:", pc, "MAE:", test_mae, "MSE:", test_mse, "RMSE:", test_rmse, "\n")
    
    # Prepare data frame for plotting
    df_test <- data.frame(Date = Y_test_with_date$date, Actual = actual_values, Predicted = predictions)
    
    # Save plots in the specified folder
    base_filename <- file.path(output_folder, paste0("var_pca_", pc, "_pcs"))
    
    # Time series plot
    time_series_plot <- ggplot(df_test, aes(x = Date)) +
      geom_line(aes(y = Actual, color = "Actual")) +
      geom_line(aes(y = Predicted, color = "Predicted")) +
      labs(title = paste("VAR with", pc, "PCs: Actual vs Predicted"),
           x = "Date", y = "CPIULFSL", color = "Legend") +
      theme_minimal() +
      scale_color_manual(values = c("Actual" = "blue", "Predicted" = "red"))
    
    ggsave(paste0(base_filename, "_timeseries.pdf"), plot = time_series_plot, width = 10, height = 8, dpi = 300, units = "in")
    
    # Residual plot
    residual_plot <- ggplot(df_test, aes(x = Actual, y = Actual - Predicted)) +
      geom_point() +
      geom_hline(yintercept = 0, color = "red") +
      ggtitle(paste("VAR Residual Plot (PCs =", pc, ")")) +
      labs(x = "Actual", y = "Residual (Actual - Predicted)") +
      theme_minimal()
    
    ggsave(paste0(base_filename, "_residuals.pdf"), plot = residual_plot, width = 10, height = 8, dpi = 300, units = "in")
    
    # Actual vs Predicted scatter plot
    scatter_plot <- ggplot(df_test, aes(x = Actual, y = Predicted)) +
      geom_point() +
      geom_abline(intercept = 0, slope = 1, color = "red") +
      ggtitle(paste("VAR with", pc, "PCs: Actual vs Predicted")) +
      labs(x = "Actual", y = "Predicted") +
      theme_minimal()
    
    ggsave(paste0(base_filename, "_scatter.pdf"), plot = scatter_plot, width = 10, height = 8, dpi = 300, units = "in")
  }
  
  return(results)
}

################################################################################
# Run VAR with Different PCA Configurations
################################################################################

# Define the number of PCs to test
pc_values <- c(3, 5, 10, 15, 20, 25, 30, 35, 40)
output_folder <- "var_pca_results"
results <- perform_var_with_pca(pc_values, output_folder)

