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
# - This script requires preprocessed datasets `train_data_holdout` and 
#   `test_data_holdout`. Ensure these datasets are generated using the 
#   preprocessing script before running this script.

# Packages:
# - Required R packages:
#   `ggplot2`
# - If a required package is missing, uncomment the corresponding `install.packages` 
#   line in the "Prepare Packages" section below and run it to install.

# Outputs:
# - Error metrics (MSE, MAE, RMSE) printed to the console for each configuration.
# - Diagnostic plots saved in the specified folder (`pcr_results`):
#   - Time series plot: Actual vs Predicted for each number of principal components.
#   - Residual plot: Residuals for each number of principal components.
#   - Scatter plot: Actual vs Predicted scatter plot for each number of principal components.

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
  
  # Re-prepare the data
  Y_train_vec <- as.numeric(train_data_holdout[-1, "CPIULFSL"])
  X_train_matrix <- as.matrix(head(train_data_holdout[,-1], -1))
  
  Y_test_vec <- as.numeric(test_data_holdout[-1, "CPIULFSL"])
  X_test_matrix <- as.matrix(head(test_data_holdout[,-1], -1))
  
  n_test <- length(Y_test_vec)
  predictions <- numeric(n_test)
  
  for (i in 1:n_test) {
    # Step 1: Standardize the current training data
    Y_train_standardized <- scale(Y_train_vec)
    X_train_standardized <- scale(X_train_matrix)
    
    # Step 2: Apply PCA
    pca_model <- prcomp(X_train_standardized, center = FALSE, scale. = FALSE)
    X_train_pca <- pca_model$x[, 1:num_components]  # Use specified number of components
    
    # Step 3: Fit the PCR model using the selected principal components
    pcr_model <- lm(Y_train_standardized ~ X_train_pca)
    
    # Step 4: Predict using the PCR model
    new_X <- as.numeric(X_test_matrix[i, ])
    new_X_standardized <- scale(matrix(new_X, nrow = 1), 
                                center = attr(X_train_standardized, "scaled:center"), 
                                scale = attr(X_train_standardized, "scaled:scale"))
    
    # Ensure column names match the training data
    colnames(new_X_standardized) <- colnames(X_train_matrix)
    
    # Predict using the PCA model
    new_X_pca <- predict(pca_model, newdata = as.data.frame(new_X_standardized))[, 1:num_components]
    prediction_standardized <- predict(pcr_model, newdata = data.frame(new_X_pca))
    
    # Step 5: De-standardize the prediction
    prediction <- prediction_standardized * attr(Y_train_standardized, "scaled:scale") + 
      attr(Y_train_standardized, "scaled:center")
    predictions[i] <- prediction
    
    # Update training data
    Y_train_vec <- c(Y_train_vec, Y_test_vec[i])
    X_train_matrix <- rbind(X_train_matrix, as.numeric(X_test_matrix[i, ]))
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
  df_test <- data.frame(Date = test_data_holdout$date[-1][1:n_test],
                        Actual = Y_test_vec,
                        Predicted = predictions)
  
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
perform_pcr(ncol(train_data_holdout) - 1, "PCR with All Principal Components", output_folder)

