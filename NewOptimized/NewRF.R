################################################################################
# Script Overview: Random Forest Forecast
################################################################################

# Author: Alessandro Dodon
# Last Update: 12-12-2024
# Description:
# This script implements Random Forest (RF) regression to forecast the variable 
# CPIULFSL (inflation proxy) using either all predictors or only past values of 
# CPIULFSL. Recursive predictions are made for the test dataset, and evaluation 
# metrics (MAE, MSE, RMSE) are calculated. Diagnostic plots (Actual vs Predicted, 
# Residual plots, and Scatter plots) are generated and saved for each configuration.

# Dependencies:
# - This script requires preprocessed datasets `train_data_holdout` and 
#   `test_data_holdout`. Ensure these datasets are generated using the 
#   preprocessing script before running this script.

# Packages:
# - Required R packages:
#   `randomForest`, `ggplot2`
# - If a required package is missing, uncomment the corresponding `install.packages` 
#   line in the "Prepare Packages" section below and run it to install.

# Outputs:
# - Error metrics (MSE, MAE, RMSE) printed to the console for each configuration.
# - Diagnostic plots saved in the specified folder (`rf_results_folder`):
#   - Time series plot: Actual vs Predicted for each RF configuration.
#   - Residual plot: Residuals for each RF configuration.
#   - Scatter plot: Actual vs Predicted scatter plot for each RF configuration.

################################################################################
# Prepare Packages
################################################################################

# Uncomment the lines below to install the necessary packages if not already installed:
# install.packages("randomForest")
# install.packages("ggplot2")

# Load required libraries
suppressWarnings(suppressMessages({
  library(randomForest)
  library(ggplot2)
}))

################################################################################
# RF 
################################################################################

# Function to perform RF and save plots in a folder
perform_rf <- function(ntree, maxnodes = NULL, use_all_predictors = TRUE, title, filename, output_folder) {
  # Create output folder if it doesn't exist
  if (!dir.exists(output_folder)) {
    dir.create(output_folder)
  }
  
  # Set seed for reproducibility
  set.seed(123)
  
  # Initial shifting for training data
  Y_train_vec <- as.numeric(train_data_holdout[-1, "CPIULFSL"])
  X_train_matrix <- as.matrix(head(train_data_holdout[,-1], -1))
  
  Y_test_vec <- as.numeric(test_data_holdout[-1, "CPIULFSL"])
  X_test_matrix <- as.matrix(head(test_data_holdout[,-1], -1))
  
  n_test <- length(Y_test_vec)
  predictions <- numeric(n_test)
  
  for (i in 1:n_test) {
    # Prepare predictors
    if (use_all_predictors) {
      X_train_rf <- as.data.frame(X_train_matrix)
      new_X <- as.data.frame(matrix(X_test_matrix[i, ], nrow = 1))  # All predictors
    } else {
      X_train_rf <- as.data.frame(X_train_matrix[, "CPIULFSL", drop = FALSE])  # Only past values
      new_X <- as.data.frame(matrix(X_test_matrix[i, "CPIULFSL", drop = FALSE], nrow = 1))
    }
    
    colnames(new_X) <- colnames(X_train_rf)  # Ensure column names match training data
    
    # Train RF model
    rf_model <- randomForest(X_train_rf, Y_train_vec, ntree = ntree, maxnodes = maxnodes)
    
    # Predict
    predictions[i] <- predict(rf_model, newdata = new_X)
    
    # Update training data
    Y_train_vec <- c(Y_train_vec, Y_test_vec[i])
    X_train_matrix <- rbind(X_train_matrix, X_test_matrix[i, ])
  }
  
  # Metrics
  test_errors <- Y_test_vec - predictions
  test_mae <- mean(abs(test_errors))
  test_mse <- mean(test_errors^2)
  test_rmse <- sqrt(test_mse)
  
  cat("Number of Trees:", ntree, "\n")
  if (!is.null(maxnodes)) {
    cat("Max Depth:", maxnodes, "\n")
  } else {
    cat("Max Depth: Unlimited\n")
  }
  cat("Test MAE:", test_mae, "\n")
  cat("Test MSE:", test_mse, "\n")
  cat("Test RMSE:", test_rmse, "\n\n")
  
  # Diagnostic plots
  df_test <- data.frame(Date = test_data_holdout$date[1:n_test],
                        Actual = Y_test_vec,
                        Predicted = predictions)
  
  # Time series plot
  time_series_plot <- ggplot(df_test, aes(x = Date)) +
    geom_line(aes(y = Actual, color = "Actual")) +
    geom_line(aes(y = Predicted, color = "Predicted")) +
    labs(title = title, x = "Date", y = "CPIULFSL", color = "Legend") +
    theme_minimal() +
    scale_color_manual(values = c("Actual" = "blue", "Predicted" = "red"))
  ggsave(file.path(output_folder, filename), plot = time_series_plot, width = 10, height = 8, dpi = 300, units = "in")
  
  # Residual plot
  residual_plot <- ggplot(df_test, aes(x = Actual, y = Actual - Predicted)) +
    geom_point() +
    geom_hline(yintercept = 0, color = "red") +
    ggtitle(paste("RF Residual Plot (ntree =", ntree, ")")) +
    labs(x = "Actual", y = "Residual (Actual - Predicted)") +
    theme_minimal()
  ggsave(file.path(output_folder, sub(".pdf", "_residuals.pdf", filename)), plot = residual_plot, width = 10, height = 8, dpi = 300, units = "in")
  
  # Actual vs Predicted scatter plot
  scatter_plot <- ggplot(df_test, aes(x = Actual, y = Predicted)) +
    geom_point() +
    geom_abline(intercept = 0, slope = 1, color = "red") +
    ggtitle(paste("RF Actual vs Predicted (ntree =", ntree, ")")) +
    labs(x = "Actual", y = "Predicted") +
    theme_minimal()
  ggsave(file.path(output_folder, sub(".pdf", "_scatter.pdf", filename)), plot = scatter_plot, width = 10, height = 8, dpi = 300, units = "in")
}

################################################################################
# Configurations Ordered from Fastest to Slowest
################################################################################

# Small trees, shallow depth
perform_rf(ntree = 10, maxnodes = 5, use_all_predictors = FALSE, 
           title = "RF with Past Value Only (10 Trees, Max Depth 5)", 
           filename = "rf_past_value_10_trees_5_depth.pdf", 
           output_folder = "rf_results")

perform_rf(ntree = 10, maxnodes = 5, use_all_predictors = TRUE, 
           title = "RF with All Predictors (10 Trees, Max Depth 5)", 
           filename = "rf_all_predictors_10_trees_5_depth.pdf", 
           output_folder = "rf_results")

# Small trees, unlimited depth
perform_rf(ntree = 10, maxnodes = NULL, use_all_predictors = FALSE, 
           title = "RF with Past Value Only (10 Trees, Unlimited Depth)", 
           filename = "rf_past_value_10_trees_unlimited_depth.pdf", 
           output_folder = "rf_results")

# Medium trees, moderate depth
perform_rf(ntree = 50, maxnodes = 10, use_all_predictors = TRUE, 
           title = "RF with All Predictors (50 Trees, Max Depth 10)", 
           filename = "rf_all_predictors_50_trees_10_depth.pdf", 
           output_folder = "rf_results")

perform_rf(ntree = 50, maxnodes = 10, use_all_predictors = FALSE, 
           title = "RF with Past Value Only (50 Trees, Max Depth 10)", 
           filename = "rf_past_value_50_trees_10_depth.pdf", 
           output_folder = "rf_results")

# Large trees, moderate depth
perform_rf(ntree = 200, maxnodes = 20, use_all_predictors = TRUE, 
           title = "RF with All Predictors (200 Trees, Max Depth 20)", 
           filename = "rf_all_predictors_200_trees_20_depth.pdf", 
           output_folder = "rf_results")

# Large trees, unlimited depth
perform_rf(ntree = 500, maxnodes = NULL, use_all_predictors = FALSE, 
           title = "RF with Past Value Only (500 Trees, Unlimited Depth)", 
           filename = "rf_past_value_500_trees_unlimited_depth.pdf", 
           output_folder = "rf_results")

perform_rf(ntree = 500, maxnodes = NULL, use_all_predictors = TRUE, 
           title = "RF with All Predictors (500 Trees, Unlimited Depth)", 
           filename = "rf_all_predictors_500_trees_unlimited_depth.pdf", 
           output_folder = "rf_results")

# Very large trees
perform_rf(ntree = 1000, maxnodes = NULL, use_all_predictors = TRUE, 
           title = "RF with All Predictors (1000 Trees, Unlimited Depth)", 
           filename = "rf_all_predictors_1000_trees_unlimited_depth.pdf", 
           output_folder = "rf_results")


