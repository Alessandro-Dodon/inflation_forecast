################################################################################
# Script Overview: Ridge Forecast with Multiple Lambda Values
################################################################################

# Author: Alessandro Dodon
# Last Update: 12-12-2024
# Description:
# This script evaluates the performance of Ridge regression models using various 
# lambda values. Recursive predictions are made for the variable CPIULFSL 
# (inflation proxy) over the test dataset. Evaluation metrics (MSE, MAE, RMSE) 
# are calculated for each lambda, and diagnostic plots (Actual vs Predicted, 
# Residual plots, Scatter plots) are generated. The script saves all outputs 
# to a specified folder.

# Dependencies:
# - This script requires preprocessed datasets `train_data_holdout` and 
#   `test_data_holdout`. Ensure these datasets are generated using the 
#   preprocessing script before running this script.

# Packages:
# - Required R packages:
#   `glmnet`, `ggplot2`
# - If a required package is missing, uncomment the corresponding `install.packages` 
#   line in the "Prepare Packages" section below and run it to install.

# Outputs:
# - Error metrics (MSE, MAE, RMSE) printed to the console for each lambda value.
# - Diagnostic plots saved in the specified folder (`ridge_results_folder`):
#   - Time series plot: Actual vs Predicted for each lambda.
#   - Residual plot: Residuals for each lambda.
#   - Scatter plot: Actual vs Predicted scatter plot for each lambda.
#   - MSE vs Lambda plot: Comparison of MSE values across lambdas.

################################################################################
# Prepare Packages
################################################################################

# Uncomment the lines below to install the necessary packages if not already installed:
# install.packages("glmnet")
# install.packages("ggplot2")

# Load required libraries
suppressWarnings(suppressMessages({
  library(glmnet)
  library(ggplot2)
}))

################################################################################
# Ridge 
################################################################################

# Function to test multiple lambdas, save plots to a folder, and print all metrics
test_ridge_lambdas <- function(lambda_values, title_prefix, output_folder) {
  # Create output folder if it doesn't exist
  if (!dir.exists(output_folder)) {
    dir.create(output_folder)
  }
  
  # Data frame to store results
  results <- data.frame(Lambda = lambda_values, MSE = NA, MAE = NA, RMSE = NA)
  
  for (i in seq_along(lambda_values)) {
    lambda <- lambda_values[i]
    cat("Testing Lambda:", format(lambda, scientific = TRUE), "\n")
    
    # Run Ridge regression for this lambda
    Y_train_vec <- as.numeric(train_data_holdout[-1, "CPIULFSL"])
    X_train_matrix <- as.matrix(head(train_data_holdout[,-1], -1))
    
    Y_test_vec <- as.numeric(test_data_holdout[-1, "CPIULFSL"])
    X_test_matrix <- as.matrix(head(test_data_holdout[,-1], -1))
    
    n_test <- length(Y_test_vec)
    predictions <- numeric(n_test)
    
    for (j in 1:n_test) {
      # Standardize the current training data
      Y_train_standardized <- scale(Y_train_vec)
      X_train_standardized <- scale(X_train_matrix)
      
      # Fit Ridge regression
      model <- glmnet(X_train_standardized, Y_train_standardized, alpha = 0, lambda = lambda, standardize = FALSE)
      
      # Predict
      new_X <- scale(matrix(as.numeric(X_test_matrix[j, ]), nrow = 1), 
                     center = attr(X_train_standardized, "scaled:center"), 
                     scale = attr(X_train_standardized, "scaled:scale"))
      prediction_standardized <- predict(model, newx = new_X)
      
      # De-standardize
      prediction <- prediction_standardized * attr(Y_train_standardized, "scaled:scale") + 
        attr(Y_train_standardized, "scaled:center")
      predictions[j] <- prediction
      
      # Update training data
      Y_train_vec <- c(Y_train_vec, Y_test_vec[j])
      X_train_matrix <- rbind(X_train_matrix, as.numeric(X_test_matrix[j, ]))
    }
    
    # Calculate metrics
    test_errors <- Y_test_vec - predictions
    mse <- mean(test_errors^2)
    mae <- mean(abs(test_errors))
    rmse <- sqrt(mse)
    results$MSE[i] <- mse
    results$MAE[i] <- mae
    results$RMSE[i] <- rmse
    
    # Print metrics for the current lambda
    cat(sprintf("Lambda: %s, MSE: %s, MAE: %s, RMSE: %s\n", 
                format(lambda, scientific = TRUE),
                format(mse, scientific = TRUE),
                format(mae, scientific = TRUE),
                format(rmse, scientific = TRUE)))
    
    # Plot Actual vs Predicted for this lambda
    df_test <- data.frame(Date = test_data_holdout$date[-1][1:n_test],
                          Actual = Y_test_vec,
                          Predicted = predictions)
    
    time_series_plot <- ggplot(df_test, aes(x = Date)) +
      geom_line(aes(y = Actual, color = "Actual")) +
      geom_line(aes(y = Predicted, color = "Predicted")) +
      labs(title = paste(title_prefix, "(Lambda =", format(lambda, scientific = TRUE), ")"),
           x = "Date", y = "CPIULFSL", color = "Legend") +
      theme_minimal() +
      scale_color_manual(values = c("Actual" = "blue", "Predicted" = "red"))
    
    ggsave(file.path(output_folder, paste0("actual_vs_predicted_lambda_", format(lambda, scientific = TRUE), ".pdf")), 
           plot = time_series_plot, width = 10, height = 8, dpi = 300, units = "in")
    
    # Residual plot
    residual_plot <- ggplot(df_test, aes(x = Actual, y = Actual - Predicted)) +
      geom_point() +
      geom_hline(yintercept = 0, color = "red") +
      ggtitle(paste("Ridge Regression: Residual Plot (Lambda =", format(lambda, scientific = TRUE), ")")) +
      labs(x = "Actual", y = "Residual (Actual - Predicted)") +
      theme_minimal()
    
    ggsave(file.path(output_folder, paste0("residuals_lambda_", format(lambda, scientific = TRUE), ".pdf")), 
           plot = residual_plot, width = 10, height = 8, dpi = 300, units = "in")
    
    # Actual vs Predicted scatter plot
    scatter_plot <- ggplot(df_test, aes(x = Actual, y = Predicted)) +
      geom_point() +
      geom_abline(intercept = 0, slope = 1, color = "red") +
      ggtitle(paste("Ridge Regression: Actual vs Predicted (Lambda =", format(lambda, scientific = TRUE), ")")) +
      labs(x = "Actual", y = "Predicted") +
      theme_minimal()
    
    ggsave(file.path(output_folder, paste0("scatter_lambda_", format(lambda, scientific = TRUE), ".pdf")), 
           plot = scatter_plot, width = 10, height = 8, dpi = 300, units = "in")
  }
  
  # Plot MSE vs Lambda
  mse_plot <- ggplot(results, aes(x = log10(Lambda), y = MSE)) +
    geom_line() +
    geom_point() +
    labs(title = "MSE vs. Lambda", x = "Log10(Lambda)", y = "MSE") +
    theme_minimal()
  
  ggsave(file.path(output_folder, "mse_vs_lambda_plot.pdf"), plot = mse_plot, width = 10, height = 8, dpi = 300, units = "in")
  
  return(results)
}

# Test multiple lambdas and save plots in a folder
lambda_values <- c(1e-6, 1e-4, 1e-3, 1e-2, 1e-1, 1, 10, 100, 1000, 1000000, 121 / sqrt(479), 121 / sqrt(718))
results <- test_ridge_lambdas(lambda_values, "Ridge Regression", "ridge_results")
