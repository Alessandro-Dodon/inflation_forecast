################################################################################
# Script Overview: Ridge Forecast with Multiple Lambda Values
################################################################################

# Author: Alessandro Dodon
# Last Update: 01-25-2025
# Description:
# This script implements Ridge regression for CPIULFSL (inflation proxy) using 
# multiple lambdas to find the optimal regularization parameter.

# Dependencies:
# - This script requires preprocessed dataset `transformed_data_cleaned_no_COVID` 
#   already split into X_train, Y_train, X_test, Y_test.

# Outputs:
# - MSE values are printed in the console.
# - A single PDF file (`actual_vs_predicted_ridge.pdf`) is saved in the same path 
#   as the script, containing:
#     - Actual vs. Predicted Time Series for each lambda.
#     - MSE vs. Lambda plot.

# Notes:
# - This script must be run sequentially.
# - Additional clarifications are provided throughout the script with # NOTE: comments.
#   For a better explanation, please refer to related slides or documentation.

################################################################################
# Ridge 
################################################################################

# Function to test multiple lambdas, save plots to a single PDF, and calculate MSE
test_ridge_lambdas <- function(lambda_values, title_prefix, output_pdf) {
  # Data frame to store results
  results <- data.frame(Lambda = lambda_values, MSE = NA)
  
  # Open a PDF device for saving all plots sequentially
  pdf(output_pdf, width = 10, height = 8)
  
  for (i in seq_along(lambda_values)) {
    lambda <- lambda_values[i]
    cat("Testing Lambda:", format(lambda, scientific = TRUE), "\n")
    
    # Initialize variables for recursive predictions
    Y_train_vec <- as.numeric(Y_train)
    X_train_matrix <- as.matrix(X_train)
    Y_test_vec <- as.numeric(Y_test)
    X_test_matrix <- as.matrix(X_test)
    
    n_test <- length(Y_test_vec)
    predictions <- numeric(n_test)
    
    for (j in 1:n_test) {
      # Standardize the current training data
      mean_Y_train <- mean(Y_train_vec)
      sd_Y_train <- sd(Y_train_vec)
      Y_train_standardized <- (Y_train_vec - mean_Y_train) / sd_Y_train
      
      mean_X_train <- colMeans(X_train_matrix)
      sd_X_train <- apply(X_train_matrix, 2, sd)
      X_train_standardized <- scale(X_train_matrix, center = mean_X_train, scale = sd_X_train)
      
      # Fit Ridge regression
      model <- glmnet(X_train_standardized, Y_train_standardized, alpha = 0, lambda = lambda, standardize = FALSE)
      
      # Predict
      new_X <- matrix(X_test_matrix[j, ], nrow = 1)  # Ensure new_X is a 1-row matrix
      new_X <- scale(new_X, center = mean_X_train, scale = sd_X_train)
      prediction_standardized <- predict(model, newx = new_X)
      
      # NOTE:
      # For `glmnet`, only the column order in `newx` must match the training matrix. 
      # Column names are optional, unlike `lm`, which requires both matching names and order.
      
      # De-standardize
      prediction <- prediction_standardized * sd_Y_train + mean_Y_train
      predictions[j] <- prediction
      
      # Update training data
      actual_value <- Y_test_vec[j]
      Y_train_vec <- c(Y_train_vec, actual_value)
      X_train_matrix <- rbind(X_train_matrix, X_test_matrix[j, ])
    }
    
    # Calculate MSE
    test_errors <- Y_test_vec - predictions
    mse <- mean(test_errors^2)
    results$MSE[i] <- mse
    
    # Print MSE for the current lambda
    cat(sprintf("Lambda: %s, MSE: %s\n", format(lambda, scientific = TRUE), format(mse, scientific = TRUE)))
    
    # Plot Actual vs Predicted for this lambda
    df_test <- data.frame(Date = Y_test_with_date$date, Actual = Y_test_vec, Predicted = predictions)
    
    # Time series plot
    time_series_plot <- ggplot(df_test, aes(x = Date)) +
      geom_line(aes(y = Actual, color = "Actual")) +
      geom_line(aes(y = Predicted, color = "Predicted")) +
      labs(title = paste(title_prefix, "(Lambda =", format(lambda, scientific = TRUE), ")"),
           x = "Date", y = "CPIULFSL", color = "Legend") +
      theme_minimal() +
      scale_color_manual(values = c("Actual" = "blue", "Predicted" = "red"))
    
    print(time_series_plot)  # Add plot to the PDF
  }
  
  # Plot MSE vs Lambda
  mse_plot <- ggplot(results, aes(x = log10(Lambda), y = MSE)) +
    geom_line() +
    geom_point() +
    labs(title = "MSE vs. Lambda", x = "Log10(Lambda)", y = "MSE") +
    theme_minimal()
  
  print(mse_plot)  # Add plot to the PDF
  
  # Close the PDF device
  dev.off()
  
  return(results)
}

# Test multiple lambdas and save plots in a single PDF file
lambda_values <- c(1e-6, 1e-4, 1e-3, 1e-2, 1e-1, 0, 1, 10, 100, 1000, 1000000, 121 / sqrt(480), 121 / sqrt(719))
results <- test_ridge_lambdas(lambda_values, "Ridge Regression", "actual_vs_predicted_ridge.pdf")

