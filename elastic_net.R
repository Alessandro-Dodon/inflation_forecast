################################################################################
#                            Elastic Net Forecasting                           #
################################################################################
# Script Name:        elastic_net.R
# Author:             Alessandro Dodon
# Last Modified:      2025-05-07
#
# Description:        Performs recursive one-step-ahead forecasts using Elastic Net 
#                     regression across a grid of lambda and alpha values. Generates 
#                     actual vs predicted plots and a combined MSE vs lambda/alpha plot.
#
# Input Data:         Requires output objects from 'pre_processing.R'
# Output:             Forecast evaluation metrics printed to console;
#                     plots saved as a single PDF file in the relative directory
#
# Usage:              Run 'pre_processing.R' first to prepare the data.
#                     Then run this script to evaluate Elastic Net forecasts 
#                     across all (alpha, lambda) combinations.
#
# Dependencies:       All necessary packages are loaded in 'pre_processing.R'
#
# Further Reference:  See the associated presentation slides and user guide in the GitHub repo.

################################################################################
# Elastic Net Function 
################################################################################

test_elastic_net <- function(lambda_values, alpha_values, title_prefix, output_pdf) {
  # Data frame to store results
  results <- expand.grid(Lambda = lambda_values, Alpha = alpha_values)
  results$MSE <- NA
  
  # Open a PDF device for saving plots
  pdf(output_pdf, width = 10, height = 8)
  
  for (a in alpha_values) {  # Iterate over alpha values
    cat("\n------------------------------------------------------\n")
    cat(sprintf(" Testing Alpha: %.2f", a))
    cat("\n------------------------------------------------------\n")
    
    for (i in seq_along(lambda_values)) {
      lambda <- lambda_values[i]
      
      # Initialize variables for recursive predictions
      Y_train_vec <- as.numeric(Y_train)
      X_train_matrix <- as.matrix(X_train)
      Y_test_vec <- as.numeric(Y_test)
      X_test_matrix <- as.matrix(X_test)
      
      n_test <- length(Y_test_vec)
      predictions <- numeric(n_test)  # Define length for predictions
      
      for (j in 1:n_test) {
        # Standardize current training data
        mean_Y_train <- mean(Y_train_vec)
        sd_Y_train <- sd(Y_train_vec)
        Y_train_standardized <- (Y_train_vec - mean_Y_train) / sd_Y_train
        
        mean_X_train <- colMeans(X_train_matrix)
        sd_X_train <- apply(X_train_matrix, 2, sd)
        X_train_standardized <- scale(X_train_matrix, center = mean_X_train, scale = sd_X_train)
        
        # Fit Elastic Net regression
        model <- glmnet(X_train_standardized, Y_train_standardized, 
                        alpha = a, lambda = lambda, standardize = FALSE)
        
        # Predict
        new_X <- matrix(X_test_matrix[j, ], nrow = 1)
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
      mse <- mean((Y_test_vec - predictions)^2)
      results$MSE[results$Lambda == lambda & results$Alpha == a] <- mse
      
      # Print structured MSE output in scientific notation
      cat(sprintf("| Alpha: %.2f | Lambda: %e | MSE: %e |\n", a, lambda, mse))
      
      # Plot Actual vs Predicted
      df_test <- data.frame(Date = Y_test_with_date$date, Actual = Y_test_vec, Predicted = predictions)
      
      time_series_plot <- ggplot(df_test, aes(x = Date)) +
        geom_line(aes(y = Actual, color = "Actual")) +
        geom_line(aes(y = Predicted, color = "Predicted")) +
        labs(title = paste(title_prefix, "(Alpha =", a, ", Lambda =", format(lambda, scientific = TRUE), ")"),
             x = "Date", y = "CPIULFSL", color = "Legend") +
        theme_minimal() +
        scale_color_manual(values = c("Actual" = "blue", "Predicted" = "red"))
      
      print(time_series_plot)  # Add plot to PDF
    }
  }
  
  # Plot MSE vs Lambda for each Alpha
  mse_plot <- ggplot(results, aes(x = log10(Lambda), y = MSE, color = as.factor(Alpha))) +
    geom_line() +
    geom_point() +
    labs(title = "MSE vs. Lambda for different Alpha values", x = "Log10(Lambda)", y = "MSE", color = "Alpha") +
    theme_minimal()
  
  print(mse_plot)  # Add plot to PDF
  
  # Close PDF device
  dev.off()
  
  # Print final summary of best-performing model
  best_result <- results[which.min(results$MSE), ]
  cat("\n------------------------------------------------------\n")
  cat(" BEST MODEL FOUND: \n")
  cat(sprintf(" Alpha: %.2f | Lambda: %e | Lowest MSE: %e \n", 
              best_result$Alpha, best_result$Lambda, best_result$MSE))
  cat("------------------------------------------------------\n")
  
  return(results)
}

# Define lambda and alpha values
lambda_values <- c(1e-6, 1e-4, 1e-3, 1e-2, 1e-1, 1, 10, 100, 1000, 1000000)
alpha_values <- seq(0, 1, by = 0.1)  # More refined search

# NOTE:
# Due to the configuration of the function and the parameters required (alpha and lambda)
# the code will need more time than lasso and ridge alone. The pdf output will also be
# considerably longer.

# Run the function
results_elastic_net <- test_elastic_net(lambda_values, alpha_values, "Elastic Net Regression", "actual_vs_predicted_elastic.pdf")



