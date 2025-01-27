################################################################################
# Script Overview: PCR Forecast with Logic Similar to OLS
################################################################################

# Author: Alessandro Dodon
# Last Update: 01-26-2025
# Description:
# This script implements Principal Component Regression (PCR) for forecasting 
# CPIULFSL (an inflation proxy). The PCR approach uses principal components 
# derived from the predictors to mitigate multicollinearity and reduce dimensionality.

# Dependencies:
# - This script requires a preprocessed dataset `transformed_data_cleaned_no_COVID` 
#   already split into X_train, Y_train, X_test, Y_test.
# - The `pls` package is used for PCR implementation.

# Outputs:
# - MSE values for each PCR configuration (different numbers of principal components)
#   are printed in the console.
# - A single PDF file (`actual_vs_predicted_pcr.pdf`) is created containing plots of 
#   actual vs. predicted values for each number of principal components.

# Notes:
# - This script must be run sequentially.
# - Additional clarifications are provided throughout the script with # NOTE: comments.
#   For more details, please consult the related documentation or slides.

################################################################################
# PCR Forecast 
################################################################################

# NOTE:
# The `pls` package simplifies Principal Component Regression by integrating 
# PCA and regression into one step, making it efficient for dimensionality reduction 
# and handling multicollinearity in high-dimensional data.

# Initialize vectors
Y_train_vec <- as.numeric(Y_train)
X_train_matrix <- as.matrix(X_train) # Full matrix of predictors

Y_test_vec <- as.numeric(Y_test)
X_test_matrix <- as.matrix(X_test)
n_test <- length(Y_test_vec)

# Create a function to perform PCR for different numbers of principal components
run_pcr <- function(num_pc_list) {
  # Create a list to store MSEs and plots
  mse_results <- list()
  
  pdf("actual_vs_predicted_pcr.pdf") # Open a single PDF file to store all plots
  
  # Loop over each number of principal components
  for (num_pc in num_pc_list) {
    # Re-initialize training and test data for each configuration
    Y_train_vec <- as.numeric(Y_train)
    X_train_matrix <- as.matrix(X_train)
    
    # NOTE:
    # Re-initializing training and test data ensures each PCR configuration starts independently,
    # avoiding data contamination from prior iterations.
    
    # Create vectors to store predictions
    predictions <- numeric(n_test)
    
    for (i in 1:n_test) {
      # Standardize the current training data
      mean_Y_train <- mean(Y_train_vec)
      sd_Y_train <- sd(Y_train_vec)
      Y_train_standardized <- (Y_train_vec - mean_Y_train) / sd_Y_train
      
      mean_X_train <- colMeans(X_train_matrix)
      sd_X_train <- apply(X_train_matrix, 2, sd)
      X_train_standardized <- scale(X_train_matrix, center = mean_X_train, scale = sd_X_train)
      
      # Fit the PCR model using the standardized training data
      pcr_model <- pcr(Y_train_standardized ~ ., data = as.data.frame(X_train_standardized), ncomp = num_pc, scale = FALSE)
      
      # Use the corresponding row from the test set for prediction
      new_X <- scale(X_test_matrix[i, , drop = FALSE], center = mean_X_train, scale = sd_X_train)
      new_X <- as.data.frame(new_X)  # Convert to a data frame to match model input
      colnames(new_X) <- colnames(X_train_standardized)  # Ensure column names match
      
      # NOTE:
      # The `pls` package automatically applies PCA to new_X using the PCA loadings 
      # computed during training, ensuring consistency between training and prediction.
      
      # Predict using the PCR model
      prediction_standardized <- predict(pcr_model, newdata = new_X, ncomp = num_pc)
      
      # De-standardize the prediction
      prediction <- prediction_standardized * sd_Y_train + mean_Y_train
      predictions[i] <- prediction
      
      # Append the actual test value to the training data
      actual_value <- Y_test_vec[i]
      Y_train_vec <- c(Y_train_vec, actual_value)
      X_train_matrix <- rbind(X_train_matrix, X_test_matrix[i, ])
    }
    
    # Calculate prediction errors for the test set
    test_errors <- Y_test_vec - predictions
    test_mse <- mean(test_errors^2)
    cat("Number of Components:", num_pc, "| Test Mean Squared Error (MSE):", test_mse, "\n")
    
    # Store the MSE result
    mse_results[[paste0("PC", num_pc)]] <- test_mse
    
    # Prepare data frame for visualization
    df_test <- data.frame(Date = Y_test_with_date$date, Actual = Y_test_vec, Predicted = predictions)
    
    # Plot: Actual vs Predicted Time Series
    plot <- ggplot(df_test, aes(x = Date)) +
      geom_line(aes(y = Actual, color = "Actual")) +
      geom_line(aes(y = Predicted, color = "Predicted")) +
      labs(title = paste("PCR with", num_pc, "Components: Actual vs Predicted Time Series"),
           x = "Date", y = "CPIULFSL", color = "Legend") +
      theme_minimal() +
      scale_color_manual(values = c("Actual" = "blue", "Predicted" = "red"))
    
    # Add the plot to the PDF file
    print(plot)
  }
  
  dev.off() # Close the PDF file
  
  return(mse_results)
}

# Run the PCR function for different numbers of components
num_pc_list <- c(1, 2, 3, 5, 10, 25, 50, 75, 121)
mse_results <- run_pcr(num_pc_list)
