################################################################################
# Script Overview: Random Forest Forecast
################################################################################

# Author: Alessandro Dodon
# Last Update: 01-26-2025
# Description:
# This script implements a Random Forest (RF) regression approach for forecasting 
# CPIULFSL (inflation proxy) using all available predictors in a multivariate setup. 
# Recursive predictions are made for the test dataset with multiple setups 
# varying the number of trees (`ntree`) and maximum tree depth (`maxnodes`).

# Dependencies:
# - This script requires the preprocessed dataset `transformed_data_cleaned_no_COVID`, 
#   which must already be split into X_train, Y_train, X_test, and Y_test.

# Outputs:
# - MSE values are printed in the console for each configuration.
# - A single PDF file, `actual_vs_predicted_rf.pdf`, contains time series plots of 
#   actual vs. predicted values for each Random Forest configuration.

# Notes:
# - This script must be run sequentially.
# - Additional clarifications are provided throughout the script with # NOTE: comments.
#   For a better explanation, please visit the slides.

################################################################################
# RF Function
################################################################################

perform_rf <- function(ntree, maxnodes = NULL, title) {
  # Initialize variables for recursive predictions
  Y_train_vec <- as.numeric(Y_train)
  X_train_matrix <- as.matrix(X_train)
  Y_test_vec <- as.numeric(Y_test)
  X_test_matrix <- as.matrix(X_test)
  n_test <- length(Y_test_vec)
  
  # Create vectors to store predictions
  predictions <- numeric(n_test)
  
  for (i in 1:n_test) {
    # Prepare predictors
    X_train_rf <- as.data.frame(X_train_matrix)
    
    # NOTE:
    # RF doesn't need standardization like the other models as it is scale invariant.
    
    # Train RF model
    set.seed(123)  # Ensures reproducibility, RF is stochastic 
    rf_model <- randomForest(X_train_rf, Y_train_vec, ntree = ntree, maxnodes = maxnodes)
    
    # Use the corresponding value from the test set for prediction
    new_X <- as.data.frame(matrix(X_test_matrix[i, ], nrow = 1))
    colnames(new_X) <- colnames(X_train_rf)
    
    # Predict
    predictions[i] <- predict(rf_model, newdata = new_X)
    
    # Update training data
    Y_train_vec <- c(Y_train_vec, Y_test_vec[i])
    X_train_matrix <- rbind(X_train_matrix, X_test_matrix[i, ])
  }
  
  # Metrics
  test_errors <- Y_test_vec - predictions
  test_mse <- mean(test_errors^2)
  
  cat("Number of Trees:", ntree, "\n")
  if (!is.null(maxnodes)) {
    cat("Max Depth:", maxnodes, "\n")
  } else {
    cat("Max Depth: Unlimited\n")
  }
  cat("Test MSE:", test_mse, "\n\n")
  
  # Prepare time series plot data
  df_test <- data.frame(Date = Y_test_with_date$date[1:n_test],
                        Actual = Y_test_vec,
                        Predicted = predictions)
  
  # Time series plot
  time_series_plot <- ggplot(df_test, aes(x = Date)) +
    geom_line(aes(y = Actual, color = "Actual")) +
    geom_line(aes(y = Predicted, color = "Predicted")) +
    labs(title = title, x = "Date", y = "CPIULFSL", color = "Legend") +
    theme_minimal() +
    scale_color_manual(values = c("Actual" = "blue", "Predicted" = "red"))
  
  return(time_series_plot)
}

################################################################################
# Configurations and PDF Output
################################################################################

# Open a PDF file to save all plots
pdf("actual_vs_predicted_rf.pdf", width = 10, height = 8)

# Generate plots for different configurations
plots <- list(
  perform_rf(ntree = 10, maxnodes = 5, title = "RF (10 Trees, Max Depth 5)"),
  perform_rf(ntree = 25, maxnodes = 8, title = "RF (25 Trees, Max Depth 8)"),
  perform_rf(ntree = 50, maxnodes = 10, title = "RF (50 Trees, Max Depth 10)"),
  perform_rf(ntree = 75, maxnodes = 12, title = "RF (75 Trees, Max Depth 12)"),
  perform_rf(ntree = 100, maxnodes = 15, title = "RF (100 Trees, Max Depth 15)"),
  perform_rf(ntree = 150, maxnodes = 18, title = "RF (150 Trees, Max Depth 18)"),
  perform_rf(ntree = 200, maxnodes = 20, title = "RF (200 Trees, Max Depth 20)"),
  perform_rf(ntree = 500, maxnodes = 25, title = "RF (500 Trees, Max Depth 25)")
)

# NOTE:
# The configurations vary the number of trees (`ntree`) and the maximum depth of 
# each tree (`maxnodes`). Increasing `ntree` generally improves accuracy by reducing 
# variance but adds computational cost. Increasing `maxnodes` allows for deeper 
# trees, capturing more complex patterns, but may risk overfitting if set too high. 
# Results typically improve up to a point, after which gains diminish or stabilize.
# Running this code chunk may take a very long time (more than 30m or so, depending
# on your computer).

# Print each plot to the PDF
for (plot in plots) {
  print(plot)
}

# Close the PDF
dev.off()
