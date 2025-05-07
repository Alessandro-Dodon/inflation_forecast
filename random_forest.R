################################################################################
#                             Random Forest Forecasting                        #
################################################################################
# Script Name:        random_forest.R
# Author:             Alessandro Dodon
# Last Modified:      2025-05-07
#
# Description:        Performs recursive one-step-ahead forecasts using Random Forest 
#                     with multiple configurations of tree count and maximum depth. 
#                     Saves actual vs predicted plots for each configuration.
#
# Input Data:         Requires output objects from 'pre_processing.R'
# Output:             Forecast evaluation metrics printed to console;
#                     all plots saved in a single PDF file in the relative directory
#
# Usage:              Run 'pre_processing.R' first to prepare the data.
#                     Then run this script to evaluate RF forecasts using varying
#                     combinations of number of trees and max nodes.
#
# Dependencies:       All necessary packages are loaded in 'pre_processing.R'
#
# Further Reference:  See the associated presentation slides and user guide in the GitHub repo.

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
  perform_rf(ntree = 10, maxnodes = 5, title = "RF (10 Trees, Max Nodes 5)"),
  perform_rf(ntree = 25, maxnodes = 8, title = "RF (25 Trees, Max Nodes 8)"),
  perform_rf(ntree = 50, maxnodes = 10, title = "RF (50 Trees, Max Nodes 10)"),
  perform_rf(ntree = 75, maxnodes = 12, title = "RF (75 Trees, Max Nodes 12)"),
  perform_rf(ntree = 100, maxnodes = 15, title = "RF (100 Trees, Max Nodes 15)"),
  perform_rf(ntree = 150, maxnodes = 18, title = "RF (150 Trees, Max Nodes 18)"),
  perform_rf(ntree = 200, maxnodes = 20, title = "RF (200 Trees, Max Nodes 20)"),
  perform_rf(ntree = 500, maxnodes = 25, title = "RF (500 Trees, Max Nodes 25)")
)

# NOTE:
# Due to the different settings and computational power required for 200 or 500 trees
# running this code chunk may take a very long time (more than 30m or so, depending
# on your machine).

# Print each plot to the PDF
for (plot in plots) {
  print(plot)
}

# Close the PDF
dev.off()
