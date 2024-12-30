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
# RF Function
################################################################################

perform_rf <- function(ntree, maxnodes = NULL, use_all_predictors = TRUE, title, filename, output_folder) {
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
    # Prepare predictors
    if (use_all_predictors) {
      X_train_rf <- as.data.frame(X_train_matrix)
      new_X <- as.data.frame(matrix(X_test_matrix[i, ], nrow = 1))
    } else {
      X_train_rf <- as.data.frame(X_train_matrix[, "CPIULFSL", drop = FALSE])
      new_X <- as.data.frame(matrix(X_test_matrix[i, "CPIULFSL", drop = FALSE], nrow = 1))
    }
    
    colnames(new_X) <- colnames(X_train_rf)
    
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
# Configurations
################################################################################

# Test all predictors
perform_rf(ntree = 10, maxnodes = 5, use_all_predictors = TRUE, 
           title = "RF with All Predictors (10 Trees, Max Depth 5)", 
           filename = "rf_all_predictors_10_trees_5_depth.pdf", 
           output_folder = "rf_results")

perform_rf(ntree = 50, maxnodes = 10, use_all_predictors = TRUE, 
           title = "RF with All Predictors (50 Trees, Max Depth 10)", 
           filename = "rf_all_predictors_50_trees_10_depth.pdf", 
           output_folder = "rf_results")

perform_rf(ntree = 100, maxnodes = 15, use_all_predictors = TRUE, 
           title = "RF with All Predictors (100 Trees, Max Depth 15)", 
           filename = "rf_all_predictors_100_trees_15_depth.pdf", 
           output_folder = "rf_results")

perform_rf(ntree = 200, maxnodes = 20, use_all_predictors = TRUE, 
           title = "RF with All Predictors (200 Trees, Max Depth 20)", 
           filename = "rf_all_predictors_200_trees_20_depth.pdf", 
           output_folder = "rf_results")

perform_rf(ntree = 500, maxnodes = 25, use_all_predictors = TRUE, 
           title = "RF with All Predictors (500 Trees, Max Depth 25)", 
           filename = "rf_all_predictors_500_trees_25_depth.pdf", 
           output_folder = "rf_results")

# Test only inflation
perform_rf(ntree = 10, maxnodes = 5, use_all_predictors = FALSE, 
           title = "RF with Past CPIULFSL Only (10 Trees, Max Depth 5)", 
           filename = "rf_past_value_10_trees_5_depth.pdf", 
           output_folder = "rf_results")

perform_rf(ntree = 50, maxnodes = 10, use_all_predictors = FALSE, 
           title = "RF with Past CPIULFSL Only (50 Trees, Max Depth 10)", 
           filename = "rf_past_value_50_trees_10_depth.pdf", 
           output_folder = "rf_results")

perform_rf(ntree = 100, maxnodes = 15, use_all_predictors = FALSE, 
           title = "RF with Past CPIULFSL Only (100 Trees, Max Depth 15)", 
           filename = "rf_past_value_100_trees_15_depth.pdf", 
           output_folder = "rf_results")

perform_rf(ntree = 200, maxnodes = 20, use_all_predictors = FALSE, 
           title = "RF with Past CPIULFSL Only (200 Trees, Max Depth 20)", 
           filename = "rf_past_value_200_trees_20_depth.pdf", 
           output_folder = "rf_results")

perform_rf(ntree = 500, maxnodes = 25, use_all_predictors = FALSE, 
           title = "RF with Past CPIULFSL Only (500 Trees, Max Depth 25)", 
           filename = "rf_past_value_500_trees_25_depth.pdf", 
           output_folder = "rf_results")
