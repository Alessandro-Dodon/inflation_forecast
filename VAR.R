################################################################################
# Pre-Processing for VAR (X_VAR and Y_VAR to avoid conflicts)
################################################################################

# Prepare X_VAR and Y_VAR to avoid overwriting
X_VAR <- transformed_data_cleaned_no_COVID[, !(names(transformed_data_cleaned_no_COVID) %in% c("CPIULFSL"))]
Y_VAR <- transformed_data_cleaned_no_COVID[, c("date", "CPIULFSL")]

# NOTE:
# CPIULFSL is excluded from X_VAR to avoid double-counting when combining X_VAR 
# and Y_VAR later.

# Define train and test date ranges
train_start <- "1960-02-01"
train_end <- "2000-01-01"
test_start <- "2000-02-01"
test_end <- "2019-12-01"

# Ensure the date column is in Date format
transformed_data_cleaned_no_COVID$date <- as.Date(transformed_data_cleaned_no_COVID$date)

# Create X_VAR and Y_VAR train/test with the date column retained
X_train_with_date_VAR <- X_VAR[transformed_data_cleaned_no_COVID$date >= train_start & transformed_data_cleaned_no_COVID$date <= train_end, ]
X_test_with_date_VAR <- X_VAR[transformed_data_cleaned_no_COVID$date >= test_start & transformed_data_cleaned_no_COVID$date <= test_end, ]

Y_train_with_date_VAR <- Y_VAR[Y_VAR$date >= train_start & Y_VAR$date <= train_end, ]
Y_test_with_date_VAR <- Y_VAR[Y_VAR$date >= test_start & Y_VAR$date <= test_end, ]

# Debugging: Print date ranges and dimensions
cat("Training Data Date Range (X):\n")
print(range(X_train_with_date_VAR$date))

cat("\nTest Data Date Range (X):\n")
print(range(X_test_with_date_VAR$date))

cat("\nTraining Data Date Range (Y):\n")
print(range(Y_train_with_date_VAR$date))

cat("\nTest Data Date Range (Y):\n")
print(range(Y_test_with_date_VAR$date))

cat("\nTraining Data Dimensions (X):\n")
print(dim(X_train_with_date_VAR))
cat("Training Data Rows (Y):", length(Y_train_with_date_VAR$CPIULFSL), "\n")

cat("\nTest Data Dimensions (X):\n")
print(dim(X_test_with_date_VAR))
cat("Test Data Rows (Y):", length(Y_test_with_date_VAR$CPIULFSL), "\n")

# Remove the date column
X_train_VAR <- X_train_with_date_VAR[, !(names(X_train_with_date_VAR) %in% c("date"))]
X_test_VAR <- X_test_with_date_VAR[, !(names(X_test_with_date_VAR) %in% c("date"))]

# Ensure Y_train_VAR and Y_test_VAR are numeric vectors
Y_train_VAR <- as.numeric(Y_train_with_date_VAR[, "CPIULFSL"])
Y_test_VAR <- as.numeric(Y_test_with_date_VAR[, "CPIULFSL"])

################################################################################
# VAR Function with Dynamic PCA Configurations
################################################################################

perform_var_with_pca <- function(pc_values, output_pdf) {
  # Open a single PDF for all PCA configurations
  pdf(output_pdf, width = 10, height = 8)
  
  for (num_components in pc_values) {
    cat("Running VAR with", num_components, "Principal Components...\n")
    
    # Initialize variables for recursive predictions
    Y_train_vec <- as.numeric(Y_train_VAR)
    X_train_matrix <- as.matrix(X_train_VAR)
    Y_test_vec <- as.numeric(Y_test_VAR)
    X_test_matrix <- as.matrix(X_test_VAR)
    
    n_test <- length(Y_test_vec)
    predictions <- numeric(n_test)
    
    for (i in 1:n_test) {
      # Standardize the current training data
      mean_Y_train <- mean(Y_train_vec)
      sd_Y_train <- sd(Y_train_vec)
      Y_train_standardized <- (Y_train_vec - mean_Y_train) / sd_Y_train
      
      mean_X_train <- colMeans(X_train_matrix)
      sd_X_train <- apply(X_train_matrix, 2, sd)
      X_train_standardized <- scale(X_train_matrix, center = mean_X_train, scale = sd_X_train)
      
      # Apply PCA dynamically
      pca_model <- prcomp(X_train_standardized, center = FALSE, scale. = FALSE)
      X_train_pca <- pca_model$x[, 1:num_components]
      
      # Fit VAR model
      train_data_standardized <- data.frame(Y = Y_train_standardized, X_train_pca)
      var_model <- VAR(train_data_standardized, p = 1)
      
      # Predict using VAR
      var_forecast <- predict(var_model, n.ahead = 1)
      prediction_standardized <- var_forecast$fcst$Y[1, "fcst"]
      
      # De-standardize the prediction
      prediction <- prediction_standardized * sd_Y_train + mean_Y_train
      predictions[i] <- prediction
      
      # Update training data
      actual_value <- Y_test_vec[i]
      Y_train_vec <- c(Y_train_vec, actual_value)
      X_train_matrix <- rbind(X_train_matrix, X_test_matrix[i, ])
    }
    
    # Calculate MSE
    test_errors <- Y_test_vec - predictions
    test_mse <- mean(test_errors^2)
    cat("Number of Principal Components:", num_components, "MSE:", test_mse, "\n")
    
    # Plot Actual vs Predicted
    df_test <- data.frame(Date = Y_test_with_date_VAR$date, Actual = Y_test_vec, Predicted = predictions)
    time_series_plot <- ggplot(df_test, aes(x = Date)) +
      geom_line(aes(y = Actual, color = "Actual")) +
      geom_line(aes(y = Predicted, color = "Predicted")) +
      labs(title = paste("VAR with", num_components, "Principal Components"),
           x = "Date", y = "CPIULFSL", color = "Legend") +
      theme_minimal() +
      scale_color_manual(values = c("Actual" = "blue", "Predicted" = "red"))
    
    print(time_series_plot)
  }
  
  # Close the PDF after plotting all configurations
  dev.off()
}

# NOTE: 
# This script tests multiple PCA configurations, so it requires additional time 
# to compute forecasts for each configuration. 

################################################################################
# Run VAR with PCA Configurations
################################################################################

pc_values <- c(2, 3, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 60, 75)
perform_var_with_pca(pc_values = pc_values, output_pdf = "actual_vs_predicted_var_pca.pdf")

################################################################################
# VAR Function with All Variables
################################################################################

perform_var_with_all_vars <- function(output_pdf) {
  # Open a single PDF for plots
  pdf(output_pdf, width = 10, height = 8)
  
  # Initialize variables for recursive predictions
  Y_train_vec <- as.numeric(Y_train_VAR)
  X_train_matrix <- as.matrix(X_train_VAR)
  Y_test_vec <- as.numeric(Y_test_VAR)
  X_test_matrix <- as.matrix(X_test_VAR)
  
  n_test <- length(Y_test_vec)
  predictions <- numeric(n_test)
  
  for (i in 1:n_test) {
    # Standardize the current training data
    mean_Y_train <- mean(Y_train_vec)
    sd_Y_train <- sd(Y_train_vec)
    Y_train_standardized <- (Y_train_vec - mean_Y_train) / sd_Y_train
    
    mean_X_train <- colMeans(X_train_matrix)
    sd_X_train <- apply(X_train_matrix, 2, sd)
    X_train_standardized <- scale(X_train_matrix, center = mean_X_train, scale = sd_X_train)
    
    # Fit VAR model using all predictors
    train_data_standardized <- data.frame(Y = Y_train_standardized, X_train_standardized)
    var_model <- VAR(train_data_standardized, p = 1)
    
    # Predict using VAR
    var_forecast <- predict(var_model, n.ahead = 1)
    prediction_standardized <- var_forecast$fcst$Y[1, "fcst"]
    
    # De-standardize the prediction
    prediction <- prediction_standardized * sd_Y_train + mean_Y_train
    predictions[i] <- prediction
    
    # Update training data
    actual_value <- Y_test_vec[i]
    Y_train_vec <- c(Y_train_vec, actual_value)
    X_train_matrix <- rbind(X_train_matrix, X_test_matrix[i, ])
  }
  
  # Calculate MSE
  test_errors <- Y_test_vec - predictions
  test_mse <- mean(test_errors^2)
  cat("MSE for All Variables:", test_mse, "\n")
  
  # Plot Actual vs Predicted
  df_test <- data.frame(Date = Y_test_with_date_VAR$date, Actual = Y_test_vec, Predicted = predictions)
  time_series_plot <- ggplot(df_test, aes(x = Date)) +
    geom_line(aes(y = Actual, color = "Actual")) +
    geom_line(aes(y = Predicted, color = "Predicted")) +
    labs(title = "VAR with All Variables", x = "Date", y = "CPIULFSL", color = "Legend") +
    theme_minimal() +
    scale_color_manual(values = c("Actual" = "blue", "Predicted" = "red"))
  
  print(time_series_plot)
  
  # Close the PDF after plotting
  dev.off()
}

# NOTE: 
# Using all predictors in the VAR can lead to overfitting and significantly longer 
# computation times. Running this code chunk may take a very long time (more than 
# 30m or so, depending on your machine).

################################################################################
# Run VAR with All Variables
################################################################################

perform_var_with_all_vars(output_pdf = "actual_vs_predicted_var_all.pdf")
