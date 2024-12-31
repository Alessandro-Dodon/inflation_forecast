# Inflation Forecasting with ML

This updated university group project focuses on forecasting inflation using various time series techniques. The dataset (`FRED-MD`) is provided by the Federal Reserve Bank of St. Louis, a trusted authority in macroeconomic data.

> **Note:** The code is being updated to improve data processing precision and model implementation efficiency.

## Methodology

The analysis incorporates multiple forecasting models, balancing both traditional time series approaches and modern machine learning techniques. The key methods used include:

- **AR(1) Model**: A standard autoregressive model to capture inflation trends.
- **Lasso Regression**: A regularization technique to improve model accuracy by selecting key predictors.
- **Ridge Regression**: Similar to Lasso but handles multicollinearity by shrinking coefficients.
- **Principal Component Regression (PCR)**: Reduces dimensionality in the dataset and uses principal components for forecasting.
- **Vector Autoregression (VAR)**: A multivariate time series model to capture relationships between inflation and macroeconomic indicators.
- **Random Forest (RF)**: A machine learning method that captures complex, non-linear patterns for forecasting.

## Files

### `PreProcessing.R`
- R script for data preparation, addressing autocorrelation issues, plotting time series graphs, handling missing values, and other essential preprocessing steps.

### `AR1.R`
- Implements the AR(1) model, serving as the baseline for evaluating more complex models.

### `Lasso.R`
- Applies Lasso regression with regularization, tuned through experimentation and inspired by referenced research.

### `Ridge.R`
- Implements Ridge regression to address multicollinearity, with parameters guided by research insights.

### `PCR.R`
- Executes Principal Component Regression (PCR), experimenting with different numbers of principal components to optimize performance.

### `VAR.R`
- Implements Vector Autoregression (VAR) models, including PCA to address multicollinearity issues effectively.

### `RandomForest.R`
- Applies Random Forest models to capture non-linear relationships in the data.

### `Report.pdf`
- A detailed report explaining the results, visualizations, and comparisons of the models.

### `current.csv`
- The dataset used in the analysis, containing US monthly macroeconomic indicators from the Federal Reserve.

## User Guide

1. **Setup the Dataset**: Download the `current.csv` file and place it in the same directory as the scripts. The working directory is set to a relative path.
   
   You can either download the dataset from this repository or directly from the [Federal Reserve Economic Data (FRED) database](https://www.stlouisfed.org/research/economists/mccracken/fred-databases) (ensure you select the "monthly" dataset).
   
2. **Run the Scripts**: Start by executing the `PreProcessing.R` script, as all other scripts depend on the preprocessed data.

3. **Install Required Packages**: If any required packages are missing, uncomment the `install.packages()` lines in the script to install them.

4. **Check Output**: The scripts generate various plots, which are automatically saved in the same directory as the script.

Ensure that R is installed on your machine to run the scripts smoothly.
