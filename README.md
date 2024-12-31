# Inflation Forecasting with ML

This updated university group project focuses on forecasting inflation using various time series techniques. The dataset (FRED-MD) is provided by the Federal Reserve Bank of St. Louis, a trusted authority in macroeconomic data.

> **Note:** The code is being updated to be more precise and efficient in terms of data processing and model implementation.

## Methodology

The project applies several models to forecast inflation, utilizing both traditional time series models and modern machine learning techniques. The key methods used include:

- **AR(1) Model**: A standard autoregressive model to capture inflation trends.
- **Lasso Regression**: A regularization technique to improve model accuracy by selecting key predictors.
- **Ridge Regression**: Similar to Lasso but handles multicollinearity differently by shrinking coefficients.
- **Principal Component Regression (PCR)**: Reduces dimensionality and uses principal components for forecasting.
- **Vector Autoregression (VAR)**: A multivariate time series model to capture relationships between inflation and macroeconomic indicators.
- **Random Forest (RF)**: A machine learning technique that captures complex, non-linear patterns for forecasting.

## Files Overview

### `PreProcessing.R`
- R script for data preparation, addressing autocorrelation issues, plotting time series graphs, removing missing values, and performing other essential preprocessing steps.

### `AR1.R`
- Implements the AR(1) model as a baseline to evaluate the performance of more complex models.

### `Lasso.R`
- Implements Lasso regression with regularization, tuned through experimentation and inspired by referenced research.

### `Ridge.R`
- Applies Ridge regression to tackle multicollinearity, with parameters guided by insights from research papers.

### `PCR.R`
- Implements Principal Component Regression (PCR), experimenting with different numbers of principal components to optimize performance.

### `VAR.R`
- Implements Vector Autoregression (VAR) models, with PCA applied to address multicollinearity effectively.

### `RandomForest.R`
- Explores the use of Random Forests to model non-linear relationships in the data.

### `Report.pdf`
- A detailed report explaining the results, visualizations, and model comparisons.

### `current.csv`
- Dataset used in the analysis, consisting of US monthly macroeconomic indicators from the Federal Reserve.

## User Guide

1. **Prepare your environment**: Ensure R is installed on your machine. If any required packages are missing, simply uncomment the `install.packages()` lines in the scripts to install them.

2. **Setup dataset**: Download the `current.csv` file and place it in the same directory as the scripts. Alternatively, you can download it from the [Federal Reserve Economic Data (FRED) database](https://www.stlouisfed.org/research/economists/mccracken/fred-databases) by choosing the "monthly" dataset.

3. **Run the scripts**: Start by running the `PreProcessing.R` script, as all other scripts depend on the preprocessed data.

4. **Review output**: The script generates numerous plots, which are automatically saved in the same directory.

This updated structure offers clear, concise instructions and maintains a professional and approachable tone.
