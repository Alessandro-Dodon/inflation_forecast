# Inflation Forecasting with ML

This updated university group project focuses on forecasting inflation using various time series techniques. The dataset (FRED-MD) is provided by the Federal Reserve Bank of St. Louis, a trusted authority in macroeconomic data.

> **Note:** The code is being updated to be more precise and efficient in terms of data processing and model implementation.

## Methodology

The analysis covers multiple forecasting models, with a focus on both traditional and modern machine learning techniques. The key methods used include:

**AR(1) Model**: A standard autoregressive model to capture inflation trends.

**Lasso Regression**: A regularization technique to improve model accuracy by selecting key predictors.

**Ridge Regression**: Similar to Lasso but handles multicollinearity differently by shrinking coefficients.

**Principal Component Regression (PCR)**: Reducing dimensionality in the dataset and using principal components for forecasting.

**Vector Autoregression (VAR)**: A multivariate time series model to capture relationships between inflation and macroeconomic indicators.

**Random Forest (RF)**: A machine learning technique that captures complex, non-linear patterns for forecasting.

## Files

### `PreProcessing.R`
- R script for data preparation, addressing autocorrelation issues, plotting time series graphs, removing missing values, and other essential preprocessing steps.

### `AR1.R`
- Uses the AR(1) model, a baseline gold standard, as the benchmark for evaluating more complex models.

### `Lasso.R`
- Implements Lasso regression with regularization tuned through experimentation and inspired by referenced research.

### `Ridge.R`
- Applies Ridge regression to tackle multicollinearity, with parameters guided by insights from research papers.

### `PCR.R`
- Performs Principal Component Regression (PCR), experimenting with different numbers of PCs to optimize performance.

### `VAR.R`
- Implements Vector Autoregression (VAR) models, including an application of PCA to address multicollinearity issues effectively.

### `RandomForest.R`
- Explores the use of Random Forests to capture non-linear relationships in the data.

### `Report.pdf`
- A detailed report explaining the results, visualizations, and model comparisons.

### `current.csv`
- Dataset used in the analysis, consisting of US monthly macroeconomic indicators from the Federal Reserve.

## User Guide

Download the `current.csv` file and place it in the same directory as the scripts. The working directory is set to a relative path. Start by running the `PreProcessing.R` script, as all other scripts depend on the preprocessed data.
If you don't have the `current.csv` file, either download it from this repo or from the [Federal Reserve Economic Data (FRED) database](https://www.stlouisfed.org/research/economists/mccracken/fred-databases) (select the "monthly" dataset).
If any required packages are missing, uncomment the `install.packages()` lines in the script to install them. Ensure that R is installed, and all libraries are available before running the analysis.

## try2
Download the `current.csv` file and place it in the same directory as the scripts. The working directory is set to a relative path. Begin by running the `PreProcessing.R` script, as all other scripts depend on the preprocessed data. If you don’t have the `current.csv` file, you can either download it from this repository or directly from the [Federal Reserve Economic Data (FRED) database](https://www.stlouisfed.org/research/economists/mccracken/fred-databases) (choose the "monthly" dataset). 
If any required packages are missing, simply uncomment the `install.packages()` lines in the script to install them. Make sure that R is installed and all necessary libraries are available before running the analysis.
Additionally, please note that since the script generates numerous plots, they are typically saved in the same directory. The script automatically handles the saving process for you.

