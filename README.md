# Inflation Forecasting with ML

This updated university group project focuses on forecasting inflation using various time series techniques. The dataset (**FRED-MD**) is provided by the Federal Reserve Bank of St. Louis, a trusted authority in macroeconomic data.

## Methodology

We combined traditional econometric models and machine learning techniques to forecast inflation. Key models include AR(1), Lasso, Ridge, Principal Component Regression (PCR), Vector Autoregression (VAR), and Random Forest (RF).

## Files

### **Scripts**
- **`PreProcessing.R`**: Prepares data by handling missing values, autocorrelation, stationarity checks, and visualization.
- **`AR1.R`**: Implements AR(1) as the benchmark model.
- **`Lasso.R`**: Uses Lasso regression with regularization.
- **`Ridge.R`**: Applies Ridge regression for multicollinearity.
- **`PCR.R`**: Performs Principal Component Regression with varying numbers of PCs.
- **`VAR.R`**: Fits Vector Autoregression models with PCA for multicollinearity.
- **`RF.R`**: Explores Random Forest for non-linear patterns.

### **Documentation**
- **`Presentation.pdf`**: A detailed report with methodology, results, and comparisons.

### **Dataset**
- **`current.csv`**: Contains US monthly macroeconomic indicators from the Federal Reserve.

---

## Results

Below is an example output of the Lasso Regression model results:

![Lasso Results](LassoResults.png)  
*Visualization of Lasso model coefficients and performance metrics.*

---

## User Guide

1. **Setup**:
   - Place the `current.csv` file in the same directory as the scripts.
   - Ensure the working directory is set to the project folder.

2. **Execution**:
   - Run the `PreProcessing.R` script first, as all other scripts depend on the preprocessed data.
   - Generated plots and metrics will be saved automatically in the same directory.

3. **Packages**:
   - If any R packages are missing, uncomment the `install.packages()` lines in each script to install them.

4. **Dataset**:
   - If the `current.csv` file is unavailable, download the monthly dataset from the [FRED-MD website](https://www.stlouisfed.org/research/economists/mccracken/fred-databases).

---

### Notes

For any questions or contributions, feel free to open an issue in this repository. This project highlights our integration of econometrics and machine learning for inflation forecasting.
