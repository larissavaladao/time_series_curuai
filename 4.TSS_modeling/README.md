# TSS Modeling - Empirical & Statistical Models

## Overview

This directory contains a comprehensive implementation and comparison of empirical and statistical models for predicting **Total Suspended Solids (TSS)** in the Curuai region. Multiple modeling approaches are explored and evaluated to identify the best-performing model.

## Notebooks

### 1. `1.TSS_reg_models.ipynb`
Implements regression-based TSS models, including polynomial regression and Ordinary Least Squares (OLS).

### 2. `2.TSS_ml_models.ipynb`
Implements machine learning-based TSS models, including Random Forest (RF) and Gradient Boosting Regressor (GBR).

### 3. `3.TSS_gee_models.ipynb`
Implements models using Google Earth Engine (GEE) data, including GBR (GEE) and RF (GEE).

### 4. `4.Model_selection.ipynb`
Performs model selection and comparison across all implemented models.

## Methodology

### Cross-Validation
All models were evaluated using **cross-validation** to ensure robust performance assessment and prevent overfitting. This approach provides reliable estimates of model generalization performance.

### Model Selection

#### `sort_models_count()` Function
The best model was selected using the **`sort_models_count()` function**, which ranks models based on a "Win Count" system that holistically evaluates both performance and stability:

- **Performance Wins**: Models earn points for achieving the best values across key metrics:
  - R² (higher is better)
  - MAE (lower is better)
  - RMSE (lower is better)
  - MAPE (lower is better)
  - Explained Variance (higher is better)

- **Stability Wins**: Models earn additional points for having the lowest standard deviation across validation folds for each metric. This ensures the selected model is not only accurate but also **consistent and reliable**.

- **Total Score**: The final ranking is based on the sum of performance and stability wins, with tiebreakers applied in the following order:
  1. Total wins (descending)
  2. R² score (descending)
  3. MAE (ascending)

This approach prioritizes models that consistently perform well across multiple metrics and validation folds.

## Selected Model

### **GBR (GEE)** - Gradient Boosting Regressor with Google Earth Engine Data

The **GBR (GEE)** model was selected as the optimal model for TSS prediction. This model combines:
- **Gradient Boosting Regressor**: A powerful ensemble machine learning technique
- **Google Earth Engine Data**: High-quality satellite imagery and derived spectral indices

The selection reflects the model's superior performance in the cross-validation framework, demonstrating both high predictive accuracy and stability across validation folds.

## Key Metrics

The models are evaluated using the following metrics:
- **R² Score**: Coefficient of determination
- **Explained Variance**: Proportion of variance explained by the model
- **MAE**: Mean Absolute Error
- **MAPE**: Mean Absolute Percentage Error
- **RMSE**: Root Mean Squared Error
- **MSE**: Mean Squared Error

Each metric includes standard deviation estimates from cross-validation to assess model stability.

## Output

- `model_selection.csv`: Summary of the top 20 models with their performance metrics
- `metrics_comparison.jpg`: Visualization of model performance across all metrics with error bars
