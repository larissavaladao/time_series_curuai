# TSS Modeling Best Practices & Configuration Guide

## Configuration & Optimization Tips

### 1. Model Configuration

Define models in a reusable way:

```python
from sklearn.linear_model import LinearRegression, Ridge
from sklearn.preprocessing import PolynomialFeatures, SplineTransformer
from sklearn.pipeline import Pipeline

# Central model definitions
MODEL_CONFIG = {
    'ols': {
        'model': LinearRegression(),
        'description': 'Ordinary Least Squares'
    },
    'polynomial': {
        'model': Pipeline([
            ('poly', PolynomialFeatures(degree=2, include_bias=False)),
            ('linear', LinearRegression(positive=False))
        ]),
        'description': '2nd Degree Polynomial'
    },
    'splines': {
        'model': Pipeline([
            ('spline', SplineTransformer(n_knots=5, degree=3)),
            ('linear', LinearRegression(positive=False))
        ]),
        'description': 'Spline Transformation'
    }
}

def get_model(model_name):
    """Get model by name."""
    return MODEL_CONFIG[model_name]['model']

# Usage
model_poly = get_model('polynomial')
model_splines = get_model('splines')
```

### 2. Feature Engineering

```python
# Define feature groups
FEATURE_GROUPS = {
    'single_band': {
        'nir': ['nir_mean'],
        'red': ['red_mean'],
        'green': ['green_mean'],
        'blue': ['blue_mean']
    },
    'multi_band': {
        'nir_red': ['nir_mean', 'red_mean'],
        'all_bands': ['blue_mean', 'green_mean', 'red_mean', 'nir_mean'],
        'spectral_indices': ['ndvi', 'gndvi']  # if available
    }
}

# Usage
X_nir_red = df_subset[FEATURE_GROUPS['multi_band']['nir_red']]
```

---

## Workflow Best Practices

### ✅ Recommended Analysis Flow

```python
# 1. Data Preparation
df_clean = load_and_prepare_data()
y = df_clean['TSS'].copy()

# 2. Feature Exploration
for features in FEATURE_GROUPS['single_band'].values():
    X = df_clean[features]
    model_fit, y_pred, metrics, cv_metrics = train_and_evaluate_model(
        model=ols,
        X=X, y=y,
        model_name=f"OLS - {features[0]}"
    )