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

### 3. Cross-Validation Settings

```python
# CV Configuration
CV_CONFIG = {
    'n_splits': 20,
    'test_size': 0.20,
    'random_state': 666
}

# Use in helper function
cv_metrics = cv_model_metrics(model, X, y, n_cv=CV_CONFIG['n_splits'])
```

### 4. Visualization Configuration

```python
# Plot styling
PLOT_CONFIG = {
    'figsize': (16, 6),
    'colors': {
        'prediction': 'gray',
        'palette': ['#1f77b4', '#ff7f0e', '#2ca02c', '#d62728', '#9467bd', '#8c564b', '#e377c2', '#7f7f7f']
    },
    'marker_size': 6,
    'alpha': 0.5,
    'linewidth': 2,
    'font_size': 10
}
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


# 3. Results Compilation
compile_results_summary(results)
```

## Results Compilation & Comparison

### Create Comparison DataFrame

```python
def compile_comparison_table(results_dict):
    """Convert results dictionary to comparison DataFrame."""
    import pandas as pd
    
    comparison_data = []
    for name, results in results_dict.items():
        row = {
            'Model': name,
            **results['cv_metrics']
        }
        comparison_data.append(row)
    
    return pd.DataFrame(comparison_data).sort_values('r2', ascending=False)

# Usage
comparison_df = compile_comparison_table(comparison_results)
print(comparison_df)
```

### Export Results to CSV

```python
def export_results(results_dict, output_path):
    """Export model results to CSV."""
    import pandas as pd
    
    for model_name, data in results_dict.items():
        metrics_df = pd.DataFrame([data['cv_metrics']])
        metrics_df.to_csv(f"{output_path}/{model_name}_metrics.csv")
        
        # Export predictions if available
        if 'predictions' in data:
            pred_df = pd.DataFrame({
                'actual': data['y'],
                'predicted': data['predictions']
            })
            pred_df.to_csv(f"{output_path}/{model_name}_predictions.csv")
```

---

## Debugging & Troubleshooting

### Issue 1: Model Not Converging
```python
# Add diagnostic information
def diagnose_model(model, X, y):
    """Print diagnostic information about model fit."""
    print(f"X shape: {X.shape}")
    print(f"y shape: {y.shape}")
    print(f"Missing values in X: {X.isnull().sum()}")
    print(f"Missing values in y: {y.isnull().sum()}")
    print(f"X data types:\n{X.dtypes}")
    print(f"y statistics:\n{y.describe()}")
    
    # Try to fit and catch errors
    try:
        model.fit(X, y)
        print("✓ Model fit successful")
    except Exception as e:
        print(f"✗ Model fit failed: {e}")

diagnose_model(model_poly, X, y)
```

### Issue 2: Poor Predictions
```python
# Analyze residuals
def analyze_residuals(y_true, y_pred):
    """Detailed residual analysis."""
    residuals = y_true - y_pred
    
    print(f"Residuals mean: {residuals.mean():.4f}")
    print(f"Residuals std: {residuals.std():.4f}")
    print(f"Residuals min/max: {residuals.min():.4f} / {residuals.max():.4f}")
    
    # Check for patterns
    import matplotlib.pyplot as plt
    plt.figure(figsize=(12, 4))
    
    plt.subplot(1, 2, 1)
    plt.scatter(y_pred, residuals, alpha=0.5)
    plt.axhline(y=0, color='r', linestyle='--')
    plt.xlabel('Predicted')
    plt.ylabel('Residuals')
    
    plt.subplot(1, 2, 2)
    plt.hist(residuals, bins=30)
    plt.xlabel('Residuals')
    plt.ylabel('Frequency')
    
    plt.tight_layout()
    plt.show()
```

---

## Code Quality Checklist

- [ ] All imports at the top
- [ ] Configuration centralized (MODEL_CONFIG, FEATURE_GROUPS, etc.)
- [ ] Consistent variable naming
- [ ] All plots use `plot_evaluation()` or `prepare_time_series_plot()`
- [ ] All model evaluations use `train_and_evaluate_model()`
- [ ] All batch operations use `batch_model_evaluation()`
- [ ] Results stored consistently in dictionaries/DataFrames
- [ ] Comments explain complex logic
- [ ] No hardcoded values (use configuration)
- [ ] Error handling for edge cases

---

## Advanced: Creating a Custom Analysis Class

For even more organization, consider a class-based approach:

```python
class TSSSModelingPipeline:
    """Complete TSS modeling pipeline."""
    
    def __init__(self, df, config=None):
        self.df = df
        self.config = config or self._default_config()
        self.models = {}
        self.results = {}
    
    def _default_config(self):
        return {
            'cv_splits': 20,
            'test_size': 0.20,
            'random_state': 666
        }
    
    def add_model(self, name, model):
        """Register a model."""
        self.models[name] = model
        return self
    
    def train_single_model(self, model_name, features, target='TSS'):
        """Train single model."""
        X = self.df[features]
        y = self.df[target]
        
        model_fit, y_pred, metrics, cv_metrics = train_and_evaluate_model(
            self.models[model_name], X, y, model_name
        )
        
        self.results[model_name] = {
            'model': model_fit,
            'predictions': y_pred,
            'metrics': metrics,
            'cv_metrics': cv_metrics
        }
        return self
    
    def train_all_models(self, features, target='TSS'):
        """Train all registered models."""
        for model_name in self.models:
            self.train_single_model(model_name, features, target)
        return self
    
    def get_comparison(self):
        """Get results comparison."""
        import pandas as pd
        return pd.DataFrame({
            name: data['cv_metrics'] 
            for name, data in self.results.items()
        }).T

# Usage
pipeline = TSSSModelingPipeline(df_subset)
pipeline.add_model('OLS', ols).add_model('Poly', model_poly).add_model('Splines', model_splines)
pipeline.train_all_models(['blue_mean', 'green_mean', 'red_mean', 'nir_mean'])
print(pipeline.get_comparison())
```

---

## Resources

- [Scikit-learn Documentation](https://scikit-learn.org/)
- [Pandas API Reference](https://pandas.pydata.org/docs/)
- [Matplotlib Gallery](https://matplotlib.org/gallery/index.html)
- [Cross-validation Guide](https://scikit-learn.org/stable/modules/cross_validation.html)

---

**Last Updated**: January 7, 2026
