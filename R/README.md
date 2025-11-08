# R/ Folder Organization

This document describes the organization of the R source code.

## File Structure

```
R/
├── guide_gb.R           # Main API - User entry point
├── fit.R                # Fitting algorithms
├── predict.guide_gb.R   # Prediction interface
├── predict_helpers.R    # Prediction utilities
├── metrics.R            # Error metrics
├── utils.R              # General utilities
├── dsc_utils.R          # DSC file operations
├── print.guide_gb.R     # Print S3 method
└── summary.guide_gb.R   # Summary S3 method
```

## Module Descriptions

### Core Modules

#### `guide_gb.R` (211 lines)
**Main API and parameter validation**
- `guide_gb()` - Main user-facing function
- Validates all input parameters
- Configures GUIDE files
- Coordinates fitting process
- Returns model object with class "guide_gb"

#### `fit.R` (302 lines)
**Model fitting algorithms**
- `fit_regression()` - Gradient boosting for regression
- `fit_binary_classifier()` - Gradient boosting for binary classification
- Implements boosting iterations
- Handles bagging and early stopping
- Integrates with GUIDE executable

#### `predict.guide_gb.R` (36 lines)
**Prediction interface**
- `predict.guide_gb()` - S3 predict method
- Handles missing value indicators
- Supports "link" and "response" types
- Delegates to prediction helpers

### Helper Modules

#### `predict_helpers.R` (204 lines)
**Prediction utilities**
- `make_prediction_tree_a()` - Tree prediction (type a)
- `make_prediction_tree_b()` - Tree prediction (type b)
- `make_prediction_tree_regressor()` - Regressor tree prediction
- `make_prediction_tree_classifier()` - Classifier tree prediction
- `get_iteration_gradients_regressor()` - Compute gradients for regression
- `get_iteration_gradients_classifier()` - Compute gradients for classification
- `make_regressor_prediction()` - Final regression predictions
- `make_classifier_prediction()` - Final classification predictions
- `map_logodds()` - Node to log-odds mapping
- `sense_check_calc()` - Diagnostic calculations

#### `metrics.R` (39 lines)
**Error metrics**
- `mse()` - Mean Squared Error
- `rmse()` - Root Mean Squared Error
- `loglik()` - Log-likelihood (probabilities)
- `loglik2()` - Log-likelihood (log-odds)

#### `utils.R` (53 lines)
**General utilities**
- `trim_file_at_marker()` - File processing utility
- `get_pred_func()` - Select prediction function by type
- `%||%` - Null-coalescing operator
- `type_map` - Complexity type mapping constant

#### `dsc_utils.R` (90 lines)
**DSC file operations**
- `create_dsc()` - Generate DSC from dataframe (exported)
- `dsc_clean()` - Clean DSC lines
- `dsc_add_weight()` - Add weight variable
- `dsc_get_variables()` - Extract variable info
- `count_missing_values()` - Track missing data

### S3 Methods

#### `print.guide_gb.R` (13 lines)
**Print method**
- `print.guide_gb()` - Display model summary

#### `summary.guide_gb.R` (34 lines)
**Summary method**
- `summary.guide_gb()` - Detailed model information

## Function Visibility

### Exported Functions (User-Facing)
- `guide_gb()` - Main model fitting function
- `predict.guide_gb()` - Prediction method
- `print.guide_gb()` - Print method
- `summary.guide_gb()` - Summary method
- `create_dsc()` - DSC file creation utility

### Internal Functions
All other functions are marked with `@keywords internal` and are not exported to users.

## Dependency Graph

```
User Code
    ↓
guide_gb() ────────┐
    ↓              │
    ├─→ utils.R (type_map)
    ├─→ dsc_utils.R (dsc operations)
    └─→ fit.R ─────┤
           ↓       │
           ├─→ metrics.R
           ├─→ utils.R
           └─→ predict_helpers.R

predict.guide_gb() ─┐
    ↓               │
    ├─→ utils.R     │
    └─→ predict_helpers.R
           ↓
           ├─→ metrics.R
           └─→ utils.R

print.guide_gb()
summary.guide_gb()
```

## Design Principles

1. **Separation of Concerns**: Each file has a single, well-defined purpose
2. **Single Responsibility**: Functions do one thing well
3. **Encapsulation**: Internal helpers are not exported
4. **Reusability**: Common utilities are centralized
5. **Maintainability**: Smaller files are easier to understand and modify
6. **Testability**: Isolated functions can be tested independently

## Usage Example

```r
# Load the package
library(myCustomModel)

# Prepare data
x <- data.frame(x1 = rnorm(100), x2 = rnorm(100))
y <- 2 + 3*x$x1 - x$x2 + rnorm(100)

# Fit model
model <- guide_gb(
  x = x, 
  y = y,
  guide_path = "path/to/guide",
  config_path = "path/to/config",
  type = "regression",
  complexity = "constant_exhaustive",
  iterations = 100,
  eta = 0.1
)

# Make predictions
predictions <- predict(model, newdata = x)

# View model
print(model)
summary(model)
```

## Adding New Features

When extending the code:

1. **New metrics**: Add to `metrics.R`
2. **New utilities**: Add to `utils.R`
3. **New fitting algorithms**: Add to `fit.R`
4. **New prediction methods**: Add to `predict_helpers.R`
5. **New API parameters**: Update `guide_gb.R`
6. **New DSC operations**: Add to `dsc_utils.R`

## Testing Recommendations

Organize tests to mirror the source structure:

```
tests/
├── test-guide_gb.R
├── test-fit.R
├── test-predict.R
├── test-metrics.R
├── test-utils.R
└── test-dsc_utils.R
```

## Performance Notes

- Prediction functions are optimized for vectorization
- Large files use chunked processing where appropriate
- Bagging uses pre-computed indices for consistency
- Early stopping reduces unnecessary iterations

## See Also

- [REFACTORING.md](../REFACTORING.md) - Detailed refactoring documentation
- [README.md](../README.md) - Main project documentation
