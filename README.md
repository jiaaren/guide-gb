# GUIDE-GB

Gradient Boosting implementation using GUIDE (Generalized, Unbiased, Interaction Detection and Estimation) decision trees as weak learners.

## About

GUIDE-GB is an R package that implements gradient boosting for regression and binary classification tasks using the GUIDE algorithm. GUIDE trees offer unbiased variable selection and can detect interactions between predictors.

**Credit**: Wei-Yin Loh (https://pages.stat.wisc.edu/~loh/guide.html)

## Requirements

- GUIDE executable (Version 45.2)
  - macOS Sequoia 15.4.1 with Apple Arm processors (compiled with NAG Fortran 7.2)
  - Linux systems (see [GUIDE website](https://pages.stat.wisc.edu/~loh/guide.html) for Linux binaries)
- **Platform Support**: This implementation is currently only supported on macOS and Linux environments. Windows is not supported at this time.

## Installation

```r
# installation
devtools::install_github("jiaaren/guide-gb")

# load the package
library(guidegb)
```

**Note**: Before installation, ensure you have the GUIDE executable properly configured (see Requirements and Setup sections).

## Setup

1. **GUIDE Executable**: Obtain the GUIDE binary from [GUIDE website](https://pages.stat.wisc.edu/~loh/guide.html)
2. **Configuration Files**: Create a folder with GUIDE configuration files (`.in` files) and a `.DSC` data description file

### Configuration Structure

Your configuration folder should contain:
- `data_constant_exhaustive.in` - for constant terminal nodes with exhaustive search
- `data_constant_quantiles.in` - for constant terminal nodes with quantile splits
- `data_poly1.in` / `data_poly2.in` - for polynomial terminal nodes
- `data_stepwise.in` - for stepwise regression in terminal nodes
- `data.DSC` - data description file

### Creating DSC Files

Use the `create_dsc()` utility function to automatically generate DSC files from dataframes:

```r
create_dsc(df, output = "path/to/data.DSC", 
           input_file = "data.csv",
           missing_values = "NA", 
           data_start = 2)
```

## Usage

### Regression Example

```r
library(MASS)

# prep data
x <- Boston[, -14]
y <- Boston$medv

# set paths
guide_path <- '/path/to/guide'
run_folder <- '/path/to/temp/folder'
config_path <- '/path/to/config/folder'

# fit model
model <- guide_gb(
  x = x, 
  y = y,
  guide_path = guide_path,
  config_path = config_path,
  run_folder = run_folder,
  type = "regression",
  complexity = "stepwise",
  eta = 0.05,
  max_split_levels = 4,
  min_node_size = 2,
  iterations = 250
)

# make predictions
pred <- predict(model, newdata = x)

# calc RMSE
rmse(y, pred)
```

### Binary Classification Example

```r
data("BreastCancer", package = "mlbench")

# prep data
x <- BreastCancer[, 2:10]
x <- data.frame(lapply(x, function(col) as.numeric(as.character(col))))
y <- as.integer(BreastCancer$Class == 'malignant')

# fit model
model <- guide_gb(
  x = x,
  y = y,
  guide_path = guide_path,
  config_path = config_path,
  run_folder = run_folder,
  type = "binary_classification",
  complexity = "constant_exhaustive",
  eta = 0.05,
  iterations = 250
)

# predict probabilities
pred_prob <- predict(model, newdata = x, type = "response")
# predict log odds
pred_logodds <- predict(model, newdata = x, type = "link")
```

### Additional Features

#### Bagging
Bagging is disabled by default, i.e. `bag_fraction = 1`. 

```r
model <- guide_gb(
  x = x, y = y,
  guide_path = guide_path,
  config_path = config_path,
  run_folder = run_folder,
  type = "regression",
  complexity = "poly1",
  bag_fraction = 0.8,  # Use 80% of data per iteration
  bag_seed = 123
)
```

#### Early Stopping with Validation Set
The current implementation does not automatically partition training data into validation sets. Validation sets require manual splitting as of current version.

```r
# Split data
train_idx <- sample(1:nrow(x), 0.8 * nrow(x))
train_x <- x[train_idx, ]
train_y <- y[train_idx]
val_x <- x[-train_idx, ]
val_y <- y[-train_idx]

# Fit with early stopping
model <- guide_gb(
  x = train_x,
  y = train_y,
  val_x = val_x,
  val_y = val_y,
  guide_path = guide_path,
  config_path = config_path,
  run_folder = run_folder,
  type = "regression",
  complexity = "stepwise",
  early_stop_rounds = 10,
  iterations = 1000
)
```

#### Prediction with Different Numbers of Trees
GUIDE-GB can be trained with a larger iteration, $M$, with subsequent predictions made with $m < M$.
```r
# predict using first 50 trees - here predictions at iteration 50 is returned
pred_50 <- predict(model, newdata = x, n_trees = 50)

# predict using range of tree counts - here predictions from iteration 1 to 50 is returned
pred_multiple <- predict(model, newdata = x, n_trees = 1:50)
```

## Main Function: `guide_gb()`

### Parameters

| Parameter | Type | Default | Description |
|-----------|------|---------|-------------|
| `x` | data.frame | - | Predictor variables |
| `y` | vector | - | Target variable (numeric for regression, 0/1 for classification) |
| `guide_path` | character | - | Path to GUIDE executable |
| `config_path` | character | - | Path to folder containing `.in` and `.DSC` files |
| `run_folder` | character | `tempdir()` | Temporary folder for GUIDE operations. If `run_folder` is not specified, a temporary directory will be created in place. |
| `type` | character | - | Either `"regression"` or `"binary_classification"` |
| `complexity` | character | - | One of: `"constant_exhaustive"`, `"constant_quantiles"`, `"poly1"`, `"poly2"`, `"stepwise"` |
| `eta` | numeric | 0.1 | Learning rate (shrinkage parameter) |
| `max_split_levels` | integer | 4 | Maximum tree depth |
| `min_node_size` | integer | 4 | Minimum samples in terminal nodes |
| `iterations` | integer | 100 | Number of boosting iterations |
| `bag_fraction` | numeric | 1.0 | Fraction of data to sample per iteration (0, 1] |
| `bag_seed` | integer | NULL | Random seed for bagging |
| `val_x` | data.frame | NULL | Validation predictors |
| `val_y` | vector | NULL | Validation target |
| `early_stop_rounds` | integer | NULL | Stop if validation error doesn't improve for N rounds |
| `fit_pred_exact` | logical | TRUE | Use exact predictions from parsed functions. |

### Returns

An object of class `"guide_gb"` with methods:
- `predict(object, newdata, n_trees = NULL, type = c("link", "response"))`
- `print(object)`
- `summary(object)`

## Complexity Options

| Option | Description | Terminal Node Prediction |
|--------|-------------|-------------------------|
| `constant_exhaustive` | Exhaustive search for splits, constant predictions | Mean of residuals |
| `constant_quantiles` | Quantile-based splits, constant predictions | Mean of residuals |
| `poly1` | Linear predictions in terminal nodes | Linear model |
| `poly2` | Quadratic predictions in terminal nodes | Quadratic model |
| `stepwise` | Stepwise regression in terminal nodes | Stepwise linear model |

## Utility Functions

### DSC File Generation
- `create_dsc()` - Automatically generate GUIDE DSC files from dataframes

### Metrics
- `mse()` - Mean squared error
- `rmse()` - Root mean squared error
- `loglik()` - Log-likelihood for classification

## Implementation Details

### Classification
- Uses log-odds for predictions
- Tree maps store predicted log-odds for each terminal node
- Supports `type = "link"` (log-odds) or `type = "response"` (probabilities) in prediction

### Regression
- Supports multiple terminal node models: constant, linear (poly1), quadratic (poly2), stepwise
- Trees stored as R functions generated by GUIDE
- Automatic handling of missing values with indicator variables (for stepwise complexity)

### Performance
- Progress printed every 10 iterations
- Validation error computed at each iteration when watchlist provided
- Early stopping based on validation error

## License

See LICENSE file.

## Version History

- Current: Uses GUIDE Version 45.2 (macOS ARM, NAG Fortran 7.2)

