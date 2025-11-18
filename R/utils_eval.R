COMPLEXITIES <- c("constant_exhaustive", "poly1", "poly2", "stepwise")

GUIDE_GB_PARAMS_REGRESSION <- list(
    eta = c(0.01, 0.3),
    max_split_levels = c(3L, 10L),
    min_node_size = c(2L, 20L),
    bag_fraction = c(0.3, 1.0),
    complexity = c(1L, 4L)
)

GUIDE_GB_PARAMS_BINARY_CLASSIFICATION <- list(
    eta = c(0.01, 0.3),
    max_split_levels = c(3L, 10L),
    min_node_size = c(2L, 20L),
    bag_fraction = c(0.3, 1.0)
)

# XGBoost mapping:
# max_split_levels -> max_depth
# min_node_size -> min_child_weight
# bag_fraction -> subsample
XGBOOST_PARAMS_REGRESSION <- list(
    eta = c(0.01, 0.3),
    max_split_levels = c(3L, 10L),
    min_node_size = c(2L, 20L),
    bag_fraction = c(0.3, 1.0),
    gamma = c(0, 10),
    lambda = c(1e-3, 10)
)

XGBOOST_PARAMS_BINARY_CLASSIFICATION <- list(
    eta = c(0.01, 0.3),
    max_split_levels = c(3L, 10L),
    min_node_size = c(2L, 20L),
    bag_fraction = c(0.3, 1.0),
    gamma = c(0, 10),
    lambda = c(1e-3, 10)
)

# gbm (R):
# max_split_levels -> interaction.depth
# min_node_size -> n.minobsinnode
# eta -> shrinkage
GBM_PARAMS_REGRESSION <- list(
    eta = c(0.01, 0.3),
    max_split_levels = c(3L, 10L),
    min_node_size = c(2L, 20L),
    bag_fraction = c(0.3, 1.0)
)

GBM_PARAMS_BINARY_CLASSIFICATION <- list(
    eta = c(0.01, 0.3),
    max_split_levels = c(3L, 10L),
    min_node_size = c(2L, 20L),
    bag_fraction = c(0.3, 1.0)
)

# CatBoost:
# max_split_levels -> depth
# min_node_size -> min_data_in_leaf
CATBOOST_PARAMS_REGRESSION <- list(
    eta = c(0.01, 0.3),
    max_split_levels = c(3L, 10L),
    min_node_size = c(2L, 20L),
    bagging_temperature = c(0.0, 1.5),
    rsm = c(0.5, 1.0),
    l2_leaf_reg = c(1.0, 15.0)
)

CATBOOST_PARAMS_BINARY_CLASSIFICATION <- list(
    eta = c(0.01, 0.3),
    max_split_levels = c(3L, 10L),
    min_node_size = c(2L, 20L),
    bagging_temperature = c(0.0, 1.5),
    rsm = c(0.5, 1.0),
    l2_leaf_reg = c(1.0, 15.0)
)
