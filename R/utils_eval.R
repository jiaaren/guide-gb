COMPLEXITIES <- c("constant_exhaustive", "poly1", "poly2", "stepwise")

GUIDE_GB_PARAMS_REGRESSION <- list(
    eta = c(0.01, 1),
    max_split_levels = c(2L, 10L),
    min_node_size = c(2L, 25L),
    bag_fraction = c(0.5, 1.0),
    complexity = c(1L, 4L)
)

GUIDE_GB_PARAMS_BINARY_CLASSIFICATION <- list(
    eta = c(0.01, 1),
    max_split_levels = c(2L, 10L),
    min_node_size = c(2L, 25L),
    bag_fraction = c(0.5, 1.0)
)

XGBOOST_PARAMS_REGRESSION <- list(
    eta = c(0.01, 0.3),
    max_split_levels = c(3L, 10L),
    min_node_size = c(2L, 20L),
    bag_fraction = c(0.5, 1.0),
    gamma = c(0, 10),
    lambda = c(1e-3, 10)
)