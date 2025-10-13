# guide-gb
Gradient Boosting with GUIDE

# Version
- Current version of GUIDE (Version 45.1) used for implementation is macOS Sequoia 15.4.1 with Apple Arm processors (compiled with NAG Fortran 7.2).
- Credited to Wei Yin Loh (https://pages.stat.wisc.edu/~loh/guide.html)

# Utility functions available
1. `create_dsc.R` - automatically reads a dataframe and converts it into a DSC, to specify the output path using the `output` parameter, if empty, DSC is printed in stdout.

# Instructions
1. Current version works with Version 45.1 of GUIDE
2. Create a `guide_run` folder, within would contain:
    - the `guide` executable


# Fitting the model



# Limitations of GUIDE
- File I/O - very slow in fitting subsequent trees, especially for larger datasets
- Current implementation stores the trees in the form of functions, for regression, initial testing was performed using terminal nodes with linear predictions (regression in terminal nodes) instead of constant, while classification used constant prediction. As the `.R` code output provides a vector as the result, the length of output would be dynamic. The type of output cannot be known dynamically as the `.R` code output does not specify name of the prediction output, i.e. node or prediction amount. Currently, two functions are used to make predictions, `make_prediction_tree_regressor` and `make_prediction_tree_regressor`, where they can actually be combined as one.
- As I am not making modifications to the GUIDE algorithm, there is a tendency to treat the GUIDE executable as a 'black-box'. Hence I may only be limited to improvements / optimisations surrounding  ensemble construction, rather than individual tree formation.

# Notes
## Classification tree map
1. A tree map is used to store the predLogOdds of each node, in R where a `list` is used. Assigning a numerical key to the list would inadvertently create list references of prior unreferenced keys, e.g. initialising the list with key 5, would produce keys 1 to 4 with `NULL`. To prevent this, each node (the key) is converted to a character.
2. Subsequently, the predict function would require conversion of nodeId integers to character in order to reference the predLogOdds stored in the tree map for each weak learner.


# Improvements
1. Investigate improvements over data structure for treemap and `map_logodds` function.
2. Include parameter in classification prediction to output as label or probability.
3. Consider adding multiclass classification
4. Adding subset training

# Experimentation
1. Experiment with GUIDE parameters:
    ```
    Choose type of regression model:
    1=linear, 2=quantile, 3=Poisson, 4=censored response,
    5=multiresponse or itemresponse, 6=longitudinal data (with T variables),
    7=binary logistic regression.
    ```

