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
- File I/O - very slow in fitting subsequent trees
