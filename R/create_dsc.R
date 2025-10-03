# Automatically creates GUIDE DSC files from a dataframe
# defaults to standard out if output is "", or else outputs to the filepath
# standardises the output file:
#   input file = data.csv
#   missing values = NA
#   data would start at row 2
#   begin constructing DSC file by guessing data types

create_dsc <- function(df, output="", input_file='data.csv',
                        missing_values='NA', data_start=2) {
    dsc_string <- paste(input_file, missing_values, data_start, sep="\n")
    cols <- c()
    for (i in 1:ncol(df)) {
        class_ <- class(df[,i])
        if (class_ == 'numeric') id = 'n'
        if (class_ == 'factor') id = 'c'
        cols[i] <- paste(i, colnames(df)[i], id)
    }
    dsc_string <- paste(dsc_string, "\n", paste(cols, collapse="\n"), "\n", sep="")
    cat(dsc_string, file=output)
    return (dsc_string)
}


# dsc = create_dsc(df, output='/Users/jkhong/Desktop/guide-gb/guide_run/data.DSC')
# create_dsc(Boston, output='/Users/jkhong/Desktop/guide-gb/guide_run/data.DSC')
